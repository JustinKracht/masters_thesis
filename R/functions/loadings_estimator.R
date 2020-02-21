# loadings_estimator(): wrapper for psych::fa that extracts the estimated factor
# loading matrices from a correlation matrix using all of the specified methods.
loadings_estimator <- function(rsm_list,
                               sample_data,
                               pop_loadings,
                               sample_size,
                               factors,
                               method = c("fapa", "fals", "faml"),
                               rotate) {
  
  # Create an empty list to store output
  out_list <- vector(mode = "list", length = length(rsm_list))
  names(out_list) <- names(rsm_list)
  
  for (i in 1:length(rsm_list)) {
    if (!anyNA(rsm_list[[i]]$R)) {
      r <- rsm_list[[i]]$R
    } else {
      # If the tetrachoric matrix is not NPD, the smoothed matrices will be NA
      out <- rep(list(list(loadings = NA,
                           h2 = NA,
                           heywood = NA,
                           convergence = NA)),
                 times = length(method))
      names(out) <- method
      out_list[[i]] <- out
      next
    }
    
    # Create a list to store the estimated loading matrices
    loading_matrix_list <- rep(NA, times = length(method))
    loading_matrix_list <- as.list(loading_matrix_list)
    names(loading_matrix_list) <- method
    
    # Apply each of the chosen factor extraction methods
    for (j in 1:length(method)) {
      if (method[j] != "faml") {
        out <- fungible::faX(
          R = r, 
          n = sample_size, 
          numFactors = factors, 
          facMethod = method[j],
          faControl = list(treatHeywood = TRUE,
                           communality = "maxr",
                           epsilon = 1e-4))
        
        # Rotate and align factor loadings
        rotated_loadings <- fungible::faMain(urLoadings = out$loadings,
                                             numFactors = factors,
                                             rotate = rotate)
        if (factors > 1) {
          rotated_loadings$loadings <- fungible::faAlign(
            F1 = pop_loadings,
            F2 = rotated_loadings$loadings,
            MatchMethod = "LS"
          )$F2
        }
        
        loading_matrix_list[[j]] <- list(loadings = rotated_loadings$loadings,
                                         h2 = rotated_loadings$h2,
                                         heywood = any(rotated_loadings$h2 >= 1),
                                         convergence = out$faFit$converged)
      }  else if (method[j] == "faml") {
        # Initialize communality estimates as the maximum (absolute) correlation
        # in each column of the correlation matrix
        r <- cor(sample_data)
        diag(r) <- 0
        communality_estimates <- apply(r, MARGIN = 2, 
                                       FUN = function(X) {
                                         as.matrix(max(abs(X)))
                                       })
        
        out <- tryCatch(
          expr = factanal(x = sample_data,
                          factors = factors,
                          n.obs = sample_size,
                          start = communality_estimates,
                          scores = "none",
                          rotation = "none",
                          control = list(trace = FALSE,
                                         lower = 0.005)),
          error = function(e) NA
        )
        
        if (!is.na(out)) {
          # Unclass loadings matrix and strip attributes
          loadings <- unclass(out$loadings)
          attributes(loadings) <- NULL
          
          # Rotate and align factor loadings
          rotated_loadings <- fungible::faMain(urLoadings = out$loadings,
                                               numFactors = factors,
                                               rotate = rotate)
          if (factors > 1) {
            rotated_loadings$loadings <- fungible::faAlign(
              F1 = pop_loadings,
              F2 = rotated_loadings$loadings,
              MatchMethod = "LS"
            )$F2
          } else {
            rotated_loadings$loadings <- unclass(rotated_loadings$loadings)
          }
          
          loading_matrix_list[[j]] <- list(
            loadings = rotated_loadings$loadings,
            h2 = rotated_loadings$h2,
            heywood = any(rotated_loadings$h2 >= 1),
            convergence = out$converged
          )
        } else {
          loading_matrix_list[[j]] <- list(
            loadings = NA,
            h2 = NA,
            heywood = NA,
            convergence = FALSE
          )
        }
      }
    }
    out_list[[i]] <- loading_matrix_list
  }
  out_list
}
