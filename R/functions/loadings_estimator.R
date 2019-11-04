# loadings_estimator(): wrapper for psych::fa that extracts the estimated factor
# loading matrices from a correlation matrix using all of the specified methods.
loadings_estimator <- function(rsm_list,
                               sample_data,
                               sample_size,
                               factors,
                               method = c("fapa", "fals", "faml")) {
  
  # Create an empty list to store output
  out_list <- vector(mode = "list", length = length(rsm_list))
  names(out_list) <- names(rsm_list)
  
  for (i in 1:length(rsm_list)) {
    if (!is.na(dim(rsm_list[[i]]$R)[1])) {
      r <- rsm_list[[i]]$R
    } else {
      # If the tetrachoric matrix is not NPD, the smoothed matrices will be NA
      out_list[[i]] <- NA
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
        loading_matrix_list[[j]] <- list(loadings = out$loadings,
                                         h2 = out$h2,
                                         heywood = out$faFit$Heywood,
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
        out <- factanal(x = sample_data,
                        factors = factors,
                        n.obs = sample_size,
                        start = communality_estimates,
                        scores = "none",
                        rotation = "none",
                        control = list(trace = FALSE,
                                       lower = 0.005))
        
        # Unclass loadings matrix and strip attributes
        loadings <- unclass(out$loadings)
        attributes(loadings) <- NULL
        
        loading_matrix_list[[j]] <- list(
            loadings = loadings,
            h2 = 1 - out$uniquenesses,
            heywood = any(out$uniquenesses <= 0),
            convergence = out$converged
          )
      }
    }
    out_list[[i]] <- loading_matrix_list
  }
  out_list
}
