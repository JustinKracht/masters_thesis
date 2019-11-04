# loadings_estimator(): wrapper for psych::fa that extracts the estimated factor
# loading matrices from a correlation matrix using all of the specified methods.
loadings_estimator <- function(rsm_list,
                               sample_data,
                               sample_size,
                               factors,
                               method = c("fapa", "fals", "faml")) {
  # Create a list to store the estimated loading matrices
  loading_matrix_list <- rep(NA, times = length(method))
  loading_matrix_list <- as.list(loading_matrix_list)
  names(loading_matrix_list) <- method
  
  for (i in 1:length(rsm_list)) {
    r <- rsm_list[[i]]
    # If the tetrachoric matrix is not NPD, the smoothed matrices will be NULL
    # Or, if a smoothing method didn't converge, it will be NA
    if (is.null(dim(r)[1])) next
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
        out <- mirt::mirt(as.data.frame(sample_data), 
                          model = factors, 
                          itemtype = "2PL", 
                          SE = FALSE,
                          verbose = FALSE)
        
        loading_matrix_list[[j]] <- list(
          loadings = extract.mirt(out, what = "F"),
          h2 = extract.mirt(out, what = "h2"),
          convergence = extract.mirt(out, what = "converged")
        )
      }
    }
  }
  loading_matrix_list
}
