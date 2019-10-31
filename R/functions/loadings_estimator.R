# loadings_estimator(): wrapper for psych::fa that extracts the estimated factor
# loading matrices from a correlation matrix using all of the specified methods.
loadings_estimator <- function(smoothed_matrices,
                               n.obs,
                               nfactors,
                               rotate = "none",
                               max.iter = 1e4,
                               warnings = FALSE,
                               method = c("pa", "minres", "ml", "minrank")) {
  # Create a list to store the estimated loading matrices
  loading_matrix_list <- rep(NA, times = length(method))
  loading_matrix_list <- as.list(loading_matrix_list)
  names(loading_matrix_list) <- method
  
  for (i in 1:length(smoothed_matrices)) {
    r <- smoothed_matrices[[i]][[1]]
    # If the tetrachoric matrix is not NPD, the smoothed matrices will be NULL
    # Or, if a smoothing method didn't converge, it will be NA
    if (is.null(dim(r)[1])) next
    # Apply each of the chosen factor extraction methods
    for (j in 1:length(method)) {
      loading_matrix_list[[i]] <- unclass(
        psych::fa(r = r,
                  n.obs = n.obs,
                  nfactors = nfactors,
                  rotate = rotate,
                  scores = "none",
                  residuals = FALSE,
                  covar = FALSE,
                  max.iter = max.iter,
                  warnings = FALSE,
                  fm = method[i])$loadings
      )
    }
  }
  loading_matrix_list
}
