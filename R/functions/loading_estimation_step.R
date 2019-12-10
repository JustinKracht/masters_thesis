loading_estimation_step <- function(conditions, data_dir, error_dir, cores, reps_to_do) {
  pbmcapply::pbmclapply(
    X = 1:nrow(conditions_matrix),
    FUN = function(i) {
      # Read in smoothed matrix list for condition i
      smoothed_matrix_list <- readRDS(
        paste0(data_dir, 
               "/smoothed_matrices",
               "/smoothed_matrix_list", 
               formatC(i, width = 3, flag = 0), 
               ".RDS")
      )
      # Read in binary data list for condition i
      binary_data <- readRDS(
        paste0(data_dir, 
               "/binary_data",
               "/binary_data", 
               formatC(i, width = 3, flag = 0), 
               ".RDS")
      )
      
      incomplete_reps <- TRUE
      
      while (incomplete_reps == TRUE) {
        loading_matrices_out <- lapply(
          X = reps_to_do,
          FUN = function(j) {
            tryCatch(
              expr = {
                suppressWarnings(
                  loadings_estimator(
                    rsm_list = smoothed_matrix_list[[j]]$smoothed_matrices,
                    sample_data = binary_data[[j]]$sample_data,
                    pop_loadings = binary_data[[j]]$loadings,
                    sample_size = conditions_matrix$sample_size[i],
                    factors = conditions_matrix$factors[i],
                    method = c("fapa", "fals", "faml"),
                    rotate = "quartimin"
                  ))
              }, error = function(err.msg) {
                # Add error message to log file
                write(toString(c("Error in loading matrix estimation: ", 
                                 err.msg, " Condition:", i, "Rep:", j)),
                      error_dir, append = TRUE)
              }
            )
          })
        reps_to_do <- which(lapply(loading_matrices_out, length) == 0)
        incomplete_reps <- (length(reps_to_do) == 0)
      }
      
      # Save condition list to an RDS file
      saveRDS(loading_matrices_out, 
              file = paste0(data_dir, 
                            "/loading_matrices",
                            "/loading_matrix_list",
                            formatC(i, 
                                    width = 3, 
                                    format = "d", 
                                    flag = "0"), 
                            ".RDS"))
    }, mc.cores = cores
  )
}