loading_estimation_step <- function(conditions, data_dir, error_dir, cores) {
  pbmcapply::pbmclapply(
    X = 1:nrow(conditions_matrix),
    FUN = function(i) {
      lapply(X = 1:reps,
             FUN = function(j) {
               tryCatch(
                 expr = {
                   smoothed_matrix_list <- readRDS(
                     paste0(data_dir, 
                            "/smoothed_matrices",
                            "/smoothed_matrix_list", 
                            formatC(i, width = 3, flag = 0), 
                            ".RDS")
                   )
                   binary_data <- readRDS(
                     paste0(data_dir, 
                            "/binary_data",
                            "/binary_data", 
                            formatC(i, width = 3, flag = 0), 
                            ".RDS")
                   )
                   loading_matrices_out <- suppressWarnings(
                     loadings_estimator(
                       rsm_list = smoothed_matrix_list[[j]]$smoothed_matrices,
                       sample_data = binary_data[[j]]$sample_data,
                       pop_loadings = binary_data[[j]]$loadings,
                       sample_size = conditions_matrix$sample_size[i],
                       factors = conditions_matrix$factors[i],
                       method = c("fapa", "fals", "faml"),
                       rotate = "quartimin"
                     ))
                   
                   saveRDS(loading_matrices_out, 
                           file = paste0(data_dir, 
                                         "/loading_matrices",
                                         "/loading_matrix_list",
                                         formatC(i, 
                                                 width = 3, 
                                                 format = "d", 
                                                 flag = "0"), 
                                         ".RDS"))
                 }, error = function(err.msg) {
                   # Add error message to log file
                   write(toString(c("Error in loading matrix estimation: ", 
                                    err.msg, " Condition:", i, "Rep:", j)),
                         error_dir, append = TRUE)
                 }
               )
             })
    }, mc.cores = cores
  )
}