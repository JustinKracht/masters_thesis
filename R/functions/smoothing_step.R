smoothing_step <- function(conditions, data_dir, error_dir, cores) {
  pbmcapply::pbmclapply(
    X = conditions,
    FUN = function(i) {
      tryCatch(
        expr = {
          tetcor_matrix_list <- readRDS(paste0(data_dir, 
                                               "/tetcor_matrices",
                                               "/tetcor_matrix_list", 
                                               formatC(i, width = 3, flag = 0), 
                                               ".RDS"))
          smoothed_matrices_out <- lapply(tetcor_matrix_list, 
                                          FUN = smoothing_applicator)

          saveRDS(smoothed_matrices_out, 
                  file = paste0(data_dir, 
                                "/smoothed_matrices",
                                "/smoothed_matrix_list",
                                formatC(i, 
                                        width = 3, 
                                        format = "d", 
                                        flag = "0"), 
                                ".RDS"))
        }, 
        error = function(err.msg) {
          # Add error message to log file
          write(toString(c("Error in matrix smoothing: ", err.msg, 
                           " Condition:", i)),
                error_dir, append = TRUE)
        })
    }, mc.cores = cores
  )
}