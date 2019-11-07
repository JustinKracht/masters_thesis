tetrachoric_step <- function(conditions, data_dir, error_dir, cores) {
  pbmcapply::pbmclapply(
    X = conditions,
    FUN = function(i) {
      tryCatch(
        expr = {
          binary_data <- readRDS(paste0(data_dir, 
                                        "/binary_data",
                                        "/binary_data", 
                                        formatC(i, width = 3, flag = 0), 
                                        ".RDS"))
          tetcor_out <- lapply(X = binary_data,
                               FUN = function(x) {
                                 fungible::tetcor(x$sample_data,
                                                  BiasCorrect = TRUE,
                                                  stderror = FALSE,
                                                  Smooth = FALSE,
                                                  max.iter = 2e4,
                                                  PRINT = FALSE)$r
                               })

          saveRDS(tetcor_out, 
                  file = paste0(data_dir, 
                                "/tetcor_matrices",
                                "/tetcor_matrix_list",
                                formatC(i, 
                                        width = 3, 
                                        format = "d", 
                                        flag = "0"), 
                                ".RDS"))
        },
        error = function(err.msg) {
          # Add error message to log file
          write(toString(c("Error in tetcor: ", err.msg, " Condition:", i)),
                error_dir, append = TRUE)
        })
    },
    mc.cores = cores
  )
}
