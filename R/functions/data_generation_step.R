data_generation_step <- function(conditions, data_dir, error_dir, cores, conditions_matrix, reps, ...) {
  pbmcapply::pbmclapply(
    X = conditions,
    FUN = function(i) {
      tryCatch(
        expr = {
          binary_data_out <- binary_data_generator(
            reps = reps,
            subjects_per_item = conditions_matrix$subjects_per_item[i],
            items_per_factor = conditions_matrix$items_per_factor[i],
            factors = conditions_matrix$factors[i],
            factor_loading = conditions_matrix$factor_loading[i],
            model_error = conditions_matrix$model_error[i],
            test_type = conditions_matrix$test_type[i],
            diff_range = diff_range
          )
          
          # If save_partial is TRUE, save an RDS file for each condition
          saveRDS(binary_data_out, 
                  file = paste0(data_dir, 
                                "/binary_data",
                                "/binary_data",
                                formatC(i, 
                                        width = 3, 
                                        format = "d", 
                                        flag = "0"), 
                                ".RDS"))
        }, error = function(err.msg) {
          # Add error message to log file
          write(toString(c("Error in binary data generation: ", err.msg, 
                           " Condition:", i)),
                error_dir, append = TRUE)
        })
    },
    mc.cores = cores
  )
}