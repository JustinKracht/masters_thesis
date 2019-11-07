# do_step(): Function that takes a step function as an argument and then
# executes that function in a while loop to make sure that all conditions are
# completed.
do_step <- function(step, conditions, step_dir, 
                    data_dir, error_dir, cores, max_iter = 50) {
  conditions_to_do <- conditions
  incomplete_conditions <- TRUE
  iter <- 0
  while (incomplete_conditions == TRUE) {
    # Generate tetrachoric matrix lists and save as .RDS files
    step(conditions = conditions_to_do,
         data_dir = data_dir,
         error_dir = error_dir,
         cores = cores)
    # Make a list of missing conditions (if there are any)
    conditions_to_do <- unsaved_condition_finder(
      conditions = conditions,
      dir = step_dir
    )
    
    # Make sure we never enter an infinite loop
    iter <- iter + 1
    if (iter >= max_iter) {
      stop("Exceeded maximum number of iterations (", max_iter, ").")
    }
    if (length(conditions_to_do) == 0) incomplete_conditions <- FALSE
  }
}