# Factor loading recovery with smoothed NPD tetrachoric correlation matrices
# Justin Kracht
# November 2019

# Load packages -----------------------------------------------------------
pacman::p_load(mirt,
               here,
               parallel,
               pbmcapply)
pacman::p_load(fungible,
               install = FALSE,
               update = FALSE)

# Source required functions -----------------------------------------------
source(here("R", "functions", "binary_data_generator.R"))
source(here("R", "functions", "smoothing_applicator.R"))
source(here("R", "functions", "loadings_estimator.R"))

# Set data and error paths ------------------------------------------------
data_dir <- here("Data")
error_dir <- here("Data", "errors.txt")

# Set simulation options --------------------------------------------------
cores <- detectCores() - 1 # increase number of cores for parallel processing
save_partial <- FALSE # save RDS files for each condition in case of a crash
save_image <- TRUE # save environment at the end of the simulation

# Define conditions -------------------------------------------------------
reps <- 5
subjects_per_item <- c(5, 10, 15)
items_per_factor <- c(5, 10)
factors <- c(1, 3, 5, 10)
factor_loading <- c(0.3, 0.5, 0.8)
model_error <- c(0.0, 0.1, 0.3)
test_type <- c("wide") # difficult

# Set the minimum and maximum difficulty values for the wide and difficult
# conditions
diff_range <- list(wide = c(0.15, 0.85),
                   difficult = c(0.05, 0.30))

# Create a conditions matrix with all combinations of factor levels
conditions_matrix <- expand.grid(subjects_per_item = subjects_per_item,
                                 items_per_factor = items_per_factor,
                                 factors = factors,
                                 factor_loading = factor_loading,
                                 model_error = model_error,
                                 test_type = test_type)

# Calculate number of items and subjects for each condition
conditions_matrix$items <- 
  conditions_matrix$items_per_factor * conditions_matrix$factors
conditions_matrix$sample_size <- 
  conditions_matrix$subjects_per_item * conditions_matrix$items

# Set seed ----------------------------------------------------------------
# If using parallel processing, need to use L'Ecuyer-CMRG
if (cores > 1) RNGkind("L'Ecuyer-CMRG")
set.seed(314159)

# Generate binary data ----------------------------------------------------
binary_data <- pbmcapply::pbmclapply(
  X = 1:nrow(conditions_matrix),
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
        if (save_partial) saveRDS(binary_data_out, 
                                  file = paste0(data_dir, 
                                                "/binary_data",
                                                "/binary_data",
                                                formatC(i, 
                                                        width = 3, 
                                                        format = "d", 
                                                        flag = "0"), 
                                                ".RDS"))
        binary_data_out
      }, error = function(err.msg) {
        # Add error message to log file
        write(toString(c("Error in binary data generation: ", err.msg, 
                         " Condition:", i)),
              error_dir, append = TRUE)
      })
  },
  mc.cores = cores
)

if (!save_partial) saveRDS(binary_data, 
                           file = paste0(data_dir, 
                                         "/binary_data",
                                         "/binary_data.RDS"))

# Compute tetrachoric correlation matrices --------------------------------
tetcor_matrix_list <- pbmcapply::pbmclapply(
  X = 1:nrow(conditions_matrix),
  FUN = function(i) {
    tryCatch(
      expr = {
        tetcor_out <- lapply(X = binary_data[[i]],
                             FUN = function(x) {
                               fungible::tetcor(x$sample_data,
                                                BiasCorrect = TRUE,
                                                stderror = FALSE,
                                                Smooth = FALSE,
                                                max.iter = 2e4,
                                                PRINT = FALSE)$r
                             })
        # If save_partial is TRUE, save an RDS file for each condition
        if (save_partial) saveRDS(tetcor_out, 
                                  file = paste0(data_dir, 
                                                "/tetcor_matrices",
                                                "/tetcor_matrices",
                                                formatC(i, 
                                                        width = 3, 
                                                        format = "d", 
                                                        flag = "0"), 
                                                ".RDS"))
        tetcor_out
      },
      error = function(err.msg) {
        # Add error message to log file
        write(toString(c("Error in tetcor: ", err.msg, " Condition:", i)),
              error_dir, append = TRUE)
      })
  },
  mc.cores = cores
)

if (!save_partial) saveRDS(tetcor_matrix_list, 
                           file = paste0(data_dir, 
                                         "/tetcor_matrices",
                                         "/tetcor_matrix_list.RDS"))

# Apply matrix smoothing to NPD matrices ----------------------------------
# # Load data generated previously, if applicable
# binary_data <- readRDS(file = paste0(data_dir, "/binary_data/binary_data.RDS"))
# tetcor_matrix_list <- readRDS(file = paste0(data_dir, 
#                                             "/tetcor_matrices/tetcor_matrix_list.RDS"))

# Smooth NPD matrices using all three smoothing algorithms
smoothed_matrix_list <- pbmclapply::pbmclapply(
  X = 1:nrow(conditions_matrix),
  FUN = function(i) {
    tryCatch(
      expr = {
        smoothed_matrices_out <- lapply(tetcor_matrix_list[[i]], 
                                        FUN = smoothing_applicator)
        # If save_partial is TRUE, save an RDS file for each condition
        if (save_partial) saveRDS(smoothed_matrices_out, 
                                  file = paste0(data_dir, 
                                                "/smoothed_matrices",
                                                "/smoothed_matrix_list",
                                                formatC(i, 
                                                        width = 3, 
                                                        format = "d", 
                                                        flag = "0"), 
                                                ".RDS"))
        smoothed_matrices_out
      }, 
      error = function(err.msg) {
        # Add error message to log file
        write(toString(c("Error in matrix smoothing: ", err.msg, 
                         " Condition:", i)),
              error_dir, append = TRUE)
      })
  }, mc.cores = cores
)

if (!save_partial) saveRDS(smoothed_matrix_list, 
                           file = paste0(data_dir, 
                                         "/smoothed_matrices",
                                         "/smoothed_matrix_list.RDS"))

# Estimate factor loading matrices ----------------------------------------
loading_matrix_list <- pbmcapply::pbmclapply(
  X = 1:nrow(conditions_matrix),
  FUN = function(i) {
    lapply(X = 1:reps,
           FUN = function(j) {
             tryCatch(
               expr = {
                 loading_matrices_out <- suppressWarnings(
                   loadings_estimator(
                     rsm_list = smoothed_matrix_list[[i]][[j]]$smoothed_matrices,
                     sample_data = binary_data[[i]][[j]]$sample_data,
                     pop_loadings = binary_data[[i]][[j]]$loadings,
                     sample_size = conditions_matrix$sample_size[i],
                     factors = conditions_matrix$factors[i],
                     method = c("fapa", "fals", "faml"),
                     rotate = "quartimin"
                   ))
                 
                 # If save_partial is TRUE, save an RDS file for each condition
                 if (save_partial) saveRDS(loading_matrices_out, 
                                           file = paste0(data_dir, 
                                                         "/loading_matrices",
                                                         "/loading_matrix_list",
                                                         formatC(i, 
                                                                 width = 3, 
                                                                 format = "d", 
                                                                 flag = "0"), 
                                                         ".RDS"))
                 loading_matrices_out
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

if (!save_partial) saveRDS(loading_matrix_list, 
                           file = paste0(data_dir, "/loading_matrices",
                                         "/loading_matrix_list.RDS"))
if (save_image) save.image(file = "environment.RDS")