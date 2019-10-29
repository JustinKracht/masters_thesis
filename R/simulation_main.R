# Factor loading recovery with smoothed NPD tetrachoric correlation matrices
# Justin Kracht
# November 2019

# Load packages -----------------------------------------------------------
pacman::p_load(here,
               parallel,
               pbmcapply)
pacman::p_load(fungible,
               install = FALSE,
               update = FALSE)

# Source required functions -----------------------------------------------
source(here("R", "functions", "binary_data_generator.R"))

# Set data and error paths ------------------------------------------------
data_dir <- here("Data")
error_dir <- here("Data", "errors.txt")

# Define conditions -------------------------------------------------------
cores <- detectCores() - 2 # increase number of cores for parallel processing
reps <- 5
subjects_per_item <- c(5, 10, 15)
items_per_factor <- c(5, 10)
factors <- c(1, 3, 5, 10)
factor_loading <- c(0.3, 0.5, 0.8)
model_error <- c(0.0, 0.1, 0.3)
test_type <- c("wide",
               "difficult")

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

# Set seed ----------------------------------------------------------------
# If using parallel processing, need to use L'Ecuyer-CMRG
if (cores > 1) RNGkind("L'Ecuyer-CMRG")
set.seed(314159)

# Generate binary data ----------------------------------------------------
binary_data <- pbmclapply(
  X = 1:nrow(conditions_matrix),
  FUN = function(i) {
    binary_data_generator(
      reps = reps,
      subjects_per_item = conditions_matrix$subjects_per_item[i],
      items_per_factor = conditions_matrix$items_per_factor[i],
      factors = conditions_matrix$factors[i],
      factor_loading = conditions_matrix$factor_loading[i],
      model_error = conditions_matrix$model_error[i],
      test_type = conditions_matrix$test_type[i],
      diff_range = diff_range
    )
  },
  mc.cores = cores
)

# Compute tetrachoric correlation matrices --------------------------------
tetcor_matrices <- pbmclapply(
  X = 1:nrow(conditions_matrix),
  FUN = function(i) {
    tryCatch(
      expr = lapply(X = binary_data[[i]],
                    FUN = fungible::tetcor,
                    BiasCorrect = TRUE,
                    stderror = FALSE,
                    Smooth = FALSE,
                    max.iter = 2e4,
                    PRINT = FALSE),
      error = function(err.msg) {
        # Add error message to log file
        write(toString(c(err.msg, " Condition:", i)),
              error_dir, append = TRUE)
      })
  },
  mc.cores = cores
)
