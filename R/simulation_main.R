# Factor loading recovery with smoothed NPD tetrachoric correlation matrices
# Justin Kracht
# April 2020

# Load packages -----------------------------------------------------------
pacman::p_load(mirt,
               here,
               parallel,
               pbmcapply,
               stringr)
pacman::p_load(fungible,
               install = FALSE,
               update = FALSE)

# Source required functions -----------------------------------------------
function_dir <- here("R", "functions")
source(paste0(function_dir, "/binary_data_generator.R"))
source(paste0(function_dir, "/smoothing_applicator.R"))
source(paste0(function_dir, "/loadings_estimator.R"))
source(paste0(function_dir, "/tetrachoric_step.R"))
source(paste0(function_dir, "/data_generation_step.R"))
source(paste0(function_dir, "/smoothing_step.R"))
source(paste0(function_dir, "/loading_estimation_step.R"))
source(paste0(function_dir, "/unsaved_condition_finder.R"))
source(paste0(function_dir, "/do_step.R"))

# Set data and error paths ------------------------------------------------
data_dir <- here("Data")
error_dir <- here("Data", "errors.txt")

# Set simulation options --------------------------------------------------
cores <- detectCores() - 1 # increase number of cores for parallel processing

# Define conditions -------------------------------------------------------
reps <- 1000
subjects_per_item <- c(5, 10, 15)
items_per_factor <- c(5, 10)
factors <- c(1, 3, 5, 10)
factor_loading <- c(0.3, 0.5, 0.8)
model_error <- c(0.0, 0.1, 0.3)
test_type <- c("wide")

# Set the minimum and maximum difficulty values for the wide and difficult
# conditions
diff_range <- list(wide = c(0.15, 0.85))

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
do_step(step = data_generation_step,
        conditions = 1:nrow(conditions_matrix),
        step_dir = paste0(data_dir, "/binary_data"),
        data_dir = data_dir,
        error_dir = error_dir,
        cores = cores)

# Compute tetrachoric correlation matrices --------------------------------
do_step(step = tetrachoric_step,
        conditions = 1:nrow(conditions_matrix),
        step_dir = paste0(data_dir, "/tetcor_matrices"),
        data_dir = data_dir,
        error_dir = error_dir,
        cores = cores)

# Apply matrix smoothing to NPD matrices ----------------------------------
# Smooth NPD matrices using all three smoothing algorithms
do_step(step = smoothing_step,
        conditions = 1:nrow(conditions_matrix),
        step_dir = paste0(data_dir, "/smoothed_matrices"),
        data_dir = data_dir,
        error_dir = error_dir,
        cores = cores)

# Estimate loading matrices -----------------------------------------------
do_step(step = loading_estimation_step,
        conditions = 1:nrow(conditions_matrix),
        step_dir = paste0(data_dir, "/loading_matrices"),
        data_dir = data_dir,
        error_dir = error_dir,
        cores = cores,
        conditions_matrix = conditions_matrix,
        reps = 1000,
        check_incomplete = FALSE)

# Save conditions_matrix and reps objects to use in data processing
save(list = c("conditions_matrix", "reps"),
     file = paste0(data_dir, "/environment.RData"))