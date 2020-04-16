# Fit mixed-effects model
pacman::p_load(lme4,
               tidyverse)

# Load the data -----------------------------------------------------------
data_path <- "Data/results_matrix.RDS"
results_matrix <- readRDS(data_path)

# Don't want to include results for which the FA algorithm didn't converge
# but do want to include results for which convergence is NA because the
# smoothing algorithm didn't converge.
loading_data <- results_matrix %>%
  filter(npd == TRUE) %>%
  filter((fa_convergence == TRUE | is.na(fa_convergence)))

# Select only the variables to be used in regression/imputation
loading_data <- loading_data %>%
  dplyr::select(id:model_error, APA_converged, BY_converged,
                smoothing_method, fa_method, loading_rmsd)

# Scale variables and log-transform loading_rmsd
loading_data <- loading_data %>%
  mutate(log_loading_rmsd = log(loading_rmsd)) %>%
  dplyr::select(-loading_rmsd) %>%
  mutate_at(.vars = vars(subjects_per_item:model_error), 
            .fun = function(x) as.vector(scale(x)))

# Fit mixed-effects model -------------------------------------------------
rmse_mod <- lmer(
  log_loading_rmsd ~ (subjects_per_item + items_per_factor + 
                        factors + factor_loading + model_error +
                        smoothing_method + fa_method)^2 + (1 | id),
  data = loading_data
)
saveRDS(rmse_mod, file = "Data/loading_model.RDS")