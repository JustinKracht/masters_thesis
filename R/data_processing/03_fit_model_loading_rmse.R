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

# Scale variables
loading_data <- loading_data %>%
  mutate_at(.vars = vars(subjects_per_item:model_error),
            .fun = function(x) as.vector(scale(x,
                                               center = mean(unique(x)),
                                               scale  = sd(unique(x)))))

# Fit mixed-effects model -------------------------------------------------
rmse_mod <- lmer(
  log(loading_rmsd) ~ (subjects_per_item + items_per_factor + 
                        factors + factor_loading + model_error +
                        smoothing_method + fa_method)^2 + (1 | id),
  data = loading_data
)

# Fit the model with degree-2 polynomial terms
rmse_mod_poly <- lmer(
  log(loading_rmsd) ~ (subjects_per_item + items_per_factor + 
                         factors + factor_loading + model_error +
                         smoothing_method + fa_method +
                         I(subjects_per_item^2) +
                         I(factor_loading^2) +
                         I(factors^2) + 
                         I(model_error^2))^2 - 
    I(subjects_per_item^2):subjects_per_item -
    I(factor_loading^2):factor_loading -
    I(factors^2):factors - 
    I(model_error^2):model_error + (1 | id),
  data = loading_data
)


saveRDS(rmse_mod, file = "Data/loading_model.RDS")
saveRDS(rmse_mod_poly, file = "Data/loading_model_poly.RDS")
