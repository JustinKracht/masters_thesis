# Fit mixed-effects model for Ds(Rpop, Rsm)
# Load packages
pacman::p_load(lme4,
               tidyverse)

# Load the data -----------------------------------------------------------
data_path <- "Data/results_matrix.RDS"
RpopRsm_data <- readRDS(data_path)

RpopRsm_data <- RpopRsm_data %>%
  dplyr::filter(npd == TRUE) %>% # only keep npd matrices
  dplyr::select(id:npd, smoothing_method, distance_Rpop_Rsm) %>%
  distinct() # keep only unique observations of smoothed matrices

# Select only the variables to be used in regression
# Scale numeric predictors
RpopRsm_data <- RpopRsm_data %>%
  dplyr::select(id:model_error, smoothing_method, distance_Rpop_Rsm) %>%
  mutate_at(.vars = vars(subjects_per_item:model_error),
            .fun = function(x) as.vector(scale(x,
                                               center = mean(unique(x)),
                                               scale = sd(unique(x)))))

# Fit linear mixed effects model ------------------------------------------
RpopRsm_mod <- lmer(
  log(distance_Rpop_Rsm) ~ (subjects_per_item + items_per_factor +
                             factors + factor_loading + model_error +
                             smoothing_method)^2 + (1 | id),
  data = RpopRsm_data
)

# Fit polynomial mixed effects model
RpopRsm_mod_poly <- lmer(
  log(distance_Rpop_Rsm) ~ (subjects_per_item + items_per_factor +
                              factors + factor_loading + model_error +
                              smoothing_method + I(subjects_per_item^2) + 
                              I(factors^2) + I(model_error^2))^2 - I(factors^2):factors - 
    I(subjects_per_item^2):subjects_per_item - I(model_error^2):model_error + (1 | id),
  data = RpopRsm_data
)

saveRDS(RpopRsm_mod, file = "Data/RpopRsm_model.RDS")
saveRDS(RpopRsm_mod_poly, file = "Data/RpopRsm_model_poly.RDS")
