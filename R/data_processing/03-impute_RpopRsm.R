# Impute missing data and fit models for Ds(Rpop, Rsm)
# Load packages
pacman::p_load(mice,
               lme4,
               tidyverse,
               miceadds)

# Load the data -----------------------------------------------------------
data_path <- "Data/results_matrix.RDS"
RpopRsm_data <- readRDS(data_path)

# Impute data for Rpop/Rsm distance ---------------------------------------
RpopRsm_data <- RpopRsm_data %>%
  dplyr::filter(npd == TRUE) %>% # only keep npd matrices
  select(id:npd, smoothing_method, distance_Rpop_Rsm) %>%
  distinct() # keep only unique observations of smoothed matrices

# Select only the variables to be used in regression/imputation
# Scale numeric predictors
# Log transform distance_RpopRsm and drop original variable
RpopRsm_data <- RpopRsm_data %>%
  dplyr::select(id:model_error, smoothing_method, distance_Rpop_Rsm) %>%
  mutate(log_distance_Rpop_Rsm = log(distance_Rpop_Rsm)) %>%
  dplyr::select(-distance_Rpop_Rsm) %>%
  mutate_at(.vars = vars(subjects_per_item:model_error), 
            .fun = function(x) as.vector(scale(x)))

# Create the predictor matrix, setting id as the grouping variable and only
# imputing log loading rsmd
pred_matrix <- make.predictorMatrix(data = RpopRsm_data)
pred_matrix[] <- 0
pred_matrix["log_distance_Rpop_Rsm", ] <- 1
pred_matrix["log_distance_Rpop_Rsm", c("rep", "condition", "log_distance_Rpop_Rsm")] <- 0
pred_matrix[c("log_distance_Rpop_Rsm"),"id"] <- -2

RNGkind(kind = "L'Ecuyer-CMRG")
set.seed(4893)
im_data_Rpop_Rsm <- mice::mice(
  data = RpopRsm_data,
  predictorMatrix = pred_matrix,
  m = 10,
  maxit = 1,
  method = "2l.pmm"
)
saveRDS(im_data_Rpop_Rsm, file = "Data/im_data_Rpop_Rsm_m10.RDS")

# Fit linear mixed effects models -----------------------------------------
# Fit the non-imputed model
RpopRsm_mod <- lmer(
  log_distance_Rpop_Rsm ~ (subjects_per_item + items_per_factor + 
                             factors + factor_loading + model_error +
                             smoothing_method)^2 + (1 | id),
  data = RpopRsm_data
)
saveRDS(RpopRsm_mod, file = "Data/RpopRsm_model.RDS")

# Fit the imputed model
RpopRsm_mod_im <- with(
  im_data_Rpop_Rsm,
  lmer(log_distance_Rpop_Rsm ~ (subjects_per_item + items_per_factor + 
                                  factors + factor_loading + 
                                  model_error + smoothing_method)^2 + (1 | id))
)
saveRDS(RpopRsm_mod_im, file = "Data/RpopRsm_imputed_model.RDS")