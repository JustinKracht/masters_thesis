# Impute missing data and fit models
pacman::p_load(mice,
               lme4,
               tidyverse,
               miceadds)

# Load the data -----------------------------------------------------------
data_path <- "Data/results_matrix.RDS"
results_matrix <- readRDS(data_path)

# Don't want to include results for which the FA algorithm didn't converge
# but do want to include results for which convergence is NA because the
# smoothing algorithm didn't converge.
imp_subset <- results_matrix %>%
  filter(npd == TRUE) %>%
  filter((fa_convergence == TRUE | is.na(fa_convergence)))

# Select only the variables to be used in regression/imputation
imp_subset <- imp_subset %>%
  dplyr::select(id:model_error, APA_converged, BY_converged,
                smoothing_method, fa_method, loading_rmsd)

# Impute missing data -----------------------------------------------------
imp_subset <- imp_subset %>%
  mutate(log_loading_rmsd = log(loading_rmsd)) %>%
  dplyr::select(-loading_rmsd) %>%
  mutate_at(.vars = vars(subjects_per_item:model_error), 
            .fun = function(x) as.vector(scale(x)))

# Create the predictor matrix, setting id as the grouping variable and only
# imputing log loading rsmd
pred_matrix <- make.predictorMatrix(data = imp_subset)
pred_matrix[] <- 0
pred_matrix["log_loading_rmsd",] <- 1
pred_matrix["log_loading_rmsd", "log_loading_rmsd"] <- 0
pred_matrix[,c("rep", "condition")] <- 0
pred_matrix["log_loading_rmsd","id"] <- -2

RNGkind(kind = "L'Ecuyer-CMRG")
set.seed(4893)
im_data <- mice::mice(
  data = imp_subset,
  predictorMatrix = pred_matrix,
  m = 10,
  maxit = 1,
  method = "2l.pmm"
)
saveRDS(im_data, file = "Data/im_data_m10.RDS")

# Fit mixed effects model on the data subset ------------------------------
rmse_mod <- lmer(
  log_loading_rmsd ~ (subjects_per_item + items_per_factor + 
                        factors + factor_loading + model_error +
                        smoothing_method + fa_method)^2 + (1 | id),
  data = imp_subset
)
saveRDS(rmse_mod, file = "Data/loading_model.RDS")

rmse_mod_im <- with(
  im_data,
  lmer(log_loading_rmsd ~ (subjects_per_item + items_per_factor + 
                             factors + factor_loading + 
                             model_error + smoothing_method + 
                             fa_method)^2 + (1 | id))
)
saveRDS(rmse_mod_im, file = "Data/loading_imputed_model.RDS")