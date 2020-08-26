pacman::p_load(lme4,
               tidyverse,
               emmeans,
               latex2exp)

# Load the data -----------------------------------------------------------
data_path <- "Data/results_matrix_npd_with_bias.RDS"
results_matrix <- readRDS(data_path)

RpopRsm_data <- results_matrix %>%
  dplyr::filter(npd == TRUE) %>% # only keep npd matrices
  dplyr::select(id:npd, smoothing_method, bias_Rpop_Rsm) %>%
  distinct() # keep only unique observations of smoothed matrices

# Select only the variables to be used in regression
# Scale numeric predictors
RpopRsm_data <- RpopRsm_data %>%
  dplyr::select(id:model_error, smoothing_method, bias_Rpop_Rsm) %>%
  mutate_at(.vars = vars(subjects_per_item:model_error),
            .fun = function(x) as.vector(scale(x,
                                               center = mean(unique(x)),
                                               scale = sd(unique(x)))))

# Load the fitted model
Rpop_Rsm_bias_mod <- readRDS("Data/RpopRsm_bias_mod_poly.RDS")

Rpop_Rsm_bias_emm <- emmeans(
  Rpop_Rsm_bias_mod,
  at = list(subjects_per_item = unique(RpopRsm_data$subjects_per_item),
            items_per_factor = unique(RpopRsm_data$items_per_factor),
            factors = unique(RpopRsm_data$factors),
            factor_loading = unique(RpopRsm_data$factor_loading),
            model_error = unique(RpopRsm_data$model_error),
            smoothing_method = unique(RpopRsm_data$smoothing_method)),
  spec = c("subjects_per_item",
           "items_per_factor",
           "factors",
           "factor_loading",
           # "model_error",
           "smoothing_method"),
  type = "response",
  level = .99
)

# Back-transform the independent variables
Rpop_Rsm_bias_emm <- data.frame(Rpop_Rsm_bias_emm) %>%
  mutate(subjects_per_item = subjects_per_item * 5 + 10,
         subjects_per_item_rec = factor(subjects_per_item,
                                        labels = paste0("Subjects/Item: ", 
                                                        c(5, 10, 15))),
         items_per_factor = round(items_per_factor * 3.535 + 7.5, 0),
         items_per_factor_rec = factor(items_per_factor,
                                       labels = c("Items/Factor: 5",
                                                  "Items/Factor: 10")),
         # model_error = round(model_error * 0.1527525 + 0.133333, 1),
         # model_error_rec = factor(model_error,
         #                          labels = c("Error: 0", "Error: 0.1", "Error: 0.3")),
         factors = round(factors * 3.8622 + 4.75, 0),
         factors_rec = factor(factors,
                              labels = c("Factors: 1", "Factors: 3", 
                                         "Factors: 5", "Factors: 10")),
         factor_loading = round(factor_loading * 0.2516611 + 0.5333333, 1))

ggplot(Rpop_Rsm_bias_emm,
       aes(x = factor_loading, 
           y = response + mean(RpopRsm_data$bias_Rpop_Rsm, na.rm = TRUE) * sd(RpopRsm_data$bias_Rpop_Rsm, na.rm = TRUE), 
           color = smoothing_method, 
           group = smoothing_method)) +
  geom_pointrange(aes(shape = smoothing_method,
                      ymin = asymp.LCL,
                      ymax = asymp.UCL),
                  size = 0.3) +
  geom_line(aes(linetype = smoothing_method)) +
  facet_grid(subjects_per_item_rec * items_per_factor_rec ~ 
               factors_rec,
             scales = "fixed",
             labeller = label_parsed) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  theme_minimal() +
  labs(y = "Correlation Bias",
       x = "Factor Loading",
       color = "Smoothing Method",
       shape = "Smoothing Method",
       linetype = "Smoothing Method")

ggsave(filename = "RpopRsm_bias_fitted_vals.png",
       path = "Text/figs",
       plot = last_plot(),
       dpi = 320,
       width = 8,
       height = 8,
       units = "in",
       scale = 1)

# Loading bias fitted values ----
# Don't want to include results for which the FA algorithm didn't converge
# but do want to include results for which convergence is NA because the
# smoothing algorithm didn't converge.
loading_data <- results_matrix %>%
  filter(npd == TRUE) %>%
  filter((fa_convergence == TRUE | is.na(fa_convergence)))

# Select only the variables to be used in regression/imputation
loading_data <- loading_data %>%
  dplyr::select(id:model_error, APA_converged, BY_converged,
                smoothing_method, fa_method, loading_rmsd, loading_bias)

# Scale variables
loading_data <- loading_data %>%
  mutate_at(.vars = vars(subjects_per_item:model_error),
            .fun = function(x) as.vector(scale(x,
                                               center = mean(unique(x)),
                                               scale  = sd(unique(x)))))

# Load the fitted model
loading_bias_mod <- readRDS("Data/loading_bias_mod_poly.RDS")

loading_bias_emm <- emmeans(loading_bias_mod,
                       at = list(subjects_per_item = unique(loading_data$subjects_per_item),
                                 items_per_factor = unique(loading_data$items_per_factor),
                                 factors = unique(loading_data$factors),
                                 factor_loading = unique(loading_data$factor_loading),
                                 model_error = unique(loading_data$model_error),
                                 smoothing_method = unique(loading_data$smoothing_method),
                                 fa_method = unique(loading_data$fa_method)),
                       spec = c("subjects_per_item",
                                "items_per_factor",
                                "factors",
                                "factor_loading",
                                "model_error",
                                "smoothing_method",
                                "fa_method"),
                       type = "response",
                       level = .99)

# Back-transform the independent variables
loading_bias_emm <- data.frame(loading_bias_emm) %>%
  mutate(subjects_per_item = subjects_per_item * 5 + 10,
         subjects_per_item_rec = factor(subjects_per_item,
                                        labels = paste0("Subjects/Item: ", 
                                                        c(5, 10, 15))),
         items_per_factor = round(items_per_factor * 3.535 + 7.5, 0),
         items_per_factor_rec = factor(items_per_factor,
                                       labels = c("Items/Factor: 5",
                                                  "Items/Factor: 10")),
         factors = round(factors * 3.8622 + 4.75, 0),
         factors_rec = factor(factors,
                              labels = c("Factors: 1", "Factors: 3", 
                                         "Factors: 5", "Factors: 10")),
         model_error = round(model_error * 0.1527525 + 0.1333333, 1),
         model_error_rec = factor(model_error,
                                  labels = c(TeX("$\\upsilon_E$: 0"),
                                             TeX("$\\upsilon_E$: 0.1"),
                                             TeX("$\\upsilon_E$: 0.3"))),
         factor_loading = round(factor_loading * 0.2516611 + 0.5333333, 1),
         fa_method_rec = factor(fa_method,
                                labels = c("OLS", "ML", "PA")))

# %>%
#   mutate_at(.vars = c("response", "asymp.LCL", "asymp.UCL"),
#             .funs = function(x) x * sd(loading_data$loading_bias, na.rm = TRUE) 
#             + mean(loading_data$loading_bias, na.rm = TRUE))

ggplot(filter(loading_bias_emm, 
              # smoothing_method %in% c("BY", "None"),
              model_error != 0.1,
              subjects_per_item != 10,
              fa_method %in% c("faml", "fals")),
       aes(x = factor_loading, 
           y = response * sd(loading_data$loading_bias, na.rm = TRUE) + 
             mean(loading_data$loading_bias, na.rm = TRUE), 
           color = smoothing_method, 
           group = interaction(fa_method_rec, smoothing_method))) +
  geom_pointrange(aes(shape = as.factor(fa_method_rec),
                      ymin = asymp.LCL* sd(loading_data$loading_bias, na.rm = TRUE) + 
                        mean(loading_data$loading_bias, na.rm = TRUE),
                      ymax = asymp.UCL* sd(loading_data$loading_bias, na.rm = TRUE) + 
                        mean(loading_data$loading_bias, na.rm = TRUE)),
                  size = 0.3) +
  geom_line(aes(linetype = smoothing_method)) +
  geom_hline(aes(yintercept = 0), color = "grey50", linetype = "dashed") +
  facet_grid(subjects_per_item_rec * items_per_factor_rec ~ 
               factors_rec * model_error_rec,
             scales = "fixed",
             labeller = label_parsed) +
  theme_minimal() +
  labs(y = "Loading Bias",
       x = "Factor Loading",
       color = "Smoothing Method",
       linetype = "Smoothing Method",
       shape = "Extraction Method")

ggsave(filename = "loading_bias_fitted_vals.png",
       path = "Text/figs",
       plot = last_plot(),
       dpi = 320,
       width = 12,
       height = 8,
       units = "in",
       scale = 1)
