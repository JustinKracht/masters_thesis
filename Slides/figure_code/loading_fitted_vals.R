# Fit mixed-effects model
pacman::p_load(lme4,
               tidyverse,
               emmeans,
               latex2exp)

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

# Load the fitted model
rmse_mod <- readRDS("Data/loading_model_poly.RDS")

loading_emm <- emmeans(rmse_mod,
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
loading_emm <- data.frame(loading_emm) %>%
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

ggplot(filter(loading_emm, 
              # smoothing_method %in% c("BY", "None"),
              # subjects_per_item != 10,
              fa_method %in% c("faml", "fals")
),
aes(x = factor_loading, 
    y = response, 
    color = smoothing_method, 
    group = interaction(fa_method_rec, smoothing_method))) +
  geom_pointrange(aes(shape = as.factor(fa_method_rec),
                      ymin = asymp.LCL,
                      ymax = asymp.UCL),
                  fatten = 2) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7)) +
  geom_line(aes(linetype = smoothing_method)) +
  facet_grid(subjects_per_item_rec * items_per_factor_rec ~ 
               factors_rec * model_error_rec,
             scales = "fixed",
             labeller = label_parsed) +
  labs(y = latex2exp::TeX("$RMSE(\\mathbf{F}, \\hat{\\mathbf{F}})$"),
       x = "Factor Loading",
       color = "Smoothing \nMethod",
       linetype = "Smoothing \nMethod",
       shape = "Extraction \nMethod")

ggsave(filename = "loading_fitted_vals.png",
       path = "Slides/figures",
       plot = last_plot(),
       dpi = 320,
       width = 9,
       height = 6.75,
       units = "in",
       scale = 1.2)

# Smoothing method effects ------------------------------------------------
# loading_emm_smooth <- emmeans(
#   rmse_mod,
#   at = list(subjects_per_item = unique(loading_data$subjects_per_item),
#             items_per_factor = unique(loading_data$items_per_factor),
#             factors = unique(loading_data$factors),
#             factor_loading = unique(loading_data$factor_loading),
#             model_error = unique(loading_data$model_error),
#             smoothing_method = unique(loading_data$smoothing_method),
#             fa_method = unique(loading_data$fa_method)),
#   spec = c("items_per_factor",
#            "subjects_per_item",
#            "factor_loading",
#            "smoothing_method",
#            "fa_method"),
#   type = "response",
#   level = .99
# )
# 
# # Back-transform the independent variables
# loading_emm_smooth <- data.frame(loading_emm_smooth) %>%
#   mutate(subjects_per_item = subjects_per_item * 5 + 10,
#          subjects_per_item_rec = factor(subjects_per_item,
#                                         labels = paste0("Subjects/Item: ", 
#                                                         c(5, 10, 15))),
#          items_per_factor = round(items_per_factor * 3.535 + 7.5, 0),
#          items_per_factor_rec = factor(items_per_factor,
#                                        labels = c("Items/Factor: 5",
#                                                   "Items/Factor: 10")),
#          factor_loading = round(factor_loading * 0.2516611 + 0.5333333, 1),
#          fa_method_rec = factor(fa_method,
#                                 labels = c("OLS", "ML", "PA")))
# 
# ggplot(loading_emm_smooth,
#        aes(x = factor_loading, 
#            y = response, 
#            color = smoothing_method, 
#            group = smoothing_method)) +
#   geom_pointrange(aes(shape = as.factor(smoothing_method),
#                       ymin = asymp.LCL,
#                       ymax = asymp.UCL),
#                   fatten = 2) +
#   scale_x_continuous(breaks = c(0.3, 0.5, 0.7)) +
#   geom_line(aes(linetype = smoothing_method)) +
#   facet_grid(subjects_per_item_rec ~ items_per_factor_rec * fa_method_rec,
#              scales = "fixed") +
#   labs(y = latex2exp::TeX("$RMSE(\\mathbf{F}, \\hat{\\mathbf{F}})$"),
#        x = "Factor Loading",
#        color = "Smoothing \nMethod",
#        linetype = "Smoothing \nMethod",
#        shape = "Smoothing \nMethod")
# 
# ggsave(filename = "loading_fitted_vals_smooth_method.png",
#        path = "Slides/figures",
#        plot = last_plot(),
#        dpi = 320,
#        width = 5,
#        height = 3.5,
#        units = "in",
#        scale = 1.2)
