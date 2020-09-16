# Fit mixed-effects model
pacman::p_load(lme4,
               tidyverse,
               emmeans,
               latex2exp)

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

# Load the fitted model
Ds_mod <- readRDS("Data/RpopRsm_model_poly.RDS")

Ds_emm <- emmeans(
  Ds_mod,
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
Ds_emm <- data.frame(Ds_emm) %>%
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
         factor_loading = round(factor_loading * 0.2516611 + 0.5333333, 1))

ggplot(Ds_emm,
       aes(x = factor_loading, 
           y = response, 
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
  labs(y = TeX("$D_s(\\mathbf{R}_{Sm}, \\mathbf{R}_{Pop})$"),
       x = "Factor Loading",
       color = "Smoothing Method",
       shape = "Smoothing Method",
       linetype = "Smoothing Method")

ggsave(filename = "RpopRsm_fitted_vals.png",
       path = "Text/figs",
       plot = last_plot(),
       dpi = 320,
       width = 8,
       height = 8,
       units = "in",
       scale = 1)
