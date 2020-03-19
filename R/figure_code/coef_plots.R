# Create and save coefficient plots for RMSE(loading) and Ds(Rpop, Rsm) models
pacman::p_load(coefplot,
               patchwork,
               broom,
               tidyverse,
               mice,
               merTools,
               broom.mixed)

# Load the models
loading_mod_im <- readRDS("Data/loading_imputed_model.RDS")
loading_mod    <- readRDS("Data/loading_model.RDS")
RpopRsm_mod_im <- readRDS("Data/RpopRsm_imputed_model.RDS")
RpopRsm_mod    <- readRDS("Data/RpopRsm_model.RDS")

# Nicely formatted coefficient names
coef_names <- c(
  "(Intercept)" = "Constant",
  "subjects_per_item" = "Subjects/Item",
  "items_per_factor" = "Items/Factor",
  "factors" = "Factors",
  "factor_loading" = "Factor Loading",
  "model_error" = "Model Error",
  "smoothing_methodBY" = "Smoothing Method (BY)",
  "smoothing_methodKB" = "Smoothing Method (KB)",
  "smoothing_methodNone" = "Smoothing Method (None)",
  "fa_methodfaml" = "Factor Extraction (ML)",
  "fa_methodfapa" = "Factor Extraction (PA)",
  "subjects_per_item:items_per_factor" = "Subjects/Item x Items/Factor",
  "subjects_per_item:factors" = "Subjects/Item x Factors",
  "subjects_per_item:factor_loading" = "Subjects/Item x Factor Loading",
  "subjects_per_item:model_error" = "Subjects/Item x Model Error",
  "subjects_per_item:smoothing_methodBY" = "Subjects/Item x Smoothing Method (BY)",
  "subjects_per_item:smoothing_methodKB" = "Subjects/Item x Smoothing Method (KB)",
  "subjects_per_item:smoothing_methodNone" = "Subjects/Item x Smoothing Method (None)",
  "subjects_per_item:fa_methodfaml" = "Subjects/Item x Factor Extraction (ML)",
  "subjects_per_item:fa_methodfapa" = "Subjects/Item x Factor Extraction (PA)",
  "items_per_factor:factors" = "Items/Factor x Factors",
  "items_per_factor:factor_loading" = "Items/Factor x Factor Loading",
  "items_per_factor:model_error" = "Items/Factor x Model Error",
  "items_per_factor:smoothing_methodBY" = "Items/Factor x Smoothing Method (BY)",
  "items_per_factor:smoothing_methodKB" = "Items/Factor x Smoothing Method (KB)",
  "items_per_factor:smoothing_methodNone" = "Items/Factor x Smoothing Method (None)",
  "items_per_factor:fa_methodfaml" = "Items/Factor x Factor Extraction (ML)",
  "items_per_factor:fa_methodfapa" = "Items/Factor x Factor Extraction (PA)",
  "factors:factor_loading" = "Factors x Factor Loading",
  "factors:model_error" = "Factors x Model Error",
  "factors:smoothing_methodBY" = "Factors x Smoothing Method (BY)",
  "factors:smoothing_methodKB" = "Factors x Smoothing Method (KB)",
  "factors:smoothing_methodNone" = "Factors x Smoothing Method (None)",
  "factors:fa_methodfaml" = "Factors x Factor Extraction (ML)",
  "factors:fa_methodfapa" = "Factors x Factor Extraction (PA)",
  "factor_loading:model_error" = "Factor Loading x Model Error",
  "factor_loading:smoothing_methodBY" = "Factor Loading x Smoothing Method (BY)",
  "factor_loading:smoothing_methodKB" = "Factor Loading x Smoothing Method (KB)",
  "factor_loading:smoothing_methodNone" = "Factor Loading x Smoothing Method (None)",
  "factor_loading:fa_methodfaml" = "Factor Loading x Factor Extraction (ML)",
  "factor_loading:fa_methodfapa" = "Factor Loading x Factor Extraction (PA)",
  "model_error:smoothing_methodBY" = "Model Error x Smoothing Method (BY)",
  "model_error:smoothing_methodKB" = "Model Error x Smoothing Method (KB)",
  "model_error:smoothing_methodNone" = "Model Error x Smoothing Method (None)",
  "model_error:fa_methodfaml" = "Model Error x Factor Extraction (ML)",
  "model_error:fa_methodfapa" = "Model Error x Factor Extraction (PA)",
  "smoothing_methodBY:fa_methodfaml" = "Smoothing Method (BY) x Factor Extraction (ML)",
  "smoothing_methodKB:fa_methodfaml" = "Smoothing Method (KB) x Factor Extraction (ML)",
  "smoothing_methodNone:fa_methodfaml" = "Smoothing Method (None) x Factor Extraction (ML)",
  "smoothing_methodBY:fa_methodfapa" = "Smoothing Method (BY) x Factor Extraction (PA)",
  "smoothing_methodKB:fa_methodfapa" = "Smoothing Method (KB) x Factor Extraction (PA)",
  "smoothing_methodNone:fa_methodfapa" = "Smoothing Method (None) x Factor Extraction (PA)"
)

# Extract model summaries
## Imputed model
loading_mod_im_summary <- summary(pool(loading_mod_im))
loading_mod_im_summary <- tibble::rownames_to_column(loading_mod_im_summary,
                                                     var = "term")
loading_mod_im_summary <- loading_mod_im_summary %>%
  mutate(missing_method = "Imputation") %>%
  select(term, estimate, "std.error", missing_method)
# Recode coefficient names to nicely formatted ones
loading_mod_im_summary$term <- coef_names[loading_mod_im_summary$term]

## Missing data omitted model
loading_mod_summary <- tidy(loading_mod)
loading_mod_summary <- loading_mod_summary %>%
  filter(effect == "fixed") %>%
  mutate(missing_method = "Listwise deletion") %>%
  select(term, estimate, 'std.error', missing_method)
# Recode coefficient names to nicely formatted ones
loading_mod_summary$term <- coef_names[loading_mod_summary$term]

# Join together the imputed and non-imputed coefficient tables
loading_mod_coefs <- rbind(loading_mod_im_summary,
                   loading_mod_summary)

# Plot coeficients (back-transformed)
p1 <- loading_mod_coefs %>%
  ggplot(aes(y = exp(estimate), x = fct_reorder(term, estimate, max), 
             color = missing_method, shape = missing_method)) +
  geom_point() +
  # geom_linerange(aes(ymin = exp(estimate - 2 * std.error), 
  #                ymax = exp(estimate + 2 * std.error))) +
  scale_color_discrete(name = "Missing data method") +
  scale_shape_discrete(name = "Missing data method") +
  labs(title = "Model coefficient estimates",
       y = latex2exp::TeX("$\\exp{(\\hat{\\beta})}$"),
       x = "") +
  coord_flip() +
  geom_hline(yintercept = 1,
             lty = 2,
             col = "grey50") +
  theme_minimal()

ggsave("loadings_coefplot.png",
       plot = p2,
       device = "png",
       path = "Text/figs/",
       dpi = "retina",
       width = 8,
       height = 7)

# Do the same for the RMSE_Rsm_Rpop models
# Nicely formatted coefficient names
coef_names <- c(
  "(Intercept)" = "Constant",
  "subjects_per_item" = "Subjects/Item",
  "items_per_factor" = "Items/Factor",
  "factors" = "Factors",
  "factor_loading" = "Factor Loading",
  "model_error" = "Model Error",
  "smoothing_methodBY" = "Smoothing Method (BY)",
  "smoothing_methodKB" = "Smoothing Method (KB)",
  "smoothing_methodNone" = "Smoothing Method (None)",
  "subjects_per_item:items_per_factor" = "Subjects/Item x Items/Factor",
  "subjects_per_item:factors" = "Subjects/Item x Factors",
  "subjects_per_item:factor_loading" = "Subjects/Item x Factor Loading",
  "subjects_per_item:model_error" = "Subjects/Item x Model Error",
  "subjects_per_item:smoothing_methodBY" = "Subjects/Item x Smoothing Method (BY)",
  "subjects_per_item:smoothing_methodKB" = "Subjects/Item x Smoothing Method (KB)",
  "subjects_per_item:smoothing_methodNone" = "Subjects/Item x Smoothing Method (None)",
  "items_per_factor:factors" = "Items/Factor x Factors",
  "items_per_factor:factor_loading" = "Items/Factor x Factor Loading",
  "items_per_factor:model_error" = "Items/Factor x Model Error",
  "items_per_factor:smoothing_methodBY" = "Items/Factor x Smoothing Method (BY)",
  "items_per_factor:smoothing_methodKB" = "Items/Factor x Smoothing Method (KB)",
  "items_per_factor:smoothing_methodNone" = "Items/Factor x Smoothing Method (None)",
  "factors:factor_loading" = "Factors x Factor Loading",
  "factors:model_error" = "Factors x Model Error",
  "factors:smoothing_methodBY" = "Factors x Smoothing Method (BY)",
  "factors:smoothing_methodKB" = "Factors x Smoothing Method (KB)",
  "factors:smoothing_methodNone" = "Factors x Smoothing Method (None)",
  "factor_loading:model_error" = "Factor Loading x Model Error",
  "factor_loading:smoothing_methodBY" = "Factor Loading x Smoothing Method (BY)",
  "factor_loading:smoothing_methodKB" = "Factor Loading x Smoothing Method (KB)",
  "factor_loading:smoothing_methodNone" = "Factor Loading x Smoothing Method (None)",
  "model_error:smoothing_methodBY" = "Model Error x Smoothing Method (BY)",
  "model_error:smoothing_methodKB" = "Model Error x Smoothing Method (KB)",
  "model_error:smoothing_methodNone" = "Model Error x Smoothing Method (None)"
)

# Extract model summaries
## Imputed model
RpopRsm_mod_im_summary <- summary(pool(RpopRsm_mod_im))
RpopRsm_mod_im_summary <- tibble::rownames_to_column(RpopRsm_mod_im_summary,
                                                     var = "term")
RpopRsm_mod_im_summary <- RpopRsm_mod_im_summary %>%
  mutate(missing_method = "Imputation") %>%
  dplyr::select(term, estimate, std.error, missing_method)
# Recode coefficient names to nicely formatted ones
RpopRsm_mod_im_summary$term <- coef_names[RpopRsm_mod_im_summary$term]

## Missing data omitted model
RpopRsm_mod_summary <- tidy(RpopRsm_mod)
RpopRsm_mod_summary <- RpopRsm_mod_summary %>%
  filter(effect == "fixed") %>%
  mutate(missing_method = "Listwise deletion") %>%
  dplyr::select(term, estimate, std.error, missing_method)
# Recode coefficient names to nicely formatted ones
RpopRsm_mod_summary$term <- coef_names[RpopRsm_mod_summary$term]

# Join together the imputed and non-imputed coefficient tables
RpopRsm_mod_coefs <- rbind(RpopRsm_mod_im_summary,
                           RpopRsm_mod_summary)

# Plot coefficients (back-transformed)
p2 <- RpopRsm_mod_coefs %>%
  ggplot(aes(y = exp(estimate), x = fct_reorder(term, estimate, max), 
             color = missing_method, shape = missing_method)) +
  geom_point() +
  scale_color_discrete(name = "Missing data method") +
  scale_shape_discrete(name = "Missing data method") +
  labs(title = "Model coefficient estimates",
       y = latex2exp::TeX("$\\exp{(\\hat{\\beta})}$"),
       x = "") +
  coord_flip() +
  geom_hline(yintercept = 1,
             lty = 2,
             col = "grey50") +
  theme_minimal()
ggsave("RpopRsm_coefplot.png",
       plot = p2,
       device = "png",
       path = "Text/figs/",
       dpi = "retina",
       width = 8,
       height = 7)