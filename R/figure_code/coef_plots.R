# Create and save coefficient plots for RMSE(loading) and Ds(Rpop, Rsm) models
pacman::p_load(coefplot,
               patchwork,
               broom,
               tidyverse,
               merTools,
               broom.mixed)

# Load the models
loading_mod    <- readRDS("Data/loading_model.RDS")
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
  "smoothing_methodAPA" = "Smoothing Method (APA)",
  "fa_methodfaml" = "Factor Extraction (ML)",
  "fa_methodfapa" = "Factor Extraction (PA)",
  "subjects_per_item:items_per_factor" = "Subjects/Item x Items/Factor",
  "subjects_per_item:factors" = "Subjects/Item x Factors",
  "subjects_per_item:factor_loading" = "Subjects/Item x Factor Loading",
  "subjects_per_item:model_error" = "Subjects/Item x Model Error",
  "subjects_per_item:smoothing_methodBY" = "Subjects/Item x Smoothing Method (BY)",
  "subjects_per_item:smoothing_methodKB" = "Subjects/Item x Smoothing Method (KB)",
  "subjects_per_item:smoothing_methodAPA" = "Subjects/Item x Smoothing Method (APA)",
  "subjects_per_item:fa_methodfaml" = "Subjects/Item x Factor Extraction (ML)",
  "subjects_per_item:fa_methodfapa" = "Subjects/Item x Factor Extraction (PA)",
  "items_per_factor:factors" = "Items/Factor x Factors",
  "items_per_factor:factor_loading" = "Items/Factor x Factor Loading",
  "items_per_factor:model_error" = "Items/Factor x Model Error",
  "items_per_factor:smoothing_methodBY" = "Items/Factor x Smoothing Method (BY)",
  "items_per_factor:smoothing_methodKB" = "Items/Factor x Smoothing Method (KB)",
  "items_per_factor:smoothing_methodAPA" = "Items/Factor x Smoothing Method (APA)",
  "items_per_factor:fa_methodfaml" = "Items/Factor x Factor Extraction (ML)",
  "items_per_factor:fa_methodfapa" = "Items/Factor x Factor Extraction (PA)",
  "factors:factor_loading" = "Factors x Factor Loading",
  "factors:model_error" = "Factors x Model Error",
  "factors:smoothing_methodBY" = "Factors x Smoothing Method (BY)",
  "factors:smoothing_methodKB" = "Factors x Smoothing Method (KB)",
  "factors:smoothing_methodAPA" = "Factors x Smoothing Method (APA)",
  "factors:fa_methodfaml" = "Factors x Factor Extraction (ML)",
  "factors:fa_methodfapa" = "Factors x Factor Extraction (PA)",
  "factor_loading:model_error" = "Factor Loading x Model Error",
  "factor_loading:smoothing_methodBY" = "Factor Loading x Smoothing Method (BY)",
  "factor_loading:smoothing_methodKB" = "Factor Loading x Smoothing Method (KB)",
  "factor_loading:smoothing_methodAPA" = "Factor Loading x Smoothing Method (APA)",
  "factor_loading:fa_methodfaml" = "Factor Loading x Factor Extraction (ML)",
  "factor_loading:fa_methodfapa" = "Factor Loading x Factor Extraction (PA)",
  "model_error:smoothing_methodBY" = "Model Error x Smoothing Method (BY)",
  "model_error:smoothing_methodKB" = "Model Error x Smoothing Method (KB)",
  "model_error:smoothing_methodAPA" = "Model Error x Smoothing Method (APA)",
  "model_error:fa_methodfaml" = "Model Error x Factor Extraction (ML)",
  "model_error:fa_methodfapa" = "Model Error x Factor Extraction (PA)",
  "smoothing_methodBY:fa_methodfaml" = "Smoothing Method (BY) x Factor Extraction (ML)",
  "smoothing_methodKB:fa_methodfaml" = "Smoothing Method (KB) x Factor Extraction (ML)",
  "smoothing_methodAPA:fa_methodfaml" = "Smoothing Method (APA) x Factor Extraction (ML)",
  "smoothing_methodBY:fa_methodfapa" = "Smoothing Method (BY) x Factor Extraction (PA)",
  "smoothing_methodKB:fa_methodfapa" = "Smoothing Method (KB) x Factor Extraction (PA)",
  "smoothing_methodAPA:fa_methodfapa" = "Smoothing Method (APA) x Factor Extraction (PA)"
)

# Extract model summaries
loading_mod_summary <- tidy(loading_mod)
loading_mod_summary <- loading_mod_summary %>%
  filter(effect == "fixed") %>%
  dplyr::select(term, estimate, 'std.error', effect)
# Recode coefficient names to nicely formatted ones
loading_mod_summary$term <- coef_names[loading_mod_summary$term]

# Plot coeficients (back-transformed)
p1 <- loading_mod_summary %>%
  ggplot(aes(y = exp(estimate), x = fct_reorder(term, estimate, max))) +
  geom_point() +
  labs(title = "Model coefficient estimates",
       y = latex2exp::TeX("$\\exp{(\\hat{\\beta})}$"),
       x = "") +
  coord_flip() +
  geom_hline(yintercept = 1,
             lty = 2,
             col = "grey50") +
  theme_minimal()

ggsave("loadings_coefplot.png",
       plot = p1,
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
  "smoothing_methodAPA" = "Smoothing Method (APA)",
  "subjects_per_item:items_per_factor" = "Subjects/Item x Items/Factor",
  "subjects_per_item:factors" = "Subjects/Item x Factors",
  "subjects_per_item:factor_loading" = "Subjects/Item x Factor Loading",
  "subjects_per_item:model_error" = "Subjects/Item x Model Error",
  "subjects_per_item:smoothing_methodBY" = "Subjects/Item x Smoothing Method (BY)",
  "subjects_per_item:smoothing_methodKB" = "Subjects/Item x Smoothing Method (KB)",
  "subjects_per_item:smoothing_methodAPA" = "Subjects/Item x Smoothing Method (APA)",
  "items_per_factor:factors" = "Items/Factor x Factors",
  "items_per_factor:factor_loading" = "Items/Factor x Factor Loading",
  "items_per_factor:model_error" = "Items/Factor x Model Error",
  "items_per_factor:smoothing_methodBY" = "Items/Factor x Smoothing Method (BY)",
  "items_per_factor:smoothing_methodKB" = "Items/Factor x Smoothing Method (KB)",
  "items_per_factor:smoothing_methodAPA" = "Items/Factor x Smoothing Method (APA)",
  "factors:factor_loading" = "Factors x Factor Loading",
  "factors:model_error" = "Factors x Model Error",
  "factors:smoothing_methodBY" = "Factors x Smoothing Method (BY)",
  "factors:smoothing_methodKB" = "Factors x Smoothing Method (KB)",
  "factors:smoothing_methodAPA" = "Factors x Smoothing Method (APA)",
  "factor_loading:model_error" = "Factor Loading x Model Error",
  "factor_loading:smoothing_methodBY" = "Factor Loading x Smoothing Method (BY)",
  "factor_loading:smoothing_methodKB" = "Factor Loading x Smoothing Method (KB)",
  "factor_loading:smoothing_methodAPA" = "Factor Loading x Smoothing Method (APA)",
  "model_error:smoothing_methodBY" = "Model Error x Smoothing Method (BY)",
  "model_error:smoothing_methodKB" = "Model Error x Smoothing Method (KB)",
  "model_error:smoothing_methodAPA" = "Model Error x Smoothing Method (APA)"
)

# Extract model summary
RpopRsm_mod_summary <- tidy(RpopRsm_mod)
RpopRsm_mod_summary <- RpopRsm_mod_summary %>%
  filter(effect == "fixed") %>%
  dplyr::select(term, estimate, std.error)
# Recode coefficient names to nicely formatted ones
RpopRsm_mod_summary$term <- coef_names[RpopRsm_mod_summary$term]

# Plot coefficients (back-transformed)
p2 <- RpopRsm_mod_summary %>%
  ggplot(aes(y = exp(estimate), x = fct_reorder(term, estimate, max))) +
  geom_point() +
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