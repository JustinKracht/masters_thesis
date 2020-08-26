# Create and save coefficient plots for loading bias and correlation bias models
pacman::p_load(coefplot,
               patchwork,
               broom,
               tidyverse,
               merTools,
               broom.mixed,
               here)

# Load the fitted models
loading_bias_mod <- readRDS(here("Data", "loading_bias_mod_linear.RDS"))
loading_bias_mod_poly <- readRDS(here("Data", "loading_bias_mod_poly.RDS"))

coef_names <- c(
  "(Intercept)" = "Constant",
  "subjects_per_item" = "Subjects/Item",
  "items_per_factor" = "Items/Factor",
  "factors" = "Factors",
  "factor_loading" = "Factor Loading",
  "model_error" = "Model Error",
  "smoothing_methodAPA" = "Smoothing Method (APA)",
  "smoothing_methodBY" = "Smoothing Method (BY)",
  "smoothing_methodKB" = "Smoothing Method (KB)",
  "fa_methodfaml" = "Extraction Method (ML)",
  "fa_methodfapa" = "Extraction Method (PA)",
  "I(subjects_per_item^2)" = "Subjects/Item^2",
  "I(factor_loading^2)" = "Factor Loading^2",
  "I(factors^2)" = "Factors^2",
  "I(model_error^2)" = "Model Error^2",
  "subjects_per_item:items_per_factor" = "Subjects/Item x Items/Factor",
  "subjects_per_item:factors" = "Subjects/Item x Factors",
  "subjects_per_item:factor_loading" = "Subjects/Item x Factor Loading",
  "subjects_per_item:model_error" = "Subjects/Item x Model Error",
  "subjects_per_item:smoothing_methodAPA" = "Subjects/Item x Smoothing Method (APA)",
  "subjects_per_item:smoothing_methodBY" = "Subjects/Item x Smoothing Method (BY)",
  "subjects_per_item:smoothing_methodKB" = "Subjects/Item x Smoothing Method (KB)",
  "subjects_per_item:fa_methodfaml" = "Subjects/Item x Extraction Method (ML)",
  "subjects_per_item:fa_methodfapa" = "Subjects/Item x Extraction Method (PA)",
  "subjects_per_item:I(factor_loading^2)" = "Subjects/Item x Factor Loading^2",
  "subjects_per_item:I(factors^2)" = "Subjects/Item x Factors^2",
  "subjects_per_item:I(model_error^2)" = "Subjects/Item x Model Error^2",
  "items_per_factor:factors" = "Items/Factor x Factors",
  "items_per_factor:factor_loading" = "Items/Factor x Factor Loading",
  "items_per_factor:model_error" = "Items/Factor x Model Error",
  "items_per_factor:smoothing_methodAPA" = "Items/Factor x Smoothing Method (APA)",
  "items_per_factor:smoothing_methodBY" = "Items/Factor x Smoothing Method (BY)",
  "items_per_factor:smoothing_methodKB" = "Items/Factor x Smoothing Method (KB)",
  "items_per_factor:fa_methodfaml" = "Items/Factor x Extraction Method (ML)",
  "items_per_factor:fa_methodfapa" = "Items/Factor x Extraction Method (PA)",
  "items_per_factor:I(subjects_per_item^2)" = "Items/Factor x Subjects/Item^2",
  "items_per_factor:I(factor_loading^2)" = "Items/Factor x Factor Loading^2",
  "items_per_factor:I(factors^2)" = "Items/Factor x Factors^2",
  "items_per_factor:I(model_error^2)" = "Items/Factor x Model Error^2",
  "factors:factor_loading" = "Factors x Factor Loading",
  "factors:model_error" = "Factors x Model Error",
  "factors:smoothing_methodAPA" = "Factors x Smoothing Method (APA)",
  "factors:smoothing_methodBY" = "Factors x Smoothing Method (BY)",
  "factors:smoothing_methodKB" = "Factors x Smoothing Method (KB)",
  "factors:fa_methodfaml" = "Factors x Extraction Method (ML)",
  "factors:fa_methodfapa" = "Factors x Extraction Method (PA)",
  "factors:I(subjects_per_item^2)" = "Factors x Subjects/Item^2",
  "factors:I(factor_loading^2)" = "Factors x Factor Loading^2",
  "factors:I(factors^2)" = "Factors x Factors^2",
  "factors:I(model_error^2)" = "Factors x Model Error^2",
  "factor_loading:model_error" = "Factor Loading x Model Error",
  "factor_loading:smoothing_methodAPA" = "Factor Loading x Smoothing Method (APA)",
  "factor_loading:smoothing_methodBY" = "Factor Loading x Smoothing Method (BY)",
  "factor_loading:smoothing_methodKB" = "Factor Loading x Smoothing Method (KB)",
  "factor_loading:fa_methodfaml" = "Factor Loading x Extraction Method (ML)",
  "factor_loading:fa_methodfapa" = "Factor Loading x Extraction Method (PA)",
  "factor_loading:I(subjects_per_item^2)" = "Factor Loading x Subjects/Item^2",
  "factor_loading:I(factors^2)" = "Factor Loading x Factors^2",
  "factor_loading:I(model_error^2)" = "Factor Loading x Model Error^2",
  "model_error:smoothing_methodAPA" = "Model Error x Smoothing Method (APA)",
  "model_error:smoothing_methodBY" = "Model Error x Smoothing Method (BY)",
  "model_error:smoothing_methodKB" = "Model Error x Smoothing Method (KB)",
  "model_error:fa_methodfaml" = "Model Error x Extraction Method (ML)",
  "model_error:fa_methodfapa" = "Model Error x Extraction Method (PA)",
  "model_error:I(subjects_per_item^2)" = "Model Error x Subjects/Item^2",
  "model_error:I(factor_loading^2)" = "Model Error x Factor Loading^2",
  "model_error:I(factors^2)" = "Model Error x Factors^2",
  "smoothing_methodAPA:fa_methodfaml" = "Smoothing Method (APA) x Extraction Method (ML)",
  "smoothing_methodBY:fa_methodfaml" = "Smoothing Method (BY) x Extraction Method (ML)",
  "smoothing_methodKB:fa_methodfaml" = "Smoothing Method (KB) x Extraction Method (ML)",
  "smoothing_methodAPA:fa_methodfapa" = "Smoothing Method (APA) x Extraction Method (PA)",
  "smoothing_methodBY:fa_methodfapa" = "Smoothing Method (BY) x Extraction Method (PA)",
  "smoothing_methodKB:fa_methodfapa" = "Smoothing Method (KB) x Extraction Method (PA)",
  "smoothing_methodAPA:I(subjects_per_item^2)" = "Smoothing Method (APA) x Subjects/Item^2",
  "smoothing_methodBY:I(subjects_per_item^2)" = "Smoothing Method (BY) x Subjects/Item^2",
  "smoothing_methodKB:I(subjects_per_item^2)" = "Smoothing Method (KB) x Subjects/Item^2",
  "smoothing_methodAPA:I(factor_loading^2)" = "Smoothing Method (APA) x Factor Loading^2",
  "smoothing_methodBY:I(factor_loading^2)" = "Smoothing Method (BY) x Factor Loading^2",
  "smoothing_methodKB:I(factor_loading^2)" = "Smoothing Method (KB) x Factor Loading^2",
  "smoothing_methodAPA:I(factors^2)" = "Smoothing Method (APA) x Factors^2",
  "smoothing_methodBY:I(factors^2)" = "Smoothing Method (BY) x Factors^2",
  "smoothing_methodKB:I(factors^2)" = "Smoothing Method (KB) x Factors^2",
  "smoothing_methodAPA:I(model_error^2)" = "Smoothing Method (APA) x Model Error^2",
  "smoothing_methodBY:I(model_error^2)" = "Smoothing Method (BY) x Model Error^2",
  "smoothing_methodKB:I(model_error^2)" = "Smoothing Method (KB) x Model Error^2",
  "fa_methodfaml:I(subjects_per_item^2)" = "Extraction Method (ML) x Subjects/Item^2",
  "fa_methodfapa:I(subjects_per_item^2)" = "Extraction Method (PA) x Subjects/Item^2",
  "fa_methodfaml:I(factor_loading^2)" = "Extraction Method (ML) x Factor Loading^2",
  "fa_methodfapa:I(factor_loading^2)" = "Extraction Method (PA) x Factor Loading^2",
  "fa_methodfaml:I(factors^2)" = "Extraction Method (ML) x Factors^2",
  "fa_methodfapa:I(factors^2)" = "Extraction Method (PA) x Factors^2",
  "fa_methodfaml:I(model_error^2)" = "Extraction Method (ML) x Model Error^2",
  "fa_methodfapa:I(model_error^2)" = "Extraction Method (PA) x Model Error^2",
  "I(subjects_per_item^2):I(factor_loading^2)" = "Subjects/Item^2 x Factor Loading^2",
  "I(subjects_per_item^2):I(factors^2)" = "Subjects/Item^2 x Factors^2",
  "I(subjects_per_item^2):I(model_error^2)" = "Subjects/Item^2 x Model Error^2",
  "I(factor_loading^2):I(factors^2)" = "Factor Loading^2 x Factors^2",
  "I(factor_loading^2):I(model_error^2)" = "Factor Loading^2 x Model Error^2",
  "I(factors^2):I(model_error^2)" = "Factors^2 x Model Error^2"
)

# Loading bias coefplot ----
# Extract model summaries
# Linear model
loading_bias_mod_linear_summary <- tidy(loading_bias_mod,
                                        conf.int = TRUE,
                                        conf.level = 0.99)
loading_bias_mod_linear_summary <- loading_bias_mod_linear_summary %>%
  filter(effect == "fixed") %>%
  dplyr::select(term, estimate, conf.low, conf.high) %>%
  mutate(model_type = "linear")
# Replace term names with nice ones from coef_names
loading_bias_mod_linear_summary$term <- coef_names[loading_bias_mod_linear_summary$term]

# Polynomial model
loading_bias_mod_poly_summary <- tidy(loading_bias_mod_poly,
                                      conf.int = TRUE,
                                      conf.level = 0.99)
loading_bias_mod_poly_summary <- loading_bias_mod_poly_summary %>%
  filter(effect == "fixed") %>%
  dplyr::select(term, estimate, conf.low, conf.high) %>%
  mutate(model_type = "polynomial")
# Replace term names with nice ones from coef_names
loading_bias_mod_poly_summary$term <- coef_names[loading_bias_mod_poly_summary$term]

# Join the model summary tables
loading_bias_mod_summary <- full_join(loading_bias_mod_linear_summary,
                                      loading_bias_mod_poly_summary)

# Remove the loading mod objects to free up memory
remove(loading_bias_mod, loading_bias_mod_poly)

# Plot coefficients (back-transformed)
p1_data <- loading_bias_mod_summary
p1_data <- filter(p1_data,
                  model_type == "polynomial",
                  estimate >= 0.01 | estimate <= -0.01,
                  term != "Constant")
p1_data$term <- fct_reorder(.f = p1_data$term,
                            .x = p1_data$estimate,
                            .fun = median)

p1 <- ggplot(p1_data, aes(y = estimate, x = term)) +
  geom_point() +
  scale_x_discrete(label = latex2exp::TeX(levels(p1_data$term))) +
  geom_linerange(aes(ymax = conf.high, ymin = conf.low)) +
  labs(title = "Coefficient estimates",
       y = latex2exp::TeX("$\\hat{b}$"),
       x = "") +
  coord_flip() +
  geom_hline(yintercept = 0,
             lty = 2,
             col = "grey50") +
  theme_minimal()

ggsave("loading_bias_coefplot.png",
       plot = p1,
       device = "png",
       path = here("Text", "figs"),
       dpi = "retina",
       scale = .8,
       width = 11,
       height = 10)

# Correlation bias coefplot ----
# Load the fitted models
RpopRsm_bias_mod <- readRDS(here("Data", "RpopRsm_bias_mod_linear.RDS"))
RpopRsm_bias_mod_poly <- readRDS(here("Data", "RpopRsm_bias_mod_poly.RDS"))

# Linear model
RpopRsm_bias_mod_linear_summary <- tidy(RpopRsm_bias_mod,
                                        conf.int = TRUE,
                                        conf.level = 0.99)

RpopRsm_bias_mod_linear_summary <- RpopRsm_bias_mod_linear_summary %>%
  filter(effect == "fixed") %>%
  dplyr::select(term, estimate, conf.low, conf.high) %>%
  mutate(model_type = "linear")
# Replace term names with nice ones from coef_names
RpopRsm_bias_mod_linear_summary$term <- coef_names[RpopRsm_bias_mod_linear_summary$term]

# Polynomial model
RpopRsm_bias_mod_poly_summary <- tidy(RpopRsm_bias_mod_poly,
                                      conf.int = TRUE,
                                      conf.level = 0.99)
RpopRsm_bias_mod_poly_summary <- RpopRsm_bias_mod_poly_summary %>%
  filter(effect == "fixed") %>%
  dplyr::select(term, estimate, conf.low, conf.high) %>%
  mutate(model_type = "polynomial")
# Replace term names with nice ones from coef_names
RpopRsm_bias_mod_poly_summary$term <- coef_names[RpopRsm_bias_mod_poly_summary$term]

# Join the model summary tables
RpopRsm_bias_mod_summary <- full_join(RpopRsm_bias_mod_linear_summary,
                                      RpopRsm_bias_mod_poly_summary)

# Remove the loading mod objects to free up memory
remove(RpopRsm_bias_mod, RpopRsm_bias_mod_poly)

# Plot coefficients (back-transformed)
p2_data <- RpopRsm_bias_mod_summary %>%
  filter(model_type == "polynomial",
         estimate <= -0.01 | estimate >= 0.01,
         term != "Constant") %>%
  mutate(term = fct_reorder(term, estimate, median))

p2 <- ggplot(p2_data, aes(y = estimate, 
                          x = term)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  scale_x_discrete(label = TeX(levels(p2_data$term))) +
  labs(title = "Coefficient estimates",
       y = latex2exp::TeX("$\\hat{b}$"),
       x = "") +
  coord_flip() +
  geom_hline(yintercept = 0,
             lty = 2,
             col = "grey50") +
  theme_minimal()

ggsave("RpopRsm_bias_coefplot.png",
       plot = p2,
       device = "png",
       path = here("Text", "figs"),
       dpi = "retina",
       scale = .8,
       width = 10,
       height = 7)