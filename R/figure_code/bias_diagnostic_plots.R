# Load the models
RpopRsm_bias_mod_linear <- readRDS(here("Data", "RpopRsm_bias_mod_linear.RDS"))
RpopRsm_bias_mod_poly <- readRDS(here("Data", "RpopRsm_bias_mod_poly.RDS"))

# Linear model ----
resid_vals_linear <- resid(RpopRsm_bias_mod_linear, type = "response")
fitted_vals_linear <- fitted(RpopRsm_bias_mod_linear)
RpopRsm_diagnostic_linear <- data.frame(fitted_vals = fitted_vals_linear,
                                        resid_vals  = resid_vals_linear,
                                        model_type = "Linear Model")
ranef_vals_linear <- ranef(RpopRsm_bias_mod_linear, drop = TRUE, condVar = FALSE)$id

# Polynomial model ----
resid_vals_poly <- resid(RpopRsm_bias_mod_poly, type = "response")
fitted_vals_poly <- fitted(RpopRsm_bias_mod_poly)
RpopRsm_diagnostic_poly <- data.frame(fitted_vals = fitted_vals_poly,
                                      resid_vals  = resid_vals_poly,
                                      model_type = "Polynomial Model")
ranef_vals_poly <- ranef(RpopRsm_bias_mod_poly, drop = TRUE, condVar = FALSE)$id

# Combine linear and polynomial results
RpopRsm_diagnostic <- rbind(RpopRsm_diagnostic_linear,
                            RpopRsm_diagnostic_poly)

# Create data for residual and random effect QQ plots
# QQ plot data
RpopRsm_qq_linear <- data.frame(
  sample_quantile = RpopRsm_diagnostic_linear$resid_vals[order(RpopRsm_diagnostic_linear$resid_vals)],
  theoretical_quantile = qnorm(ppoints(length(RpopRsm_diagnostic_linear$resid_vals))),
  model_type = "Linear Model"
)

RpopRsm_qq_poly <- data.frame(
  sample_quantile = RpopRsm_diagnostic_poly$resid_vals[order(RpopRsm_diagnostic_poly$resid_vals)],
  theoretical_quantile = qnorm(ppoints(length(RpopRsm_diagnostic_poly$resid_vals))),
  model_type = "Polynomial Model"
)

RpopRsm_qq <- rbind(RpopRsm_qq_linear,
                    RpopRsm_qq_poly)
# Random effect data
RpopRsm_random_effects <- data.frame(
  random_effect = c(ranef_vals_linear[order(ranef_vals_linear)],
                    ranef_vals_poly[order(ranef_vals_poly)]),
  theoretical_quantile = c(qnorm(ppoints(length(ranef_vals_linear))),
                           qnorm(ppoints(length(ranef_vals_poly)))),
  model_type = rep(c("Linear Model",
                     "Polynomial Model"),
                   c(length(ranef_vals_linear),
                     length(ranef_vals_poly)))
)

# Create diagnostic plots
resid_vs_fitted <- ggplot(RpopRsm_diagnostic, 
                          aes(x = fitted_vals, y = resid_vals)) +
  geom_hex(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  facet_grid(. ~ model_type) +
  labs(x = "Fitted value",
       y = "Residual") +
  theme_minimal()

resid_qq <- ggplot(RpopRsm_qq, aes(x = theoretical_quantile,
                                   y = sample_quantile)) +
  geom_hex(bins = 75) +
  facet_grid(. ~ model_type) +
  scale_fill_continuous(type = "viridis") +
  labs(y = "Residual",
       x = "Theoretical Quantile") +
  theme_minimal()

ranef_qq <- ggplot(RpopRsm_random_effects,
                   aes(x = theoretical_quantile,
                       y = random_effect)) +
  geom_hex(bins = 75) +
  facet_grid(. ~ model_type) +
  scale_fill_continuous(type = "viridis") +
  labs(y = "Random Effect",
       x = "Theoretical Quantile") +
  theme_minimal()

# Combine and save plots ----
ggsave(filename = "RpopRsm_bias_resid_vs_fitted.png",
       path = here("Text", "figs"),
       plot = resid_vs_fitted,
       dpi = "retina",
       width = 12,
       height = 6,
       scale = .55)

ggsave(filename = "RpopRsm_bias_resid_qq.png",
       path = here("Text", "figs"),
       plot = resid_qq,
       dpi = "retina",
       width = 12,
       height = 6,
       scale = .55)

ggsave(filename = "RpopRsm_bias_ranef_qq.png",
       path = here("Text", "figs"),
       plot = ranef_qq,
       dpi = "retina",
       width = 12,
       height = 6,
       scale = .55)

## Loading bias plots ----
# Load the models
loading_bias_mod_linear <- readRDS(here("Data", "loading_bias_mod_linear.RDS"))
loading_bias_mod_poly <- readRDS(here("Data", "loading_bias_mod_poly.RDS"))

# Linear model ----
resid_vals_linear <- resid(loading_bias_mod_linear, type = "response")
fitted_vals_linear <- fitted(loading_bias_mod_linear)
loading_diagnostic_linear <- data.frame(fitted_vals = fitted_vals_linear,
                                        resid_vals  = resid_vals_linear,
                                        model_type = "Linear Model")
ranef_vals_linear <- ranef(loading_bias_mod_linear, drop = TRUE, condVar = FALSE)$id

# Polynomial model ----
resid_vals_poly <- resid(loading_bias_mod_poly, type = "response")
fitted_vals_poly <- fitted(loading_bias_mod_poly)
loading_diagnostic_poly <- data.frame(fitted_vals = fitted_vals_poly,
                                      resid_vals  = resid_vals_poly,
                                      model_type = "Polynomial Model")
ranef_vals_poly <- ranef(loading_bias_mod_poly, drop = TRUE, condVar = FALSE)$id

# Combine linear and polynomial results
loading_diagnostic <- rbind(loading_diagnostic_linear,
                            loading_diagnostic_poly)

# Create data for residual and random effect QQ plots
# QQ plot data
loading_qq_linear <- data.frame(
  sample_quantile = loading_diagnostic_linear$resid_vals[order(loading_diagnostic_linear$resid_vals)],
  theoretical_quantile = qnorm(ppoints(length(loading_diagnostic_linear$resid_vals))),
  model_type = "Linear Model"
)

loading_qq_poly <- data.frame(
  sample_quantile = loading_diagnostic_poly$resid_vals[order(loading_diagnostic_poly$resid_vals)],
  theoretical_quantile = qnorm(ppoints(length(loading_diagnostic_poly$resid_vals))),
  model_type = "Polynomial Model"
)

loading_qq <- rbind(loading_qq_linear,
                    loading_qq_poly)
# Random effect data
loading_random_effects <- data.frame(
  random_effect = c(ranef_vals_linear[order(ranef_vals_linear)],
                    ranef_vals_poly[order(ranef_vals_poly)]),
  theoretical_quantile = c(qnorm(ppoints(length(ranef_vals_linear))),
                           qnorm(ppoints(length(ranef_vals_poly)))),
  model_type = rep(c("Linear Model",
                     "Polynomial Model"),
                   c(length(ranef_vals_linear),
                     length(ranef_vals_poly)))
)

# Create diagnostic plots
resid_vs_fitted <- ggplot(loading_diagnostic, 
                          aes(x = fitted_vals, y = resid_vals)) +
  geom_hex(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  facet_grid(. ~ model_type) +
  labs(x = "Fitted value",
       y = "Residual") +
  theme_minimal()

resid_qq <- ggplot(loading_qq, aes(x = theoretical_quantile,
                                   y = sample_quantile)) +
  geom_hex(bins = 75) +
  facet_grid(. ~ model_type) +
  scale_fill_continuous(type = "viridis") +
  labs(y = "Residual",
       x = "Theoretical Quantile") +
  theme_minimal()

ranef_qq <- ggplot(loading_random_effects,
                   aes(x = theoretical_quantile,
                       y = random_effect)) +
  geom_hex(bins = 75) +
  facet_grid(. ~ model_type) +
  scale_fill_continuous(type = "viridis") +
  labs(y = "Random Effect",
       x = "Theoretical Quantile") +
  theme_minimal()

# Combine and save plots ----
ggsave(filename = "loading_bias_resid_vs_fitted.png",
       path = here("Text", "figs"),
       plot = resid_vs_fitted,
       dpi = "retina",
       width = 12,
       height = 6,
       scale = .55)

ggsave(filename = "loading_bias_resid_qq.png",
       path = here("Text", "figs"),
       plot = resid_qq,
       dpi = "retina",
       width = 12,
       height = 6,
       scale = .55)

ggsave(filename = "loading_bias_ranef_qq.png",
       path = here("Text", "figs"),
       plot = ranef_qq,
       dpi = "retina",
       width = 12,
       height = 6,
       scale = .55)