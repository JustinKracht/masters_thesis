# Generate Ds(Rpop, Rsm) coefficient table
# extract() method; allows texreg() to get coefficient estimates and standard
# errors from multiple imputed lmer models.
extract.mira <- function(model) {
  gof           <- numeric()
  gof.names     <- character()
  gof.decimal   <- logical()
  
  model_ex      <- model$analyses[[1]]
  s             <- summary(pool(model))
  s_ex          <- summary(model$analyses[[1]])
  coef_names    <- rownames(s)
  betas         <- s$estimate
  se            <- s$std.error
  pval          <- s$p.value
  
  n           <- dim(model.frame(model_ex))[1]
  gof         <- c(gof, n)
  gof.names   <- c(gof.names, "Num.\ obs.")
  gof.decimal <- c(gof.decimal, FALSE)
  
  grps        <- s_ex$ngrps
  grp.names   <- names(grps)
  grp.names   <- paste("Num.\ groups:", grp.names)
  gof         <- c(gof, grps)
  gof.names   <- c(gof.names, grp.names)
  gof.decimal <- c(gof.decimal, rep(FALSE, length(grps)))
  
  tr <- createTexreg(
    coef.names = coef_names,
    coef = betas,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  
  return(tr)
}

# Load fitted models
RpopRsm_mod <- readRDS(file = paste0(project_dir, "/Data", "/RpopRsm_model.RDS"))
RpopRsm_imputed_model <- readRDS(
  file = paste0(project_dir, "/Data", "/RpopRsm_imputed_model.RDS")
)

# Set pretty coef names
coef_names <- c(
  "(Intercept)" = "Constant",
  "scale(subjects_per_item)" = "Subjects/Item",
  "scale(items_per_factor)" = "Items/Factor",
  "scale(factors)" = "Factors",
  "scale(factor_loading)" = "Factor Loading",
  "scale(model_error)" = "Model Error",
  "smoothing_methodBY" = "Smoothing Method (BY)",
  "smoothing_methodKB" = "Smoothing Method (KB)",
  "smoothing_methodNone" = "Smoothing Method (None)",
  "scale(subjects_per_item):scale(items_per_factor)" = "Subjects/Item $\\times$ Items/Factor",
  "scale(subjects_per_item):scale(factors)" = "Subjects/Item $\\times$ Factors",
  "scale(subjects_per_item):scale(factor_loading)" = "Subjects/Item $\\times$ Factor Loading",
  "scale(subjects_per_item):scale(model_error)" = "Subjects/Item $\\times$ Model Error",
  "scale(subjects_per_item):smoothing_methodBY" = "Subjects/Item $\\times$ Smoothing Method (BY)",
  "scale(subjects_per_item):smoothing_methodKB" = "Subjects/Item $\\times$ Smoothing Method (KB)",
  "scale(subjects_per_item):smoothing_methodNone" = "Subjects/Item $\\times$ Smoothing Method (None)",
  "scale(items_per_factor):scale(factors)" = "Items/Factor $\\times$ Factors",
  "scale(items_per_factor):scale(factor_loading)" = "Items/Factor $\\times$ Factor Loading",
  "scale(items_per_factor):scale(model_error)" = "Items/Factor $\\times$ Model Error",
  "scale(items_per_factor):smoothing_methodBY" = "Items/Factor $\\times$ Smoothing Method (BY)",
  "scale(items_per_factor):smoothing_methodKB" = "Items/Factor $\\times$ Smoothing Method (KB)",
  "scale(items_per_factor):smoothing_methodNone" = "Items/Factor $\\times$ Smoothing Method (None)",
  "scale(factors):scale(factor_loading)" = "Factors $\\times$ Factor Loading",
  "scale(factors):scale(model_error)" = "Factors $\\times$ Model Error",
  "scale(factors):smoothing_methodBY" = "Factors $\\times$ Smoothing Method (BY)",
  "scale(factors):smoothing_methodKB" = "Factors $\\times$ Smoothing Method (KB)",
  "scale(factors):smoothing_methodNone" = "Factors $\\times$ Smoothing Method (None)",
  "scale(factor_loading):scale(model_error)" = "Factor Loading $\\times$ Model Error",
  "scale(factor_loading):smoothing_methodBY" = "Factor Loading $\\times$ Smoothing Method (BY)",
  "scale(factor_loading):smoothing_methodKB" = "Factor Loading $\\times$ Smoothing Method (KB)",
  "scale(factor_loading):smoothing_methodNone" = "Factor Loading $\\times$ Smoothing Method (None)",
  "scale(model_error):smoothing_methodBY" = "Model Error $\\times$ Smoothing Method (BY)",
  "scale(model_error):smoothing_methodKB" = "Model Error $\\times$ Smoothing Method (KB)",
  "scale(model_error):smoothing_methodNone" = "Model Error $\\times$ Smoothing Method (None)"
)

RpopRsm_coef_tab <- texreg::texreg(
  l = list(RpopRsm_mod, extract.mira(RpopRsm_imputed_model)),
  custom.coef.names = coef_names,
  custom.model.names = c("Non-imputed", "Imputed (Pooled)"),
  custom.note = "",
  digits = 4,
  stars = 0,
  longtable = TRUE,
  use.packages = FALSE,
  single.row = TRUE,
  caption.above = TRUE,
  caption = "Coefficient estimates and standard errors for the linear mixed effects model using $\\log[\\mathrm{D}_{\\mathrm{s}}(\\Rsm, \\Rpop)]$ as the dependent variable and estimating a random intercept for each NPD correlation matrix.",
  label = "tab:distance-mod-summary")

# Right-align coefficient columns
RpopRsm_coef_tab <- stringr::str_replace(RpopRsm_coef_tab, 
                                         pattern = "\\{l c c \\}", 
                                         replacement = "\\{l r r \\}")

writeLines(RpopRsm_coef_tab, 
           paste0(project_dir, "/Text", "/tabs", "/RpopRsm_coef_tab.txt"))
