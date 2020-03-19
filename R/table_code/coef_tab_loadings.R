# Generate loading RMSE coefficient table
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

loading_mod <- readRDS(paste0(project_dir, "/Data", "/loading_model.RDS"))
loading_imputed_mod <- readRDS(paste0(project_dir, "/Data", "/loading_imputed_model.RDS"))

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
  "fa_methodfaml" = "Factor Extraction (ML)",
  "fa_methodfapa" = "Factor Extraction (PA)",
  "scale(subjects_per_item):scale(items_per_factor)" = "Subjects/Item $\\times$ Items/Factor",
  "scale(subjects_per_item):scale(factors)" = "Subjects/Item $\\times$ Factors",
  "scale(subjects_per_item):scale(factor_loading)" = "Subjects/Item $\\times$ Factor Loading",
  "scale(subjects_per_item):scale(model_error)" = "Subjects/Item $\\times$ Model Error",
  "scale(subjects_per_item):smoothing_methodBY" = "Subjects/Item $\\times$ Smoothing Method (BY)",
  "scale(subjects_per_item):smoothing_methodKB" = "Subjects/Item $\\times$ Smoothing Method (KB)",
  "scale(subjects_per_item):smoothing_methodNone" = "Subjects/Item $\\times$ Smoothing Method (None)",
  "scale(subjects_per_item):fa_methodfaml" = "Subjects/Item $\\times$ Factor Extraction (ML)",
  "scale(subjects_per_item):fa_methodfapa" = "Subjects/Item $\\times$ Factor Extraction (PA)",
  "scale(items_per_factor):scale(factors)" = "Items/Factor $\\times$ Factors",
  "scale(items_per_factor):scale(factor_loading)" = "Items/Factor $\\times$ Factor Loading",
  "scale(items_per_factor):scale(model_error)" = "Items/Factor $\\times$ Model Error",
  "scale(items_per_factor):smoothing_methodBY" = "Items/Factor $\\times$ Smoothing Method (BY)",
  "scale(items_per_factor):smoothing_methodKB" = "Items/Factor $\\times$ Smoothing Method (KB)",
  "scale(items_per_factor):smoothing_methodNone" = "Items/Factor $\\times$ Smoothing Method (None)",
  "scale(items_per_factor):fa_methodfaml" = "Items/Factor $\\times$ Factor Extraction (ML)",
  "scale(items_per_factor):fa_methodfapa" = "Items/Factor $\\times$ Factor Extraction (PA)",
  "scale(factors):scale(factor_loading)" = "Factors $\\times$ Factor Loading",
  "scale(factors):scale(model_error)" = "Factors $\\times$ Model Error",
  "scale(factors):smoothing_methodBY" = "Factors $\\times$ Smoothing Method (BY)",
  "scale(factors):smoothing_methodKB" = "Factors $\\times$ Smoothing Method (KB)",
  "scale(factors):smoothing_methodNone" = "Factors $\\times$ Smoothing Method (None)",
  "scale(factors):fa_methodfaml" = "Factors $\\times$ Factor Extraction (ML)",
  "scale(factors):fa_methodfapa" = "Factors $\\times$ Factor Extraction (PA)",
  "scale(factor_loading):scale(model_error)" = "Factor Loading $\\times$ Model Error",
  "scale(factor_loading):smoothing_methodBY" = "Factor Loading $\\times$ Smoothing Method (BY)",
  "scale(factor_loading):smoothing_methodKB" = "Factor Loading $\\times$ Smoothing Method (KB)",
  "scale(factor_loading):smoothing_methodNone" = "Factor Loading $\\times$ Smoothing Method (None)",
  "scale(factor_loading):fa_methodfaml" = "Factor Loading $\\times$ Factor Extraction (ML)",
  "scale(factor_loading):fa_methodfapa" = "Factor Loading $\\times$ Factor Extraction (PA)",
  "scale(model_error):smoothing_methodBY" = "Model Error $\\times$ Smoothing Method (BY)",
  "scale(model_error):smoothing_methodKB" = "Model Error $\\times$ Smoothing Method (KB)",
  "scale(model_error):smoothing_methodNone" = "Model Error $\\times$ Smoothing Method (None)",
  "scale(model_error):fa_methodfaml" = "Model Error $\\times$ Factor Extraction (ML)",
  "scale(model_error):fa_methodfapa" = "Model Error $\\times$ Factor Extraction (PA)",
  "smoothing_methodBY:fa_methodfaml" = "Smoothing Method (BY) $\\times$ Factor Extraction (ML)",
  "smoothing_methodKB:fa_methodfaml" = "Smoothing Method (KB) $\\times$ Factor Extraction (ML)",
  "smoothing_methodNone:fa_methodfaml" = "Smoothing Method (None) $\\times$ Factor Extraction (ML)",
  "smoothing_methodBY:fa_methodfapa" = "Smoothing Method (BY) $\\times$ Factor Extraction (PA)",
  "smoothing_methodKB:fa_methodfapa" = "Smoothing Method (KB) $\\times$ Factor Extraction (PA)",
  "smoothing_methodNone:fa_methodfapa" = "Smoothing Method (None) $\\times$ Factor Extraction (PA)"
)

loading_coef_tab <- texreg::texreg(
  l = list(loading_mod, extract.mira(loading_imputed_mod)),
  custom.coef.names = coef_names,
  custom.model.names = c("Non-imputed", "Imputed (Pooled)"),
  digits = 4,
  single.row = TRUE,
  stars = 0,
  longtable = TRUE,
  use.packages = FALSE,
  caption.above = TRUE,
  caption = "Coefficient estimates and standard errors for the linear mixed effects model using $\\log[\\RMSE]$ as the dependent variable and estimating a random intercept for each NPD correlation matrix.",
  label = "tab:loading-mod-summary"
)

writeLines(loading_coef_tab, paste0(project_dir, "/Text", "/tabs", "/loading_coef_tab.txt"))