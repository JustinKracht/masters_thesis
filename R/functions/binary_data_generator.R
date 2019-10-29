# binary_data_generator(): wrapper for simFA for convenient data generation
binary_data_generator <- function(reps = NULL,
                                  subjects_per_item = NULL,
                                  items_per_factor = NULL,
                                  factors = NULL,
                                  factor_loading = NULL,
                                  model_error = NULL,
                                  test_type = c("wide", "difficult"),
                                  diff_range = NULL) {
  
  items <- factors * items_per_factor  # Number of items in the data set
  sample_size <- subjects_per_item * items # Number of subjects in the data set
  # Generate classical item difficulty parameters
  if (test_type == "wide") {
    diff_params <- seq(from = diff_range$wide[1], 
                       to = diff_range$wide[2], 
                       length.out = items)
  } else if (test_type == "difficult") {
    diff_params <- seq(from = diff_range$difficult[1], 
                       to = diff_range$difficult[2], 
                       length.out = items)
  }
  
  # Generate thresholds (i.e., z-scores corresponding to having P(Z > z) = diff)
  Thresholds <- qnorm(p = diff_params, lower.tail = FALSE)
  Model <- list(NFac = factors,
                NItemPerFac = items_per_factor,
                Model = "orthogonal")
  Loadings <- list(FacLoadRange = factor_loading,
                   FacLoadDist = "fixed",
                   h2 = NULL)
  CrossLoadings <- list(ProbCrossLoad = 0,
                        CrudFactor = 0)
  ModelError <- list(ModelError = TRUE,
                     NMinorFac = 150,
                     ModelErrorType = "U",
                     ModelErrorVar = model_error,
                     epsTKL = 0.2)
  MonteCarlo <- list(NSamples = 1,
                     SampleSize = sample_size,
                     Raw = TRUE,
                     Thresholds = Thresholds)
  FactorScores <- list(FS = FALSE)
  Missing <- list(Missing = FALSE)
  
  # Generate reps sample binary datasets; if any items in a dataset have zero
  # variance, toss the dataset and generate a new one
  lapply(X = 1:reps,
         FUN = function(i) {
           zero_var <- TRUE
           while(zero_var) {
             sample_data <- fungible::simFA(Model = Model,
                                            Loadings = Loadings,
                                            CrossLoadings = CrossLoadings,
                                            ModelError = ModelError,
                                            MonteCarlo = MonteCarlo,
                                            FactorScores = FactorScores,
                                            Missing = Missing)$Monte$MCDataME[[1]]
             item_var <- apply(sample_data, MARGIN = 2, FUN = var)
             zero_var <- any(item_var == 0)
           }
           sample_data
         }
  )
}
