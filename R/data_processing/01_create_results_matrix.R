# Create results_matrix from saved RDS files
pacman::p_load(tidyverse, here)

# Load data
project_dir <- here()
data_dir <- here("Data")
load(paste0(data_dir, "/environment.RData"))

# Create a data frame with a row for each rep (observation) ---------------
results_matrix <- slice(conditions_matrix,
                        rep(1:nrow(conditions_matrix), each = reps))
# Add condition, rep, and id variables
results_matrix <- mutate(results_matrix,
                         condition = rep(1:nrow(conditions_matrix), 
                                         each = reps),
                         rep = rep(1:reps, times = nrow(conditions_matrix)),
                         id = 1:n())

# Calculate the proportion of npd matrices --------------------------------
rmsd_extractor <- function(rep_list, Rpop) {
  map_dfc(.x = rep_list$smoothed_matrices,
          .f = function(X) {
            if (!anyNA(X$R)) {
              out <- fungible::rmsd(X$R, 
                                    Rpop, 
                                    Symmetric = TRUE, 
                                    IncludeDiag = FALSE)
            } else {
              out <- NA
            }
            out
          })
}

extract_condition_data <- function(condition) {
  condition_smoothed <- readRDS(paste0(data_dir,
                                       "/smoothed_matrices",
                                       "/smoothed_matrix_list", 
                                       formatC(condition, width = 3, flag = 0), 
                                       ".RDS"))
  condition_binary_data <- readRDS(paste0(data_dir,
                                          "/binary_data",
                                          "/binary_data", 
                                          formatC(condition, width = 3, flag = 0), 
                                          ".RDS"))
  reps <- length(condition_smoothed)
  # Extract npd_vec
  npd <- purrr::map_lgl(.x = condition_smoothed, "npd")
  # Extract rmsd(Rpop, RSm)
  rmsd_df <- purrr::map_dfr(.x = 1:reps, 
                            .f = ~ rmsd_extractor(condition_smoothed[[.x]],
                                                  Rpop = condition_binary_data[[.x]]$Rpop))
  names(rmsd_df) <- stringr::str_replace(names(rmsd_df),
                                         pattern = c("RNPD", "RAPA", "RBY", "RKB"),
                                         replace = c("None", "APA", "BY", "KB"))
  # Extract convergence for RBY and RAPA
  convergence_df <- purrr::map_dfr(.x = condition_smoothed,
                                   .f = function(X) {
                                     if (length(X$smoothed_matrices$RAPA) == 1) {
                                       APA_converged <- NA
                                     } else {
                                       APA_converged <- X$smoothed_matrices$RAPA$convergence == 0
                                     }
                                     if (length(X$smoothed_matrices$RBY) == 1) {
                                       BY_converged <- NA
                                       BY_constant <- NA
                                       BY_outstatus <- NA
                                     } else {
                                       BY_converged <- X$smoothed_matrices$RBY$convergence
                                       BY_constant <- X$smoothed_matrices$RBY$constant
                                       BY_outstatus <- X$smoothed_matrices$RBY$outStatus
                                       if (is.null(BY_constant)) BY_constant <- NA
                                       if (is.null(BY_outstatus)) BY_outstatus <- NA
                                     }
                                     data.frame("APA_converged" = APA_converged,
                                                "BY_converged" = BY_converged,
                                                "BY_constant" = BY_constant,
                                                "BY_outstatus" = BY_outstatus)
                                   })
  
  out <- cbind(results_matrix[results_matrix$condition == condition,],
               npd,
               rmsd_df,
               convergence_df)
  tidyr::pivot_longer(out, cols = c("None", "APA", "BY", "KB"),
                      names_to = "smoothing_method",
                      values_to = "distance_Rpop_Rsm")
}

# Add proportion of negative variance variable ----------------------------
eigvals_list <- lapply(1:nrow(conditions_matrix),
                       FUN = function(i) {
                         tetcor_file <- paste0(data_dir,
                                               "/tetcor_matrices",
                                               "/tetcor_matrix_list",
                                               formatC(i, digits = 2, flag = "0"),
                                               ".RDS")
                         tetcor_list <- readRDS(tetcor_file)
                         lapply(tetcor_list, FUN = function(R) { 
                           eigen(R, symmetric = TRUE, only.values = TRUE)$values
                         }
                         ) 
                       }
)

prop_neg_var_vec <- unlist(
  lapply(eigvals_list, function(X) {
    unlist(
      lapply(X,
             function(eigs) {
               if (any(eigs < 0)) {
                 prop_neg_var <- abs(sum(eigs[eigs < 0])) / sum(eigs)
               } else {
                 prop_neg_var <- 0
               }
               prop_neg_var
             })
    )
  })
)

# Save data extracted from smoothed_matrix_list RDS objects ---------------
smoothing_results <- map_dfr(1:nrow(conditions_matrix), 
                             .f = extract_condition_data)
smoothing_results$prop_neg_var <- rep(prop_neg_var_vec, each = 4)
saveRDS(smoothing_results,
        file = "./Data/smoothing_results.RDS")

# Save data extracted from loading_matrix_list RDS objects ----------------
extract_factor_loadings_by_condition <- function(condition, data_dir = data_dir) {
  extract_loadings_from_fa_method <- function(loadings_by_fa_method,
                                              pop_loadings) {
    map_dfr(.x = names(loadings_by_fa_method),
            .f = function(fa_method_name) {
              fa_method <- loadings_by_fa_method[[fa_method_name]]
              if (!anyNA(fa_method)) {
                list(fa_method = fa_method_name,
                     loading_rmsd = fungible::rmsd(fa_method$loadings,
                                                   pop_loadings,
                                                   Symmetric = FALSE),
                     fa_convergence = fa_method$convergence,
                     heywood = fa_method$heywood)
              } else {
                list(fa_method = fa_method_name,
                     loading_rmsd = NA,
                     fa_convergence = NA,
                     heywood = NA)
              }
            })
  }
  # Load in the RDS file corresponding to the condition
  condition_loadings <- readRDS(paste0(data_dir,
                                       "/loading_matrices",
                                       "/loading_matrix_list", 
                                       formatC(condition, width = 3, flag = 0), 
                                       ".RDS"))
  condition_data <- readRDS(paste0(data_dir,
                                   "/binary_data/",
                                   "binary_data",
                                   formatC(condition, width = 3, flag = 0),
                                   ".RDS"))
  # Extract loadings for each rep
  map_dfr(.x = 1:length(condition_loadings),
          .f = function(i) {
            smoothing_methods <- names(condition_loadings[[i]])
            # Extract loadings for each smoothing method in each rep
            rep_data <- map_dfr(.x = smoothing_methods,
                                .f = function(smooth_method) {
                                  out <- extract_loadings_from_fa_method(
                                    condition_loadings[[i]][[smooth_method]],
                                    pop_loadings = condition_data[[i]]$loadings
                                  )
                                  smooth_method_name <- switch(smooth_method,
                                                               "RNPD" = "None",
                                                               "RAPA" = "APA",
                                                               "RBY" = "BY",
                                                               "RKB" = "KB")
                                  out$smoothing_method <- smooth_method_name
                                  out$rep <- i
                                  out$condition <- condition
                                  out
                                })
          })
}

loading_results <- map_dfr(1:nrow(conditions_matrix), 
                           .f = extract_factor_loadings_by_condition,
                           data_dir = data_dir)
saveRDS(loading_results,
        file = "./Data/loading_results.RDS")

# smoothing_results <- readRDS(here("Data", "smoothing_results.RDS"))
# loading_results <- readRDS(here("Data", "loading_results.RDS"))

# Merge the datasets to create a complete results matrix ------------------
results_matrix <- right_join(x = smoothing_results, y = loading_results)
results_matrix <- results_matrix %>%
  dplyr::select(id, rep, condition, subjects_per_item:test_type, npd:BY_outstatus, 
         smoothing_method, fa_method, fa_convergence, heywood, distance_Rpop_Rsm,
         loading_rmsd, prop_neg_var)

## Make "None" the baseline level for smoothing methods
results_matrix$smoothing_method <- fct_relevel(results_matrix$smoothing_method,
                                               "None")

## Recoding results_matrix$factors into results_matrix$factors_rec
results_matrix$factors_rec <- as.character(results_matrix$factors)
results_matrix$factors_rec <- fct_recode(results_matrix$factors_rec,
                                         "Factors: 1" = "1",
                                         "Factors: 3" = "3",
                                         "Factors: 5" = "5",
                                         "Factors: 10" = "10")
## Recoding results_matrix$factor_loading into results_matrix$factor_loading_rec
results_matrix$factor_loading_rec <- as.character(results_matrix$factor_loading)
results_matrix$factor_loading_rec <- fct_recode(results_matrix$factor_loading_rec,
                                                "Loading: 0.3" = "0.3",
                                                "Loading: 0.5" = "0.5",
                                                "Loading: 0.8" = "0.8")
## Recoding results_matrix$subjects_per_item into results_matrix$subjects_per_item_rec
results_matrix$subjects_per_item_rec <- as.character(results_matrix$subjects_per_item)
results_matrix$subjects_per_item_rec <- fct_recode(results_matrix$subjects_per_item_rec,
                                                   "Subjects/item: 5" = "5",
                                                   "Subjects/item: 10" = "10",
                                                   "Subjects/item: 15" = "15")
## Recoding results_matrix$items_per_factor into results_matrix$items_per_factor_rec
results_matrix$items_per_factor_rec <- as.character(results_matrix$items_per_factor)
results_matrix$items_per_factor_rec <- fct_recode(results_matrix$items_per_factor_rec,
                                                  "Items/factor: 5" = "5",
                                                  "Items/factor: 10" = "10")
## Recoding results_matrix$factors into results_matrix$factors_rec
results_matrix$factors_rec <- as.character(results_matrix$factors)
results_matrix$factors_rec <- fct_recode(results_matrix$factors_rec,
                                         "Factors: 1" = "1",
                                         "Factors: 3" = "3",
                                         "Factors: 5" = "5",
                                         "Factors: 10" = "10")
## Recoding results_matrix$model_error into results_matrix$model_error_rec
results_matrix$model_error_rec <- as.character(results_matrix$model_error)
results_matrix$model_error_rec <- fct_recode(results_matrix$model_error_rec,
                                             "Error: 0.0" = "0",
                                             "Error: 0.1" = "0.1",
                                             "Error: 0.3" = "0.3")
## Recoding results_matrix$fa_method into results_matrix$fa_method_rec
results_matrix$fa_method_rec <- fct_recode(results_matrix$fa_method,
                                           "PA" = "fapa",
                                           "ULS" = "fals",
                                           "ML" = "faml")
## Reordering results_matrix$factors_rec
results_matrix$factors_rec <- factor(results_matrix$factors_rec, 
                                     levels = c("Factors: 1", "Factors: 3", 
                                              "Factors: 5", "Factors: 10"))

## Reordering results_matrix_npd$subjects_per_item_rec
results_matrix$subjects_per_item_rec <- factor(
  results_matrix$subjects_per_item_rec, 
  levels = c("Subjects/item: 5", "Subjects/item: 10", "Subjects/item: 15")
)

## Reordering results_matrix_npd$items_per_factor_rec
results_matrix$items_per_factor_rec <- factor(
  results_matrix$items_per_factor_rec, 
  levels = c("Items/factor: 5", "Items/factor: 10")
)

# Save results matrix -----------------------------------------------------
saveRDS(results_matrix, file = paste0(data_dir, "/results_matrix.RDS"))
saveRDS(dplyr::filter(results_matrix, npd == TRUE), 
        file = paste0(data_dir, "/results_matrix_npd.RDS"))
