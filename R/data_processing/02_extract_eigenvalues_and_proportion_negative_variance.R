# Extract eigenvalues from tetrachoric matrices and then calculate the
# proportion of negative variance
pacman::p_load(here)
data_dir <- here("Data")
eigvals_list <- lapply(1:216,
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

saveRDS(prop_neg_var_vec,
        paste0(data_dir, "/prop_neg_var_vec.RDS"))
