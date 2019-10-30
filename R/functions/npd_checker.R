# npd_checker: function to check whether a matrix is PSD
npd_checker <- function(r) {
  min_eig <- RSpectra::eigs_sym(A = r, k = 1, which = "SA")$values
  (min_eig < 0)
}