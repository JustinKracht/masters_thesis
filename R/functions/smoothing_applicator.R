# smoothing_applicator(): function smooths NPD tetrachoric correlation matrices
# and then returns a list of smoothed correlation matrices.
smoothing_applicator <- function(r) {
  # Check if the correlation matrix is NPD
  npd <- (RSpectra::eigs_sym(A = r, k = 1, which = "SA")$values < 0)
  
  # If the matrix is NPD, smooth using the three smoothing algorithms
  if (npd == TRUE) {
    out <- list(
      rnpd = r,
      rapa = fungible::smoothAPA(R = r,
                                 maxTries = 1e4)[c("RAPA", "convergence")],
      rby  = fungible::smoothBY(R = r, const = 1, eps = 0.001)[-c("glb", "eps")],
      rkb  = fungible::smoothKB(R = r)$RKB,
      npd  = npd
    )
  } else {
    out <- list(
      rnpd = r,
      rapa = NULL,
      rby  = NULL,
      rkb  = NULL,
      npd  = npd
    )
  }
}