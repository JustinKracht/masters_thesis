# smoothing_applicator(): function smooths NPD tetrachoric correlation matrices
# and then returns a list of smoothed correlation matrices.
smoothing_applicator <- function(r) {
  # Check if the correlation matrix is NPD
  npd <- (min(eigen(r)$values) < 0)
  
  # If the matrix is NPD, smooth using the three smoothing algorithms
  if (npd == TRUE) {
    # Apply the alternating projections algorithm (APA; Higham, 2002)
    RAPA <- fungible::smoothAPA(R = r,
                                maxTries = 1e4)[c("RAPA", "convergence")]
    RAPA <- list(R = RAPA$RAPA,
                 convergence = RAPA$convergence)
    
    # If the APA algorithm did not converge, set Rsm to NA
    if (RAPA$convergence != 0) RAPA$R <- NA
    
    # Apply the Bentler-Yuan algorithm (BY; Bentler & Yuan, 2011)
    RBY <- fungible::smoothBY(R = r, const = 1, eps = 0.001)[c("RBY", 
                                                               "constant", 
                                                               "convergence", 
                                                               "outStatus")]
    RBY <- list(R = RBY$RBY,
                convergence = RBY$convergence,
                constant = RBY$constant,
                outStatus = RBY$outStatus)
    
    # If the BY algorithm did not converge, set R to NA
    if (RBY$convergence != TRUE) RBY$R <- NA
    
    # Apply the Knol-Berger algorithm (KB; Knol & Berger, 1991)
    RKB <- fungible::smoothKB(R = r)$RKB
    RKB <- list(R = RKB)
    
    out <- list(
      smoothed_matrices = list(RNPD = list(R = r),
                               RAPA = RAPA,
                               RBY  = RBY,
                               RKB  = RKB),
      npd  = npd
    )
  } else {
    out <- list(
      smoothed_matrices = list(RNPD = list(R = r),
                               RAPA = list(R = NA),
                               RBY  = list(R = NA),
                               RKB  = list(R = NA)),
      npd   = npd
    )
  }
  out
}