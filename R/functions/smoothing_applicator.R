# smoothing_applicator(): function smooths NPD tetrachoric correlation matrices
# and then returns a list of smoothed correlation matrices.
smoothing_applicator <- function(r) {
  # Check if the correlation matrix is NPD
  npd <- (min(eigen(r)$values) < 0)
  
  # If the matrix is NPD, smooth using the three smoothing algorithms
  if (npd == TRUE) {
    # Apply the alternating projections algorithm (APA; Higham, 2002)
    r_apa <- fungible::smoothAPA(R = r,
                                 maxTries = 1e4)[c("RAPA", "convergence")]
    # If the APA algorithm did not converge, set r_apa to NA
    if (r_apa$convergence != 0) r_apa <- NA
    
    # Apply the Bentler-Yuan algorithm (BY; Bentler & Yuan, 2011)
    r_by <- fungible::smoothBY(R = r, const = 1, eps = 0.001)[c("RBY", 
                                                                "constant", 
                                                                "convergence", 
                                                                "outStatus")]
    # If the BY algorithm did not converge, set r_by to NA
    if (r_by$convergence != TRUE) r_by <- NA
    
    # Apply the Knol-Berger algorithm (KB; Knol & Berger, 1991)
    r_kb <- fungible::smoothKB(R = r)$RKB
    
    out <- list(
      r_npd = r,
      r_apa = r_apa,
      r_by  = r_by,
      r_kb  = r_kb,
      npd   = npd
    )
  } else {
    out <- list(
      r_npd = r,
      r_apa = NULL,
      r_by  = NULL,
      r_kb  = NULL,
      npd   = npd
    )
  }
  out
}