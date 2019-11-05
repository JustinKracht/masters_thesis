# MFRA(): Function to conduct minimum rank factor analysis. This function is
# taken from within the psych::fa() function.
# This is modified from factanal
FAout.wls <-  function(Psi, S, q) {
  diag(S) <- diag(S)- Psi  # added diag(S) - Psi instead of 1- Psi to handle covar=TRUE  9/11/18
  E <- eigen(S,symmetric = TRUE)
  L <- E$vectors[,1L:q,drop=FALSE] %*% diag(sqrt(pmax(E$values[1L:q,drop=FALSE],0)),q)  #added the > 0 test August 30, 2017
  return(L)
}

MRFA <- function(S, nf) {
  com.glb <- psych::glb.algebraic(S)
  L <- FAout.wls(1-com.glb$solution,S,nf)
  h2 <- com.glb$solution
  result <- list(loadings = L, communality = h2)
  result
}
