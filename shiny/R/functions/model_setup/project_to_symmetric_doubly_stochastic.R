project_to_symmetric_doubly_stochastic <- function(mat, max_iter = 1000, tol = 1e-8) {
  if (any(mat < 0)) stop("Matrix must be non-negative.")
  
  # Initial guess
  X <- mat / sum(mat)  # Normalize to help convergence
  
  for (i in 1:max_iter) {
    X_old <- X
    
    # Step 1: Project to symmetric matrix
    X <- (X + t(X)) / 2
    
    # Step 2: Project to doubly stochastic matrix using Sinkhorn-Knopp
    for (j in 1:50) {  # Inner loop to normalize
      X <- X / rowSums(X)
      X <- t(t(X) / colSums(X))
    }
    
    # Check convergence
    if (max(abs(X - X_old)) < tol) {
      return(X)
    }
  }
  
  warning("Did not converge within max_iter iterations.")
  return(X)
}
