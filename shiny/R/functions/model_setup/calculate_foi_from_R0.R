calculate_foi_from_R0 <- function(R0, contact_matrix, N, I = NULL, infectious_period = 5) {
  # R0              = target basic reproduction number
  # contact_matrix  = age-by-age matrix, contacts from age i to j
  # N               = total population by age group (length n)
  # I               = infected individuals by age group (length n); if NULL, uses eigenvector
  # infectious_period = mean duration of infectiousness (in days)
  
  stopifnot(length(N) == nrow(contact_matrix))
  
  # Step 1: Construct unscaled NGM using full susceptibility assumption
  unscaled_NGM <- matrix(0, nrow = length(N), ncol = length(N))
  for (i in seq_along(N)) {
    for (j in seq_along(N)) {
      unscaled_NGM[i, j] <- contact_matrix[i, j] * infectious_period
    }
  }
  
  # Step 2: Find leading eigenvalue to get scaling
  lambda_max <- Re(eigen(unscaled_NGM)$values[1])
  
  # Step 3: Scale beta to hit target R0
  beta <- R0 / lambda_max / infectious_period
  
  # Step 4: Define I/N (infection prevalence by age)
  if (is.null(I)) {
    # Use right eigenvector as proxy for relative infection intensity
    eig <- eigen(unscaled_NGM)
    v <- Re(eig$vectors[, 1])
    v <- abs(v)
    I <- v / sum(v) * sum(N) * 0.01  # assume 1% total prevalence as default scale
  }
  
  I_over_N <- I / N
  
  # Step 5: FOI on susceptibles in each age group
  FOI <- beta * (contact_matrix %*% I_over_N)
  names(FOI) <- paste0("age_group_", seq_along(FOI))
  
  return(as.vector(FOI))
}
