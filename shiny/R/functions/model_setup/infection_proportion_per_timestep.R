infection_proportion_per_timestep <- function(contact_matrix, R0, D, population_vector = NULL, timestep_length = 1) {
  # timestep_length: number of days in the desired output timestep (e.g., 1 for daily, 7 for weekly, 365 for yearly)
  
  if (!is.matrix(contact_matrix) || nrow(contact_matrix) != ncol(contact_matrix)) {
    stop("contact_matrix must be a square matrix.")
  }
  
  if (!is.null(population_vector) && length(population_vector) != nrow(contact_matrix)) {
    stop("population_vector must match contact_matrix dimensions.")
  }
  
  # Dominant eigenvalue of contact matrix
  dominant_eigenvalue <- max(Re(eigen(contact_matrix)$values))
  
  # Estimate beta
  beta <- R0 / (D * dominant_eigenvalue)
  
  # Total population
  N <- if (is.null(population_vector)) 1 else sum(population_vector)
  
  # Infection proportion per desired timestep
  infection_rate_per_day <- R0 / (D * N)
  infection_proportion <- infection_rate_per_day * timestep_length
  
  return(list(
    infection_proportion_per_timestep = infection_proportion,
    timestep_length = timestep_length,
    beta = beta,
    dominant_eigenvalue = dominant_eigenvalue
  ))
}
