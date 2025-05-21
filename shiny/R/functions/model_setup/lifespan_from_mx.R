#' Calculate Life Expectancy and Median Lifespan from Age-Specific Mortality Rates
#'
#' This function takes a vector of age-specific annual mortality rates (`mx`)
#' and calculates the mean and median lifespan by constructing a simplified life table.
#'
#' @param mx A numeric vector of age-specific mortality rates (e.g., for ages 0 to 100). 
#' The last value is treated as the open-ended final age group, where everyone dies.
#'
#' @return A named list with:
#' \describe{
#'   \item{mean_lifespan}{Life expectancy at birth (average number of years lived).}
#'   \item{median_lifespan}{Median age at death (age at which 50% of a birth cohort has died).}
#' }
#'
#' @examples
#' # Simulated mortality rates increasing with age
#' mx <- 0.0005 * exp(0.07 * (0:100))
#' lifespan_from_mx(mx)
#'
#' # Using a shorter age range
#' mx_short <- 0.0003 * exp(0.08 * (0:79))
#' lifespan_from_mx(mx_short)
#'
#' @export
lifespan_from_mx <- function(mx) {
  n_age <- length(mx)
  age <- 0:(n_age - 1)
  
  # Step 1: Convert mortality rate to probability of death (qx)
  qx <- mx / (1 + 0.5 * mx)
  qx[n_age] <- 1  # Everyone dies in final open-ended age group
  
  # Step 2: Survivorship (lx)
  lx <- numeric(n_age)
  lx[1] <- 100000
  for (i in 2:n_age) {
    lx[i] <- lx[i - 1] * (1 - qx[i - 1])
  }
  
  # Step 3: Number dying in each age group (dx)
  dx <- lx * qx
  
  # Step 4: Person-years lived (Lx)
  Lx <- numeric(n_age)
  Lx[1:(n_age - 1)] <- (lx[1:(n_age - 1)] + lx[2:n_age]) / 2
  Lx[n_age] <- lx[n_age] / mx[n_age]  # Approximation for final age group
  
  # Step 5: Total person-years above each age (Tx)
  Tx <- rev(cumsum(rev(Lx)))
  
  # Step 6: Life expectancy at each age (ex)
  ex <- Tx / lx
  
  # Step 7: Median lifespan (where cumulative deaths reach 50%)
  dx_norm <- dx / sum(dx)
  median_lifespan <- which(cumsum(dx_norm) >= 0.5)[1] - 1
  
  return(list(
    mean_lifespan = ex[1],
    median_lifespan = median_lifespan
  ))
}
