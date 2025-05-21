#' Symmetrize a Contact Matrix Using Population Weights
#'
#' This function symmetrizes a square contact matrix using population sizes,
#' ensuring that total contacts between age groups are reciprocal.
#'
#' The symmetrized matrix is computed using the formula:
#' \deqn{C^\text{sym}_{i,j} = \frac{n_i C_{i,j} + n_j C_{j,i}}{n_i + n_j}}
#' where \eqn{C_{i,j}} is the contact rate from age group \eqn{i} to \eqn{j},
#' and \eqn{n_i} and \eqn{n_j} are the population sizes of groups \eqn{i} and \eqn{j}.
#'
#' @param C A square numeric matrix of contact rates (e.g., POLYMOD contact matrix),
#'   where each element \code{C[i, j]} represents the average number of daily contacts
#'   made by an individual in age group \code{i} with individuals in age group \code{j}.
#' @param pop A numeric vector of population sizes for each age group. Must be the same
#'   length as the number of rows and columns in \code{C}.
#'
#' @return A symmetric matrix of the same dimensions as \code{C}, with contact rates
#'   adjusted to satisfy reciprocal contact symmetry based on population sizes.
#'
#' @examples
#' C <- matrix(c(10, 2, 1,
#'               2, 8, 2,
#'               1, 2, 6), nrow = 3, byrow = TRUE)
#' rownames(C) <- colnames(C) <- c("0-4", "5-17", "18+")
#' pop <- c(3000000, 7000000, 50000000)
#' symmetrize_contact_matrix(C, pop)
#'
#' @export
symmetrize_contact_matrix <- function(C, pop) {
  if (!is.matrix(C)) stop("C must be a matrix")
  if (length(pop) != nrow(C)) stop("Population vector must match matrix dimensions")
  if (nrow(C) != ncol(C)) stop("Matrix must be square")
  
  n <- length(pop)
  C_sym <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      C_sym[i, j] <- (pop[i] * C[i, j] + pop[j] * C[j, i]) / (pop[i] + pop[j])
    }
  }
  
  rownames(C_sym) <- rownames(C)
  colnames(C_sym) <- colnames(C)
  
  return(C_sym)
}
