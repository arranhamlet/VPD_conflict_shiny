#' Reformat Contact Matrix to Match Age Vector
#'
#' This function reformats a raw contact matrix (which uses age groups in 5-year intervals)
#' to match a given age vector, mapping each age group in the contact matrix to the nearest
#' value in the age vector.
#'
#' @param contact_matrix_raw A square matrix of contact rates, with rows and columns corresponding to age groups in 5-year intervals.
#' @param age_vector A vector of age categories that the contact matrix will be reformatted to match.
#'
#' @return A matrix of contact rates with dimensions matching the provided age vector.
#' @export
reformat_contact_matrix <- function(contact_matrix_raw, age_vector) {
  
  # Define age groups based on 5-year intervals
  age_group <- seq(0, 80, by = 5)
  age_groups <- age_group[2:length(age_group)]
  
  # Rename rows and columns of the raw contact matrix to match age groups
  rownames(contact_matrix_raw) <- age_groups
  colnames(contact_matrix_raw) <- age_groups
  
  # Initialize a new age matrix with the desired age_vector dimensions
  new_age_matrix <- matrix(0, 
                           ncol = length(age_vector),
                           nrow = length(age_vector),
                           dimnames = list(age_vector, age_vector))
  
  # Map contact values from raw matrix to new matrix based on closest matching age group
  for (i in age_vector) {
    for (j in age_vector) {
      # Find the nearest matching age group from the raw matrix
      i_idx <- which.min(abs(age_groups - i))
      j_idx <- which.min(abs(age_groups - j))
      
      # Assign the contact value to the new matrix
      new_age_matrix[colnames(new_age_matrix) == i, rownames(new_age_matrix) == j] <- contact_matrix_raw[i_idx, j_idx]
    }
  }
  
  new_age_matrix
}
