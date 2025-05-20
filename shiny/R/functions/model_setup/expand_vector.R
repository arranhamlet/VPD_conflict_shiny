expand_vector <- function(vec, n_extra) {
  # Check that vec is numeric and non-empty
  stopifnot(is.numeric(vec), length(vec) > 0)
  
  # Get the last value and create new sequence
  last_val <- max(vec, na.rm = TRUE)
  new_vals <- seq(from = last_val + 1, length.out = n_extra)
  
  # Combine and return
  c(vec, new_vals)
}
