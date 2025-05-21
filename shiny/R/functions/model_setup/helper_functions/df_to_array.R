df_to_array <- function(df) {
  # Identify dimension columns
  dim_cols <- grep("^dim\\d+$", names(df), value = TRUE)
  n_dims <- length(dim_cols)
  dim_names <- paste0("dim", seq_len(n_dims))
  
  # Determine array shape
  dims <- sapply(dim_names, function(col) max(df[[col]]))
  
  # Convert index columns to matrix
  idx_matrix <- as.matrix(df[dim_names])
  values <- df$value
  
  # Precompute linear indices
  strides <- c(1, cumprod(head(dims, -1)))
  lin_idx <- rowSums((idx_matrix - 1) * matrix(strides, nrow = nrow(idx_matrix), ncol = n_dims, byrow = TRUE)) + 1
  
  # Create array and assign values
  arr <- array(0, dim = dims)
  arr[lin_idx] <- values
  arr
}



