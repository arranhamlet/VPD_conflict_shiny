add_extra_time_rows <- function(df, n_extra, time_col = "time", value_col = "value", final_value = NULL) {
  # Identify characteristic columns
  char_cols <- setdiff(names(df), c(time_col, value_col))
  
  # Check that required columns exist
  stopifnot(all(c(time_col, value_col) %in% names(df)))
  stopifnot(length(char_cols) > 0)
  
  # Load required libraries
  library(dplyr)
  library(tidyr)
  
  # Get last time and generate new times
  last_time <- max(df[[time_col]])
  new_times <- (last_time + 1):(last_time + n_extra)
  
  # Create the base structure for new rows
  base_characters <- df %>%
    distinct(across(all_of(char_cols)))
  
  new_rows <- tidyr::crossing(
    base_characters,
    !!time_col := new_times
  )
  
  # Determine the value to assign
  if (is.null(final_value)) {
    # Use last known value per group
    last_values <- df %>%
      group_by(across(all_of(char_cols))) %>%
      slice_max(order_by = .data[[time_col]], n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(all_of(char_cols), !!value_col)
    
    new_rows <- left_join(new_rows, last_values, by = char_cols)
  } else {
    # Use specified final_value
    new_rows[[value_col]] <- final_value
  }
  
  # Bind and return
  bind_rows(df, new_rows)
}
