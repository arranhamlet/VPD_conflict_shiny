#' Process Dust Model Data
#'
#' Transforms a multi-dimensional model output array into a long-format data frame for analysis.
#'
#' @param this_obj The model data array to process.
#' @param present_dimensions Vector of dimension names for this state.
#' @param colnames Column names for the melted data frame.
#' @param time_length Length of the time dimension.
#' @param x Index of the current state in the model.
#' @param model_system The model system containing particle info.
#' @param dimension_names List of dimension name values.
#' @param dust_state List of unpacked model state data.
#'
#' @return A list of [detailed_melted_df, aggregate_df].
#' @export
#' 
process_obj <- function(this_obj, present_dimensions, colnames, time_length, x, model_system, dimension_names, dust_state) {
  state_name <- names(dust_state)[x]
  
  has_particles <- model_system$n_particles > 1
  obj_dims <- length(dim(this_obj))
  expect_dims <- length(present_dimensions)
  
  # Case 1: Multi-particle model or unexpected dimensions
  if (has_particles || obj_dims != expect_dims) {
    dim_names <- vector("list", obj_dims)
    
    # Match dimension names from present_dimensions and dimension_names
    for (i in seq_along(present_dimensions)) {
      dim_label <- present_dimensions[i]
      dim_names[[i]] <- dimension_names[[dim_label]][[1]]
    }
    
    # Identify particle dimension (assumed to be the second-to-last)
    particle_dim_index <- obj_dims - 1
    dim_names[[particle_dim_index]] <- paste0("run_", seq_len(model_system$n_particles))
    dimnames(this_obj) <- dim_names
    
    # Melt the object to long format
    melted_df <- reshape2::melt(this_obj)
    setnames(melted_df, c(colnames[1:(which(colnames == "time") - 1)], "run", "time", "value"))
    
    melted_df <- melted_df %>%
      mutate(state = state_name) %>%
      select(time, state, all_of(colnames[1:(which(colnames == "time") - 1)]), run, value)
    
    # Aggregate by state, time, run
    aggregate_df <- melted_df %>%
      fgroup_by(state, time, run) %>%
      fsummarise(value = sum(value))
    
    list(melted_df, aggregate_df)
    
  } else {
    # Case 2: Single-particle model, dimensions match
    dim_names <- vector("list", obj_dims)
    for (i in seq_along(present_dimensions)) {
      dim_label <- present_dimensions[i]
      dim_names[[i]] <- dimension_names[[dim_label]][[1]]
    }
    dimnames(this_obj) <- dim_names
    
    melted_df <- reshape2::melt(this_obj)
    setnames(melted_df, c(present_dimensions, "value"))
    
    melted_df <- melted_df %>%
      mutate(state = state_name) %>%
      select(time, state, all_of(present_dimensions[present_dimensions != "time"]), value)
    
    aggregate_df <- melted_df %>%
      fgroup_by(state, time) %>%
      fsummarise(value = sum(value))
    
    list(melted_df, aggregate_df)
  }
}
