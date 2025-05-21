#' #' Unpack Dust Model State Data
#' #'
#' #' This function extracts and unpacks data from a dust model state object, separating compartments and non-compartments.
#' #'
#' #' @param model_system The model system object containing packing information and particle details.
#' #' @param model_object The dust model object containing state data.
#' #' @param dimension_names List of dimension names for states, time, and other variables.
#' #' @param which_state_dimensions List mapping state names to their associated dimensions.
#' #'
#' #' @return A data frame containing the unpacked dust model state data.
#' #' @export
unpack_dust2 <- function(model_system, model_object, dimension_names, which_state_dimensions) {
#'   
#'   #' Extract dust state data from the provided model
  dust_state <- dust_unpack_state(model_system, model_object)

  #' Identify which states are compartments (non-empty packing state entries)
  these_are_compartments <- sapply(model_system$packing_state, length) != 0

  #' Extract the total length of the time dimension
  time_length <- dimension_names$time[[1]]

  #' Unpack compartmental states
  unpacked_compartments <- rbindlist(
    lapply(as.numeric(which(these_are_compartments)), function(x) {
      this_obj <- dust_state[[x]]  # Extract compartment data
      present_dimensions <- which_state_dimensions[[which(names(which_state_dimensions) == names(these_are_compartments[x]))]]
      colnames <- present_dimensions  # Assign column names for clarity

      #' Process the data object into its proper format
      processed_data <- process_obj(this_obj, present_dimensions, colnames, time_length, x, model_system, dimension_names, dust_state)

      #' Combine processed data with appropriate structure
      rbindlist(processed_data, fill = TRUE)
    }), fill = TRUE
  )

  #' Unpack non-compartmental states
  unpacked_noncompartments <- rbindlist(
    lapply(as.numeric(which(!these_are_compartments)), function(x) {
      this_obj <- dust_state[[x]]  # Extract non-compartmental data

      #' Process data based on particle presence
      if (model_system$n_particles > 1) {
        as.data.frame(t(this_obj)) %>%
          gather(key = "run", value = "value") %>%
          mutate(time = rep(time_length, model_system$n_particles),
                 state = names(dust_state)[x],
                 run = gsub("V", "run_", run))
      } else {
        data.frame(this_obj) %>%
          setNames("value") %>%
          mutate(time = time_length, state = names(dust_state)[x])
      }
    }), fill = TRUE
  )

  #' Combine compartment and non-compartment data
  colnames_use <- unique(unlist(which_state_dimensions))
  colnames_use <- colnames_use[-which(colnames_use == "time")]

  #' Final data cleaning and formatting
  bind_rows(unpacked_compartments, unpacked_noncompartments) %>%
    mutate(across(.cols = all_of(colnames_use), .fns = as.character)) %>%
    mutate(across(.cols = all_of(c("time", "value")), as.numeric)) %>%
    mutate(across(.cols = all_of(colnames_use), \(x) replace_na(x, "All"))) %>%
    mutate(state = factor(state, levels = c(
      names(these_are_compartments[these_are_compartments]),
      names(these_are_compartments[!these_are_compartments])
    )))
}
