#' Run Dust Model Simulation
#'
#' This function initializes and runs a dust model simulation, then processes the output into a clean data frame format.
#'
#' @param params A list of model parameters, including dimensions and rates.
#' @param time The end time step for the simulation (default = 1000).
#' @param no_runs The number of stochastic runs/particles for the simulation (default = 25).
#'
#' @return A data frame containing the unpacked and cleaned model simulation results.
#' @export
run_model <- function(odin_model, params, time = 1000, no_runs = 25) {
  
  #' Define the dust system and initialize it with given parameters
  sys <- dust2::dust_system_create(odin_model(), params, n_particles = no_runs)
  
  #' Set the initial state for the dust system
  dust2::dust_system_set_state_initial(sys)
  
  #' Define the time vector for simulation (starting from 0)
  full_time_vector <- 0:(time - 1)

  #' Run the dust system simulation over the defined time period
  y <- dust2::dust_system_simulate(sys, full_time_vector)
  
  #' Process and clean output data by unpacking the results
  clean_df <- unpack_dust2(
    model_system = sys,
    model_object = y,
    dimension_names = list(
      age = list(as.character(1:params$n_age)),
      vaccination = list(as.character(1:params$n_vacc)),
      risk = list(as.character(1:params$n_risk)),
      time = list(full_time_vector),
      no_migration_changes = list(params$tt_migration + 1)
    ),
    which_state_dimensions = list(
      S = c("age", "vaccination", "risk", "time"),
      E = c("age", "vaccination", "risk", "time"),
      I = c("age", "vaccination", "risk", "time"),
      R = c("age", "vaccination", "risk", "time"),
      Is = c("age", "vaccination", "risk", "time"),
      Rc = c("age", "vaccination", "risk", "time"),
      Reff_age_prop = c("age", "time"),
      Reff_age = c("age", "time"),
      new_case = c("age", "vaccination", "risk", "time"),
      FOI_scale_sum = c("age", "time")
    )
  )
}
