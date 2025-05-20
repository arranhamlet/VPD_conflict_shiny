
process_for_plotting <- function(run_model_output, input_data){
  
  #Aggregate across runs
  # Convert to data.table if needed
  if(all(!colnames(run_model_output) %in% "run")){
    run_model_output$run <- 0
  }
  
  setDT(run_model_output)
  
  # Cache constants
  time_adj <- input_data$time_adjust
  year_start <- input_data$year_start
  
  # Define states to split
  state_groups <- list(
    new_case = "new_case",
    rest = c("S", "E", "I", "R", "Is", "Rc")
  )
  
  # Define fast quantile summariser
  fast_summary <- function(x) {
    x <- sort(x)
    n <- length(x)
    list(
      value = median(x),
      value_min = x[max(1, floor(0.025 * n))],
      value_max = x[min(n, ceiling(0.975 * n))]
    )
  }
  
  # Apply summarisation per group
  result_list <- lapply(1:length(state_groups), function(group) {
   
     states_subset <- state_groups[[group]]
    
    dt <- run_model_output[state %in% states_subset]
    dt[, year := floor(year_start + (time * time_adj) / 365)]
    
    if(all(states_subset != "new_case")){
      dt <- dt[year %in% max(year) & age != "All"]
    } else {
      dt <- dt[age == "All"]
    }
    
    if (names(state_groups)[group]  == "new_case") {
      # First aggregate to yearly total per run
      dt_agg <- dt[, .(value = sum(value)), by = .(state, age, vaccination, risk, run, year)]
    } else {
      dt_agg <- dt
    }
    
    # Then summarise across runs
    dt_agg[, fast_summary(value), by = .(state, age, vaccination, risk, year)]
  })
  
  aggregate_df <- Reduce(rbind, result_list) %>%
    setDT()

  #Aggregate by susceptibility
  # Step 1: Filter and compute 'status'
  susceptibility_data <- aggregate_df[
    state %in% c("S", "E", "I", "R", "Is", "Rc") & age != "All"
  ][
    , status := fifelse(state == "S" & vaccination == 1, "Susceptible",
                        fifelse(state == "S" & vaccination > 1, "Vaccine protected",
                                fifelse(state != "S" & vaccination == 1, "Exposure protected",
                                        "Vaccine and exposure protected")))
    
    # Optionally enforce factor level ordering
  ][
    , status := factor(status, levels = c("Susceptible", "Vaccine protected", "Exposure protected", 
                                          "Vaccine and exposure protected"))
  ]
  
  # Step 2: Aggregate
  susceptibility_agg <- susceptibility_data[
    , .(value = sum(value)), by = .(year, age, status)
  ]
  
  # Step 3: Compute coverage within each (year, age)
  susceptibility_agg[
    , coverage := value / sum(value), by = .(year, age)
  ][
    is.na(coverage), coverage := 0
  ]

  list(aggregate_df = aggregate_df,
       susceptibility_data = susceptibility_data)
  
}