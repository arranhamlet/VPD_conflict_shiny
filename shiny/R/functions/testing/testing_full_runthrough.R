
testing_full_runthrough <- function(
    migration, 
    fertility, 
    mortality, 
    population_all, 
    population_female,
    year_start,
    year_end,
    iso,
    n_age,
    n_vacc, 
    n_risk,
    length_model_run,
    no_runs
  
  ){
  
  #Run function - add in modifiers to account for Gaza
  demog_data <- process_demography(
    migration = migration, 
    fertility = fertility, 
    mortality = mortality, 
    population_all = population_all, 
    population_female = population_female,
    year_start = year_start,
    year_end = year_end,
    iso = iso,
    n_age = n_age,
    n_vacc = n_vacc, 
    n_risk = n_risk
  )
  
  #Set up model
  params <- param_packager(
    
    n_age = n_age,
    n_vacc = n_vacc,
    n_risk = n_risk,
    
    N0 = demog_data$N0,
    crude_birth = demog_data$crude_birth,
    crude_death = demog_data$crude_death,
    simp_birth_death = 0,
    aging_rate = 1,
    
    tt_migration = demog_data$tt_migration,
    migration_in_number = demog_data$migration_in_number,
    migration_distribution_values = demog_data$migration_distribution_values
    
  )
  
  #Run model
  clean_df <- run_model(
    odin_model = model,
    params = params,
    time = length_model_run,
    no_runs = no_runs
  ) %>%
    mutate(n_vacc_comp = n_vacc)
  
  clean_df
  
}