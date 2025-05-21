
# Creates the data files that get read in later on 


setup_for_shiny <- function(
    iso,
    disease,
    vaccine,
    R0,
    timestep = "day",
    year_start = "",
    year_end = "",
    susceptability_distribution
){
  
  #Load files needed
  migration <- import(here("data", "processed", "WPP", "migration.csv"))
  fertility <- import(here("data", "processed", "WPP", "fertility.csv"))
  mortality <- import(here("data", "processed", "WPP", "deaths.csv"))
  population_all <- import(here("data", "processed", "WPP", "age_both.csv"))
  population_female <- import(here("data", "processed", "WPP", "age_female.csv"))
  contact_matricies <- import(here("data", "raw", "contact_matricies", "contact_all.rdata"))
  routine_vaccination_data <- import("data/raw/WHO/coverage-data_updated.xlsx")
  sia_vaccination <- import("data/processed/vaccination/sia_vimc.rds")
  full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")
  vaccination_schedule <- import("data/processed/WHO/vaccine-schedule-data.xlsx")
  vaccination_translated <- import("data/processed/vaccination/Vaccine_Abbreviations_and_Diseases.csv")
  vaccination_pre1980 <- import("data/processed/vaccination/vaccine_coverage_backextrapolation_rules.xlsx")
  
  routine_vaccination_data <- routine_vaccination_data %>%
    left_join(vaccination_translated, by = c("ANTIGEN" = "Abbreviation"))
  
  disease_parameters <- import("data/processed/model_parameters/disease_parameters_table.xlsx") %>%
    rename(disease_n = disease) %>%
    subset(disease_n == disease) %>%
    mutate(value = replace_na(as.numeric(value), 0))
  
  vaccine_parameters <- import("data/processed/vaccination/vaccine_protection.xlsx") %>%
    rename(disease_n = disease) %>%
    subset(disease_n == disease)
  
  #Calculate number of vaccines
  number_of_vaccines <- ifelse(
    disease == "measles", 2,
    ifelse(disease == "diphtheria" | disease == "pertussis", 3, 0)
  )

  #Run processing
  model_data_preprocessed <- model_input_formatter_wrapper(
    iso = iso,    
    disease = disease,
    vaccine = vaccine,
    n_age = 101,
    number_of_vaccines = length(number_of_vaccines),
    #Datasets
    migration,
    fertility,
    mortality,
    population_all,
    population_female,
    contact_matricies = contact_matricies,
    disease_data = full_disease_df,
    vaccination_data_routine = routine_vaccination_data,
    vaccination_data_sia = sia_vaccination,
    year_start = year_start,
    year_end = year_end
  )
  
  #Set up pre-1980 vaccination
  vac_pre1980_sub <- vaccination_pre1980 %>%
    rownames_to_column() %>%
    group_by(rowname) %>%
    clean_names() %>%
    mutate(income_group = paste(c(sapply(unlist(strsplit(income_group, " |-")), function(x) substring(x, 1, 1)), "C"), collapse = "")) %>%
    rename(disease_n = disease) %>%
    subset(tolower(disease_n) == disease & who_region == paste(c(get_WHO_region(iso3cs = iso), "O"), collapse = "") & income_group == as.character(get_income_group(iso)))
  
  #First year
  first_year_vac <- model_data_preprocessed$processed_vaccination_data %>% 
    subset(year == min(year))
  
  year_diff <- min(first_year_vac$year) - vac_pre1980_sub$introduction_year
  vac_prop <- sapply(first_year_vac$coverage, function(e) seq(vac_pre1980_sub$starting_coverage_percent, max(vac_pre1980_sub$starting_coverage_percent, e), length.out = 6), simplify = FALSE)
  
  
  #Add in this pre-1980 vaccination
  pre_1980 <- Reduce(rbind, sapply(1:year_diff, function(x){
    Reduce(rbind, sapply(1:nrow(first_year_vac), function(k){
      here <- first_year_vac[k, ]
      here$year <- vac_pre1980_sub$introduction_year + (x - 1)
      here$coverage <- vac_prop[[k]][min(c(x, length(vac_prop[[k]])))]
      here      
    }, simplify = FALSE)
    )
  }, simplify = FALSE))
  
  total_vac <- rbind(pre_1980,
                     model_data_preprocessed$processed_vaccination_data)
  
  #Take pre-processed case and vaccination data and get it ready for params
  case_vaccination_ready <- case_vaccine_to_param(
    demog_data = model_data_preprocessed$processed_demographic_data,
    processed_vaccination = total_vac %>%
      subset(year == max(year)) %>%
      mutate(disease = disease),
    processed_vaccination_sia = model_data_preprocessed$processed_vaccination_sia,
    processed_case = model_data_preprocessed$processed_case_data,
    vaccination_schedule = vaccination_schedule
  )
  
  #Add in vaccine provided protection
  n_vacc <- model_data_preprocessed$processed_demographic_data$input_data$n_vacc
  
  vacc_order <- seq(2, n_vacc, by = 2)
  age_vaccination_beta_modifier <- Reduce(rbind, sapply(vacc_order, function(j){
    
    dose_details <- vaccine_parameters %>%
      mutate(order = abs(j - vaccine_parameters$dose)) %>%
      subset(order == min(order))
    
    rbind(
      expand.grid(
        dim1 = 1:101,
        dim2 = j,
        dim3 = 1,
        value = dose_details %>% subset(parameter == "short_term_protection") %>% pull(value)
      ),
      expand.grid(
        dim1 = 1:101,
        dim2 = j + 1,
        dim3 = 1,
        value = dose_details %>% subset(parameter == "long_term_protection") %>% pull(value)
      )
    )
    
  }, simplify = FALSE))
  
  
  time_adjust <- if(timestep == "day"){
    1
  } else if(timestep == "week"){
    7
  } else if(timestep == "month"){
    30 
  } else if(timestep == "quarter"){
    91.25
  } else if(timestep == "year"){
    365
  }
  
  #Set up model
  waning_immunity <- subset(disease_parameters, parameter == "natural immunity waning") %>% pull(value) %>% as.numeric() * 365
  
  params <- param_packager(
    
    # Demographic parameters
    n_age = model_data_preprocessed$processed_demographic_data$input_data$n_age,
    n_vacc = model_data_preprocessed$processed_demographic_data$input_data$n_vacc,
    n_risk = model_data_preprocessed$processed_demographic_data$input_data$n_risk,
    
    # Vaccine parameters
    short_term_waning = 1/((vaccine_parameters %>% subset(parameter == "short_term_waning") %>% pull(value) %>% as.numeric() %>% max() * 365)/time_adjust),
    long_term_waning = 1/((vaccine_parameters %>% subset(parameter == "long_term_waning") %>% pull(value) %>% as.numeric() %>% max() * 365)/time_adjust),
    age_vaccination_beta_modifier = age_vaccination_beta_modifier,
    
    # Disease parameters 
    R0 = R0,
    tt_R0 = 0,
    user_specified_foi = 0,
    
    #Vaccination
    vaccination_coverage = case_vaccination_ready$vaccination_coverage %>%
      subset(dim4 == max(dim4)),
    tt_vaccination_coverage = 0,
    
    #Disease parameters
    cfr_normal = 0,
    cfr_severe = 0,
    incubation_rate = 1/subset(disease_parameters, parameter == "incubation period") %>% pull(value) %>% as.numeric() * time_adjust,
    recovery_rate = 1/subset(disease_parameters, parameter == "infectious period") %>% pull(value) %>% as.numeric() * time_adjust,
    severe_recovery_rate = 1/subset(disease_parameters, parameter == "infectious period") %>% pull(value) %>% as.numeric() * time_adjust,
    
    natural_immunity_waning = if(waning_immunity == 0) 0 else  1/waning_immunity * time_adjust,
    
    #Setting up vaccination
    #Demographic parameters
    contact_matrix = model_data_preprocessed$processed_demographic_data$contact_matrix,
    S0 = susceptability_distribution %>% 
      subset(dim4 == 1),
    Rpop0 = susceptability_distribution %>% 
      subset(dim4 == 2),
    I0 = 0,

    #List of when birth_death_changes
    crude_birth = model_data_preprocessed$processed_demographic_data$crude_birth %>%
      subset(dim2 == max(dim2)) %>%
      mutate(value = value/(365/time_adjust)),
    crude_death = model_data_preprocessed$processed_demographic_data$crude_death %>%
      subset(dim3 == max(dim3)) %>%
      mutate(value = value/(365/time_adjust)),
    aging_rate = time_adjust/365,
   
    #Birth ages
    repro_low = 15,
    repro_high = 49,
    age_maternal_protection_ends = 1,
    protection_weight_vacc = 1,
    protection_weight_rec = 1,
    migration_represent_current_pop = 1
    
  )
  
  #Export these
  list(
    params = params,
    input_data = model_data_preprocessed$processed_demographic_data$input_data %>%
      mutate(time_adjust = time_adjust)
  )
  
}
