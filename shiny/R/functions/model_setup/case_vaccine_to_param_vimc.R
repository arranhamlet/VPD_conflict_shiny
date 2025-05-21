case_vaccine_to_param_vimc <- function(
    demog_data,
    processed_vaccination_vimc,
    processed_case,
    vaccination_schedule,
    setting
){
  
  #Set up parameters
  n_age <- demog_data$input_data$n_age
  n_vacc <- demog_data$input_data$n_vacc
  n_risk <- demog_data$input_data$n_risk
  
  ages <- 0:(n_age - 1)
  years <- demog_data$input_data$year_start:demog_data$input_data$year_end
  
  #Subset schedule
  schedule_subset <- vaccination_schedule %>%
    filter(grepl(unique(processed_case$disease_description), Disease)) %>%
    filter(grepl(setting, transmission_setting))
  
  #Get dose timing
  dose_timing <- schedule_subset %>%
    select(contains("dose in ")) %>%
    gather() %>%
    filter(!is.na(value)) %>%
    mutate(value = floor(value)) %>%
    mutate(key = case_when(
      grepl("First", key) ~ "1st",
      grepl("Second", key) ~ "2nd",
      grepl("Third", key) ~ "3rd",
      grepl("Fourth", key) ~ "4th",
      grepl("Fifth", key) ~ "5th",
      grepl("Sixth", key) ~ "6th"
    ))
  
  #Process vaccination first
  processed_vaccination_upd <- fill_missing_years_general(
    df = processed_vaccination_vimc %>%
      rename(iso = iso3),
    year_col = "year",
    value_col = "coverage"
  ) %>%
    group_by(name, age, year) %>%
    summarise(coverage = max(coverage), .groups = "drop") %>%
    arrange(year)
  
  vaccination_years <- unique(processed_vaccination_upd$year)
  vaccination_param_df <- Reduce(rbind, sapply(1:nrow(processed_vaccination_upd), function(x){
    
    #Subset to the row and work out the timing
    this_row <- processed_vaccination_upd[x, ]

    vaccine_dose <- which.min(abs(dose_timing$value - this_row$age))
    target_vaccination <- if (vaccine_dose == 1) {
      1
    } else {
      ((2 * vaccine_dose - 2):(2 * vaccine_dose - 1))
    }
    
    #Create dataframe
    #Need to create a dataframe of here and then a dataframe 1 after with a value of 0 to turn off the interpolation
    Reduce(rbind,
           sapply(target_vaccination, function(e){
             data.frame(
               dim1 = which(ages == this_row$age),
               dim2 = e,
               dim3 = 1,
               dim4 = which(this_row$year == vaccination_years),
               value = this_row$coverage
             )
           }, simplify = FALSE))
    
  }, simplify = FALSE))
  
  #Remove above 1 values, sometimes found in the data
  vaccination_param_df <- vaccination_param_df %>%
    #Have to include a row for time 1 where the values are 0
    rbind(
      vaccination_param_df[1, ] %>%
        mutate(dim4 = 1,
               value = 0)
    )
  
  #Combine vaccination
  vaccination_combo_param_df <- vaccination_param_df %>%
    group_by(dim1, dim2, dim3, dim4) %>%
    summarise(
      value = pmin(sum(value), 1),
      .groups = "drop"
    )
  #Seeded case
  processed_case_upd <- fill_missing_years_general(
    df = processed_case,
    year_col = "year",
    value_col = "cases"
  ) %>%
    filter(year <= demog_data$input_data$year_end)
  
  case_param_df <- Reduce(rbind, sapply(1:nrow(processed_case_upd), function(x){
    
    #Subset to the row and work out the timing
    this_row <- processed_case_upd[x, ]
    #Population_distribution
    popdist <- demog_data$population_data[which(this_row$year == years), ]
    popdist <- popdist/sum(popdist)
    
    set.seed(1)
    
    #Work out case distribution
    case_distribution_first <- as.data.frame(table(names(sample(popdist, this_row$cases, prob = popdist, replace = T))))
    
    if(nrow(case_distribution_first) == 0){
      data.frame(
        dim1 = ages + 1,
        dim2 = 1,
        dim3 = 1,
        dim4 = x + 1,
        value = 0
      )
    } else {
      case_distribution <- case_distribution_first %>%
        mutate(Var1 = as.numeric(as.character(Var1)))
      
      #Assign cases dataframe
      Reduce(rbind, sapply(1:nrow(case_distribution), function(y){
        data.frame(
          dim1 = case_distribution[y, 1],
          dim2 = 1,
          dim3 = 1,
          value = case_distribution[y, 2]
        )
      }, simplify = FALSE)) %>%
        mutate(dim4 = x + 1) %>%
        select(dim1, dim2, dim3, dim4, value)
    }
    
  }, simplify = FALSE))
  
  #Have to include a row for time 1 where the values are 0
  case_param_df <- case_param_df %>%
    rbind(
      case_param_df[1, ] %>%
        mutate(dim4 = 1,
               value = 0)
    ) %>% filter(!(duplicated(dim1, dim4) & value == 0))
  
  list(tt_vaccination = c(0, which(years %in% processed_vaccination_upd$year)),
       vaccination_coverage = vaccination_combo_param_df,
       tt_seeded = c(0, which(years %in% processed_case_upd$year)),
       seeded = case_param_df)
  
}