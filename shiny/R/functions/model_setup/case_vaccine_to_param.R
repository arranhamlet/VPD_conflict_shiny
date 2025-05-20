case_vaccine_to_param <- function(
    demog_data,
    processed_vaccination,
    processed_vaccination_sia,
    processed_case,
    vaccination_schedule
){
  
  #Set up parameters
  n_age <- demog_data$input_data$n_age
  n_vacc <- demog_data$input_data$n_vacc
  n_risk <- demog_data$input_data$n_risk
  
  vaccination_type <- paste(unique(c(unique(processed_case$disease_description),
                                     processed_vaccination_sia$vaccination_name,
                                     processed_vaccination_sia$disease,
                                     processed_vaccination$disease,
                                     processed_vaccination$antigen,
                                     processed_vaccination$antigen_description)), collapse = "|")
  
  if(grepl("Diphtheria|Pertussis", vaccination_type, ignore.case = T)) vaccination_type <- paste0(vaccination_type, "|DTPCV1|DTPCV3|DTaP|DT|DTwP", collapse = "")

  ages <- 0:(n_age - 1)
  years <- demog_data$input_data$year_start:demog_data$input_data$year_end
  
  #Subset schedule
  schedule_subset <- vaccination_schedule %>%
    subset(!is.na(AGEADMINISTERED) & grepl(vaccination_type, VACCINE_DESCRIPTION, ignore.case = T) & ISO_3_CODE == demog_data$input_data$iso & TARGETPOP_DESCRIPTION != "Travellers") %>%
    mutate(
      age_years = case_when(
        grepl("Y", AGEADMINISTERED) ~ as.numeric(gsub("[^0-9.-]", "", AGEADMINISTERED)),
        grepl("M", AGEADMINISTERED) ~ as.numeric(gsub("[^0-9.-]", "", AGEADMINISTERED))/12,
        grepl("W", AGEADMINISTERED) ~ as.numeric(gsub("[^0-9.-]", "", AGEADMINISTERED))/52,
      )
    ) %>%
    arrange(age_years) %>%
    subset(!grepl("ADULTS", TARGETPOP, ignore.case = T) & !grepl("contact", AGEADMINISTERED) & VACCINECODE != "TD_S")
  
  if(any(is.na(schedule_subset$SCHEDULEROUNDS))){
    schedule_subset <- schedule_subset %>%
      case_when(
        is.na(SCHEDULEROUNDS) ~ max(SCHEDULEROUNDS) + 1,
        TRUE ~ SCHEDULEROUNDS 
      )
  }
  

  #Process vaccination first
  processed_vaccination_upd <- fill_missing_years_general(
    df = processed_vaccination,
    year_col = "year",
    value_col = "coverage"
  ) %>%
    group_by(antigen, antigen_description, dose_order, year) %>%
    summarise(coverage = max(coverage), .groups = "drop") %>%
    subset(coverage != 0) %>%
    arrange(year)
  
  vaccination_years <- unique(processed_vaccination_upd$year)
  vaccination_param_df <- Reduce(rbind, sapply(1:nrow(processed_vaccination_upd), function(x){
    
    #Subset to the row and work out the timing
    this_row <- processed_vaccination_upd[x, ]
    dose_to_use <- paste(unlist(strsplit(this_row$antigen_description, " ")), collapse = "|")
    timing <- schedule_subset %>% 
      subset(SCHEDULEROUNDS == min(this_row$dose_order, max(SCHEDULEROUNDS))) %>% 
      pull(age_years)
    
    target_vaccination <- if (this_row$dose_order == 1) {
      1
    } else {
      ((2 * this_row$dose_order - 2):(2 * this_row$dose_order - 1))
    }
    
    #Create dataframe
    #Need to create a dataframe of here and then a dataframe 1 after with a value of 0 to turn off the interpolation
    Reduce(rbind, sapply(target_vaccination, function(e){
      data.frame(
        dim1 = which(ages == floor(timing)),
        dim2 = e,
        dim3 = 1,
        dim4 = which(this_row$year == vaccination_years),
        value = this_row$coverage/100
      )
    }, simplify = FALSE))
    
  }, simplify = FALSE))
  
  #Process sia data - different data and different approach
  if(nrow(processed_vaccination_sia) > 0){
    processed_vaccination_sia_upd <- fill_missing_years_general(
      df = processed_vaccination_sia,
      year_col = "year",
      value_col = "coverage"
    ) %>%
      group_by(disease, vaccination_name, age, year) %>%
      summarise(coverage = max(coverage), .groups = "drop") %>%
      arrange(year)
    
    
    sia_param_df <- Reduce(rbind, sapply(1:nrow(processed_vaccination_sia_upd), function(x){
      
      #Subset to the row and work out the timing
      this_row <- processed_vaccination_sia_upd[x, ]
      
      #Create dataframe
      #Need to create a dataframe of here and then a dataframe 1 after with a value of 0 to turn off the interpolation
      data.frame(
        dim1 = which(ages == this_row$age),
        dim2 = 1,
        dim3 = 1,
        dim4 = which(this_row$year == vaccination_years),
        value = this_row$coverage
      )
      
    }, simplify = FALSE))
    
    #Combine vaccination
    vaccination_combo_param_df <- vaccination_param_df %>%
      rbind(sia_param_df) %>%
      group_by(dim1, dim2, dim3, dim4) %>%
      summarise(
        value = pmin(sum(value), 1),
        .groups = "drop"
      )
    
  } else {
    vaccination_combo_param_df <- vaccination_param_df
  }
  
  #Have to include a row for time 1 where the values are 0
  vaccination_combo_param_df <- vaccination_combo_param_df %>%
    rbind(
      vaccination_combo_param_df[1, ] %>%
        mutate(dim4 = 1,
               value = 0)
    ) %>%
    group_by(dim1, dim2, dim3, dim4) %>%
    summarise(value = max(value), .groups = "drop")
  
  #Seeded case
  if(nrow(processed_case) != 0){
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
  
  tt_seeded = c(0, which(years %in% processed_case_upd$year) - 1)
  
  } else {
    
  tt_seeded = 0
  case_param_df = data.frame(dim1 = 1,
                             dim2 = 1,
                             dim3 = 1,
                             dim4 = 1,
                             value = 0)
  }
  
  list(tt_vaccination = c(0, which(years %in% processed_vaccination_upd$year)),
       vaccination_coverage = vaccination_combo_param_df,
       tt_seeded = tt_seeded,
       seeded = case_param_df)
  
}