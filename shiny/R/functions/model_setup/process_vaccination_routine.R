
process_vaccination_routine <- function(
    vaccination_data,
    iso,
    vaccine = "All",
    year_start = "",
    year_end = ""
){
  
  # Ensure data.table format
  setDT(vaccination_data)
  
  # Subset to country and year range
  years <- get_years(vaccination_data$year, year_start, year_end)
  
  filtered <- vaccination_data[CODE == iso & YEAR %in% years] %>%
    filter(!is.na(COVERAGE))
  
  # Subset vaccine
  if (vaccine != "All") {
    filtered <- filtered[grepl(vaccine, ANTIGEN_DESCRIPTION, ignore.case = T) | grepl(vaccine, Disease, ignore.case = T)]
  }
  
  filtered %>%
    clean_names() %>%
    group_by(year, antigen, antigen_description) %>%
    summarise(
      target_number = median(target_number, na.rm = T),
      doses = median(doses, na.rm = T),
      dose_order = median(dose_order, na.rm = T),
      coverage = median(coverage, na.rm = T),
      .groups = "drop"
    ) %>%
    arrange(year)
  
}
