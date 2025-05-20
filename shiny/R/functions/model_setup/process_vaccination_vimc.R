
process_vaccination_vimc <- function(
    vaccination_data ,
    vaccine,
    iso,
    year_start,
    year_end
){
  
  # Ensure data.table format
  setDT(vaccination_data)
  
  # Subset to country and year range
  years <- get_years(vaccination_data$year, year_start, year_end)
  
  vaccination_data[iso3 == iso & year %in% years & grepl(vaccine, name, ignore.case = T)] %>%
    filter(!is.na(coverage))
  
}