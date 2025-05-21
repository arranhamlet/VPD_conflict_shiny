
process_vaccination_sia <- function(
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
  
  vaccination_data <- vaccination_data %>%
    rename(vaccination_name = vaccine)
  
  vaccination_data[!is.na(coverage) & country == iso & year %in% years & grepl(vaccine, vaccination_name, ignore.case = T)]
  
}