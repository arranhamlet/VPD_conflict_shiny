#' Filter Vaccination Data by Country, Vaccine, and Year
#'
#' This function filters vaccination data for a specified country (`iso`), vaccine type,
#' and optional year range. It ensures the data is in `data.table` format and uses an external
#' helper function `get_years()` to generate the year vector.
#'
#' @param vaccination_data A data frame or data.table containing vaccination data.
#'   Must include columns: `iso3`, `year`, and `name` (vaccine name).
#'
#' @param iso A character string specifying the ISO3 country code to filter by.
#'
#' @param vaccine A character string specifying the vaccine name to filter by.
#'   Defaults to `"All"` which includes all vaccines.
#'
#' @param year_start A numeric or character year indicating the start of the time range.
#'   If left as an empty string, includes all years before `year_end`.
#'
#' @param year_end A numeric or character year indicating the end of the time range.
#'   If left as an empty string, includes all years after `year_start`.
#'
#' @return A filtered `data.table` with vaccination data for the specified country,
#'   vaccine, and year range.
#'
#' @import data.table
#'
#' @examples
#' # Example usage:
#' filtered_data <- process_vaccination(vaccination_df, iso = "KEN", vaccine = "Measles", year_start = 2010, year_end = 2020)
#'
#' @export
process_vaccination <- function(
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
  
  filtered <- vaccination_data[iso3 == iso & year %in% years]
  
  # Subset vaccine
  if (vaccine != "All") {
    filtered <- filtered[name == vaccine]
  }
  
  return(filtered)
}
