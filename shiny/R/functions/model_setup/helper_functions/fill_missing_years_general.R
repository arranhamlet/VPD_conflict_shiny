#' Fill Missing Years by Group (Tidyverse)
#'
#' For each combination of grouping columns (all columns except year_col and value_col),
#' this function fills in missing consecutive years, setting the value_col to 0,
#' and replicates the grouping information. One additional year is added after the last.
#'
#' @param df A data frame with at least a year and value column.
#' @param year_col The name of the year column (as a string).
#' @param value_col The name of the value column (as a string).
#'
#' @return A tidy data frame with missing years filled for each group.
#'
#' @examples
#' df <- tibble::tibble(
#'   region = c("A", "A", "A", "B", "B"),
#'   source = c("X", "X", "X", "Y", "Y"),
#'   year = c(2000, 2005, 2010, 2000, 2010),
#'   value = c(1, 25, 29, 3, 12)
#' )
#' fill_missing_years_by_group(df, year_col = "year", value_col = "value")
#'
#' @export
fill_missing_years_general <- function(df, year_col, value_col) {

  year_sym <- sym(year_col)
  value_sym <- sym(value_col)
  
  # Grouping columns are all others
  group_cols <- setdiff(names(df), c(year_col, value_col))
  
  df_filled <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      min_year = min(.data[[year_col]]),
      max_year = max(.data[[year_col]]),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(data = list(tibble(!!year_sym := seq(min_year, max_year + 1)))) %>%
    select(-min_year, -max_year) %>%
    unnest(data) %>%
    left_join(df, by = c(setNames(group_cols, group_cols), year_col)) %>%
    mutate(
      !!value_sym := if_else(is.na(.data[[value_col]]), 0, .data[[value_col]])
    ) %>%
    arrange(across(all_of(c(group_cols, year_col))))
  
  return(df_filled)
}
