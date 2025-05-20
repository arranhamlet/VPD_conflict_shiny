#' Generate a Sequence of Years from a Vector and Optional Start/End Bounds
#'
#' This helper function returns a sequence of years based on a full vector of years (`yrs`)
#' and optional start and end bounds. If either `start` or `end` is an empty string,
#' the minimum or maximum value in `yrs` is used, respectively.
#'
#' @param yrs A numeric vector of years from which to derive default start or end values.
#'
#' @param start A numeric or character value specifying the start year. If `""`, the minimum year in `yrs` is used.
#'
#' @param end A numeric or character value specifying the end year. If `""`, the maximum year in `yrs` is used.
#'
#' @return A numeric vector of consecutive years from `start` to `end` (inclusive).
#'
#' @examples
#' get_years(2000:2020, start = 2005, end = 2010)
#' get_years(2000:2020, start = "", end = 2010)
#' get_years(2000:2020, start = 2005, end = "")
#' get_years(2000:2020, start = "", end = "")
#'
#' @export
get_years <- function(yrs, start, end) {
  start <- if (start == "") min(yrs, na.rm = TRUE) else as.numeric(start)
  end <- if (end == "") max(yrs, na.rm = TRUE) else as.numeric(end)
  start:end
}
