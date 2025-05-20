#' Plot Case Counts and Vaccination Coverage
#'
#' This function creates two vertically stacked plots for visual comparison:
#' \enumerate{
#'   \item A bar chart showing annual disease case counts, faceted by `name` (typically region or group).
#'   \item A line chart showing annual vaccination coverage as a percentage, faceted by `iso3` (country code).
#' }
#' Missing case values are excluded from the case plot. The final plot includes a title and combines both plots using `patchwork`.
#'
#' @param case_data A data frame containing reported disease case counts.
#'   Must include the columns: \code{year}, \code{cases}, \code{name}, and optionally \code{disease_description}.
#'
#' @param vaccination_data A data frame containing vaccination data.
#'   Must include the columns: \code{iso3}, \code{year}, \code{name}, \code{fully_vaccinated_persons}, and \code{total_population}.
#'
#' @return A combined ggplot object (using `patchwork`) showing disease case counts and vaccination coverage over time.
#'   The top plot is faceted by `name`, and the bottom plot by `iso3`.
#'
#' @import dplyr
#' @import ggplot2
#' @import patchwork
#' @importFrom scales comma
#'
#' @examples
#' # Example usage:
#' plot_case_vaccination(case_df, vaccination_df)
#'
#' @export
plot_case_vaccination_routine <- function(
    case_data,
    vaccination_data
){
  
  # Plot case counts
  cases <- ggplot2::ggplot(
    data = dplyr::filter(case_data, !is.na(cases)),
    mapping = aes(
      x = year,
      y = cases
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "",
      y = "Cases",
      fill = ""
    ) +
    ggplot2::theme(
      legend.position = "top"
    ) +
    ggplot2::scale_y_continuous(label = scales::comma)
  
  # Plot vaccination coverage
  vaccination <- ggplot2::ggplot(
    data = vaccination_data,
    mapping = aes(
      x = year,
      y = coverage,
      color = antigen_description
    )
  ) +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "",
      y = "Vaccination\ncoverage (%)",
      color = ""
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 100))
  
  # Combine and return the plots
  cases / vaccination + 
    patchwork::plot_layout(guides = 'collect') +
    patchwork::plot_annotation(title = "Cases and vaccinations")
}
