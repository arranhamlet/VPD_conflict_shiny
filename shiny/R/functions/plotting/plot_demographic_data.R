#' Plot Demographic Trends Over Time
#'
#' This function generates a suite of demographic visualizations using population,
#' fertility, mortality, and migration data. It outputs two panels of plots:
#' one showing population trends and age distribution, and the other showing 
#' crude death, birth, and migration rates over time.
#'
#' @param demographic_data A list containing demographic data objects. Must include:
#' \itemize{
#'   \item \code{input_data$year_start}: Integer start year.
#'   \item \code{population_data}: Matrix or data frame of population counts (time x age).
#'   \item \code{crude_death}: Data frame with mortality values and dimensions \code{time}, \code{value}, and optionally \code{dim3}.
#'   \item \code{crude_birth}: Data frame with birth rate values and column \code{time}.
#'   \item \code{migration_in_number}: Data frame with migration counts and column \code{dim1}.
#' }
#'
#' @return A `patchwork` grid of ggplot objects showing:
#' \enumerate{
#'   \item Total population over time
#'   \item Final year population age distribution
#'   \item Annual mortality
#'   \item Annual fertility
#'   \item Annual migration
#' }
#'
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom scales comma
#' @import patchwork
#'
#' @examples
#' # plot_demographic_data(my_demographic_object)
#'
#' @export
plot_demographic_data <- function(
    demographic_data  
){
  # Extract base year
  year_start <- demographic_data$input_data$year_start
  
  # Reshape population data to long format
  demog_data <- demographic_data$population_data
  population_by_age <- reshape2::melt(demog_data) %>%
    data.table::setnames(c("time", "age", "value")) %>%
    dplyr::arrange(time, age)
  
  # Total population over time
  population_total <- population_by_age %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(value = sum(value), .groups = "drop")
  
  # Weighted average mortality (crude death rate)
  demographic_data$crude_death$population <- population_by_age$value
  
  overall_mortality <- demographic_data$crude_death %>%
    dplyr::group_by(dim3) %>%
    dplyr::mutate(
      population_weight = population / sum(population),
      weighted_value = value * population_weight
    ) %>%
    dplyr::summarise(value = sum(weighted_value), .groups = "drop")
  
  # Total migration by year
  migration_aggregate <- demographic_data$migration_in_number %>%
    dplyr::group_by(dim4) %>%
    dplyr::summarise(value = sum(value), .groups = "drop")
  
  # Plot: Population over time
  population_over_time <- ggplot2::ggplot(
    data = population_total,
    mapping = aes(
      x = time + year_start,
      y = value * 1000
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = "Population") +
    ggplot2::scale_y_continuous(label = scales::comma) +
    ggplot2::theme_bw()
  
  # Plot: Age pyramid in the final year
  age_pyramid_final <- ggplot2::ggplot(
    data = dplyr::filter(population_by_age, time == max(time)),
    mapping = aes(
      x = age,
      y = value / sum(value)
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Age", y = "Proportion of\nthe population") +
    ggplot2::theme_bw()
  
  # Plot: Mortality over time
  mortality_plot <- ggplot2::ggplot(
    data = overall_mortality,
    mapping = aes(
      x = dim3 + year_start - 1,
      y = value
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = "Annual\nmortality") +
    ggplot2::scale_y_continuous(label = scales::comma) +
    ggplot2::theme_bw()
  
  # Plot: Fertility over time
  fertility_plot <- ggplot2::ggplot(
    data = demographic_data$crude_birth,
    mapping = aes(
      x = dim2 + year_start,
      y = value
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = "Annual\nfertility") +
    ggplot2::scale_y_continuous(label = scales::comma) +
    ggplot2::theme_bw()
  
  # Plot: Migration over time
  migration_plot <- ggplot2::ggplot(
    data = migration_aggregate,
    mapping = aes(
      x = dim4 + year_start - 1,
      y = value
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = "Annual\nmigration") +
    ggplot2::scale_y_continuous(label = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  
  # Assemble and return plots
  population_plots <- population_over_time + age_pyramid_final
  inflow_outflow_plots <- mortality_plot + fertility_plot + migration_plot
  
  population_plots / inflow_outflow_plots + 
    patchwork::plot_annotation(title = "Demographics")
}

