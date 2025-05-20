#' Prepare Demographic Inputs for a Transmission Model
#'
#' This function processes raw demographic data—covering migration, fertility, mortality,
#' and population counts—into a structured format suitable for use in an age-, vaccination-,
#' and risk-structured infectious disease transmission model.
#'
#' @param migration A data frame of net migration rates by year and country. Must contain columns `iso3`, `year`, and `migration_rate_1000`.
#' @param fertility A data frame of fertility rates by single year of age (15–49) and year. Must contain columns `iso3`, `year`, and `x15` to `x49`.
#' @param mortality A data frame of mortality counts by single year of age (0–100) and year. Must contain columns `iso3`, `year`, and `x0` to `x100`.
#' @param population_all A data frame of total population counts by age (0–100) and year. Must contain columns `iso3`, `year`, and `x0` to `x100`.
#' @param population_female A data frame of female population counts by age (15–49) and year. Must contain columns `iso3`, `year`, and `x15` to `x49`.
#' @param iso A 3-letter ISO country code to subset the data.
#' @param year_start First year to include (default: earliest year in migration data).
#' @param year_end Final year to include (default: latest year in migration data).
#' @param population_modifier A scalar to multiply all population values (default: 1).
#' @param fertility_modifier A scalar to multiply fertility rates (default: 1).
#' @param death_modifier A scalar to multiply death rates (currently not applied, default: 1).
#' @param migration_modifier A scalar to multiply migration rates (default: 1).
#' @param n_age Number of collapsed age bins for output data (e.g., 5-year bins).
#' @param n_vacc Number of vaccination strata (used for model structure).
#' @param n_risk Number of risk strata (used for model structure).
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{N0}{Initial population distribution by age, suitable for model initialization.}
#'   \item{crude_birth}{Annual fertility rate data frame.}
#'   \item{crude_death}{Annual mortality rate data frame.}
#'   \item{tt_migration}{Time index corresponding to migration data.}
#'   \item{migration_in_number}{Age-specific annual net migration counts.}
#'   \item{migration_distribution_values}{Uniform migration distribution across strata (constant 1s).}
#'   \item{population_data}{Collapsed population matrix over time.}
#'   \item{input_data}{A data frame summarizing input assumptions and configuration.}
#' }
#'
#' @details
#' This function uses age-collapsing utilities (`collapse_age_bins()`, `split_and_sum()`) and assumes
#' consistent naming for age columns (e.g., `x0` to `x100`). It performs per-year filtering and
#' applies modifiers to scale data as needed for model fitting or sensitivity analysis.
#'
#' @import data.table
#' @import dplyr
#' @importFrom reshape2 melt
#'
#' @examples
#' # processed <- process_demography(mig, fert, mort, pop_all, pop_fem, iso = "KEN", n_age = 10)
#'
#' @export
process_demography <- function(
    migration, fertility, mortality, population_all, population_female, contact_matricies,
    iso,
    year_start = "", 
    year_end = "",
    population_modifier = 1, 
    fertility_modifier = 1, 
    death_modifier = 1,
    migration_modifier = 1,
    n_age = 1, 
    number_of_vaccines = 0, 
    n_risk = 1
) {
  
  n_vacc <- if(number_of_vaccines == 0) 1 else number_of_vaccines * 2 + 1
  
  filter_country <- function(dt) dt[iso3 == iso]
  
  # Time window
  years <- get_years(migration$year, start = year_start, end = year_end)
  time_run_for <- length(years)
  time_all <- 0:(time_run_for - 1)
  
  # Convert to data.table
  data.table::setDT(migration); data.table::setDT(fertility)
  data.table::setDT(mortality); data.table::setDT(population_all)
  data.table::setDT(population_female)
  
  # Filter for country
  migration <- filter_country(migration)
  fertility <- filter_country(fertility)
  mortality <- filter_country(mortality)
  population_all <- filter_country(population_all)
  population_female <- filter_country(population_female)
  
  # Base population
  pop_all_raw <- as.matrix(population_all[year %in% years, paste0("x", 0:100), with = FALSE]) * population_modifier
  pop_all <- collapse_age_bins(pop_all_raw, n_age)
  
  #Contact matricies - take those provided and reformat for the world from Prem et al., 2017
  all_ages <- 0:100
  age_groups <- sapply(split(all_ages, sort(all_ages %% n_age)), function(x) min(x))
  
  country_contact <- if(!any(names(contact_matricies) == iso)) Reduce("+", contact_matricies)/length(contact_matricies) else contact_matricies[[which(names(contact_matricies) == iso)]]
  
  reformatted_contact_matrix <- reformat_contact_matrix(
    contact_matrix_raw = country_contact,
    age_vector = age_groups
  )
  
  reformatted_contact_matrix <- symmetrize_contact_matrix(
    reformatted_contact_matrix,
    pop = pop_all[nrow(pop_all), ]
  )

  reformatted_contact_matrix <- project_to_symmetric_doubly_stochastic(reformatted_contact_matrix)

  # Mortality
  mort_mat_raw <- as.matrix(mortality[year %in% years, paste0("x", 0:100), with = FALSE]) * death_modifier
  mort_mat <- collapse_age_bins(mort_mat_raw, n_age)
  mortality_rate <- pmin(mort_mat / pop_all, 1)
  mortality_rate[!is.finite(mortality_rate)] <- 1
  mortality_df <- reshape2::melt(t(mortality_rate)) %>%
    data.table::setnames(c("dim1", "dim3", "value")) %>%
    dplyr::mutate(dim2 = 1)
  
  # Fertility
  fert_mat <- as.matrix(fertility[year %in% years, paste0("x", 15:49), with = FALSE]) * fertility_modifier
  pop_fem <- as.matrix(population_female[year %in% years, paste0("x", 15:49), with = FALSE]) * population_modifier
  denom <- rowSums(pop_fem)
  denom[denom == 0] <- NA
  fertility_by_year <- data.frame(
    dim1 = n_risk,
    dim2 = time_all + 1,
    value = pmin(rowSums((fert_mat / 1000) * pop_fem) / denom, 1)
  )
  
  # Migration
  mig_rates <- migration[year %in% years, migration_rate_1000] * migration_modifier
  
  migration_in_number <- data.table::rbindlist(lapply(seq_len(nrow(pop_all)), function(i) {
    mig_vals <- round(pop_all[i, ] * mig_rates[i])
    chunk <- split_and_sum(mig_vals, n_age)
    data.frame(
      dim1 = seq_len(n_age),
      dim2 = 1,
      dim3 = 1,
      dim4 = i,
      value = chunk
    )
  }))
  
  # Initial population
  init_vals <- round(pop_all[1, ] * 1000)
  init_chunk <- split_and_sum(init_vals, n_age)
  N0_df <- data.frame(
    dim1 = seq_len(n_age),
    dim2 = 1,
    dim3 = 1,
    value = init_chunk
  )
  
  # Total population df (optional)
  total_population_df <- data.table::rbindlist(lapply(seq_len(nrow(pop_all)), function(i) {
    chunk <- split_and_sum(round(pop_all[i, ] * 1000), n_age)
    data.frame(
      dim1 = seq_len(n_age),
      dim2 = 1,
      dim3 = 1,
      dim1 = i,
      value = chunk
    )
  }))
  
  # Migration distribution
  migration_distribution_values <- data.table::CJ(
    dim1 = 1,
    dim2 = seq_along(time_all)
  )[, value := 1]
  
  # Output
  list(
    N0 = N0_df,
    crude_birth = fertility_by_year,
    crude_death = mortality_df,
    tt_migration = time_all,
    migration_in_number = migration_in_number,
    migration_distribution_values = migration_distribution_values,
    population_data = pop_all,
    contact_matrix = reformatted_contact_matrix,
    input_data = data.frame(
      iso = iso,
      year_start = min(years), 
      year_end = max(years),
      population_modifier = population_modifier, 
      fertility_modifier = fertility_modifier, 
      death_modifier = death_modifier,
      migration_modifier = migration_modifier,
      n_age = n_age, 
      n_vacc = n_vacc, 
      n_risk = n_risk
    )
  )
}
