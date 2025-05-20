#' Parameter Packager Function
#'
#' This function packages and formats all input parameters for an infectious disease transmission model
#' with age, vaccination, and risk stratification. It includes support for demographic processes such as
#' births, deaths, aging, and risk group movement, as well as interpolated time-varying parameters.
#'
#' @param n_age Number of age groups.
#' @param n_vacc Number of vaccination strata.
#' @param n_risk Number of risk groups.
#' @param S0 Initial population size (array or scalar).
#' @param I0 Initial number of infections (array or scalar).
#' @param contact_matrix Contact matrix defining age-specific contact patterns.
#' @param incubation_rate Incubation rate (E to I transition).
#' @param recovery_rate Recovery rate for non-severe infections.
#' @param severe_recovery_rate Recovery rate for severe infections (defaults to `recovery_rate` if `NULL`).
#' @param prop_severe Proportion of infections that become severe (can be scalar or array).
#' @param prop_complications Proportion of severe infections that result in complications.
#' @param natural_immunity_waning Waning rate of natural protection.
#' @param vaccination_coverage Time-varying vaccination coverage array.
#' @param tt_vaccination_coverage Time points for changes in vaccination coverage.
#' @param age_vaccination_beta_modifier Modifies transmission based on age and vaccination status.
#' @param R0 Basic reproduction number.
#' @param tt_R0 Time points at which R0 changes.
#' @param seeded Initial number of seeded infections (can be time-varying).
#' @param tt_seeded Time points for seeding changes.
#' @param aging_rate Rate of aging between age compartments (last age group should be 0).
#' @param initial_background_death Background death rate at baseline.
#' @param simp_birth_death If set to 1, births and deaths are linked to background rates.
#' @param tt_birth_changes Time points when the birth rate changes.
#' @param tt_death_changes Time points when the death rate changes.
#' @param crude_birth Crude birth rate (used if `simp_birth_death != 1`).
#' @param crude_death Crude death rate (used if `simp_birth_death != 1`).
#' @param protection_weight_vacc Weight of vaccine-derived maternal protection by age.
#' @param protection_weight_rec Weight of natural infection-derived maternal protection by age.
#' @param age_maternal_protection_ends Age group at which maternal protection ends.
#' @param repro_low Lowest reproductive age group.
#' @param repro_high Highest reproductive age group (defaults to `n_age` if `NULL`).
#' @param tt_moving_risk Time points for risk group movement transitions.
#' @param moving_risk_values Total number of people moving between risk groups (by age/vacc/risk), accepts a data.frame of dimensions 1-4, and values.
#' @param moving_risk_distribution_values Proportion of movers going to each compartment (S, E, I, R, Is, Rc), accepts a data.frame of dimensions 1-2, and values.
#'
#' @return A named list of formatted and validated parameters for input into a stochastic infectious disease model.
#'         The function also performs internal checks on validity and consistency of the inputs.
#'
#' @export
param_packager <- function(
    
  #Dimensions
  n_age = 1,
  n_vacc = 1,
  n_risk = 1,  
  
  #Initial values
  S0 = 1000,
  I0 = 0,
  Rpop0 = 0,
  
  #Add in contact matrix
  contact_matrix = NULL,
  #Disease parameters
  #Incubation rate
  incubation_rate = 0,
  #Recovery rate
  recovery_rate = 0,
  #Recovery rate for severe infections
  severe_recovery_rate = NULL,
  #Proportion of infections that are severe
  prop_severe = 0,
  #Proportion of severe infections that have complications
  prop_complications = 0,
  #Natural protection waning
  natural_immunity_waning = 0,
  cfr_severe  = 0,
  #Vaccination
  vaccination_coverage = 0,
  tt_vaccination_coverage = 0,
  age_vaccination_beta_modifier = 0,
  #Vaccination waning
  short_term_waning = 0,
  long_term_waning = 0,
  
  #R0
  R0 = 0,
  tt_R0 = 0,
  cfr_normal = 0,
  user_specified_foi = 0,
  initial_FOI = 0,
  foi_turn_off_when_vaccinating = 0, 
  #Seeding parameters
  seeded = 0,
  #Define times when seeding occurs
  tt_seeded = 0,
  
  #Births, deaths and aging
  #Aging rate by compartment
  aging_rate = 0,
  #Set initial background death rate
  initial_background_death = 0,
  #Set simple births and deaths
  simp_birth_death = 0,
  #Changing birth rate if  simp_birth_death != 1
  tt_birth_changes = 0,
  tt_death_changes = 0,
  #Crude birth rate if simp_birth_death != 1
  crude_birth = 0,
  crude_death = 0,
  #Weight given to maternal protection
  protection_weight_vacc = 0,
  protection_weight_rec = 0,

  #Age group where maternal protection ends
  age_maternal_protection_ends = 1,
  #Reproductive ages
  repro_low = 1,
  repro_high = NULL,
  
  #Movement between risk groups
  tt_moving_risk = 0,
  moving_risk_values = 0,
  moving_risk_distribution_values = 0,
  
  #Migration
  tt_migration = 0,
  migration_in_number = 0,
  migration_distribution_values = 0,
  migration_represent_current_pop = 0,
  
  #Fitting parameters
  death_modifier = 1,
  fertility_modifier = 1,
  
  #Test
  test = F
  
){
  
  if (is.null(contact_matrix)) {
    contact_matrix <- matrix(1 / (n_age^2), nrow = n_age, ncol = n_age)
  }
  
  if (is.null(severe_recovery_rate)) {
    severe_recovery_rate <- recovery_rate
  }
  
  repro_high <- if (is.null(repro_high)) {
    n_age
  } else {
    repro_high
  }
  
  no_seeded_changes <- length(tt_seeded)

  seeded_dims <- c(n_age, n_vacc, n_risk, no_seeded_changes)
  seeded <- if (length(seeded) == 1) {
    array(seeded, dim = seeded_dims)
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, dim4 = no_seeded_changes, updates = seeded)
  }
  
  cfr_dims <- n_age
  cfr_normal <- if (length(cfr_normal) == 1) {
    array(cfr_normal, dim = cfr_dims)
  } else {
    array_from_df(dim1 = n_age, updates = cfr_normal)
  }
  
  cfr_severe <- if (length(cfr_severe) == 1) {
    array(cfr_severe, dim = cfr_dims)
  } else {
    array_from_df(dim1 = n_age, updates = cfr_severe)
  }
  
  initial_FOI <- if (length(initial_FOI) == 1) {
    array(initial_FOI, dim = n_age)
  } else {
    initial_FOI
  }

  prop_complications_dims <- n_age
  prop_complications <- if (length(prop_complications) == 1) {
    array(prop_complications, dim = prop_complications_dims)
  } else {
    array_from_df(dim1 = n_age, updates = prop_complications)
  }

  S0_dims <- c(n_age, n_vacc, n_risk)
  S0 <- if (length(S0) == 1) {
    array(S0, dim = S0_dims)
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, updates = S0 %>% select(dim1:dim3, value) %>% subset(value != 0))
  }
  
  Rpop0_dims <- c(n_age, n_vacc, n_risk)
  Rpop0 <- if (length(Rpop0) == 1) {
    array(Rpop0, dim = Rpop0_dims)
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, updates = Rpop0 %>% select(dim1:dim3, value) %>% subset(value != 0))
  }
  
  I0 <- if (length(I0) == 1) {
    array(I0, dim = S0_dims)
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, updates = I0)
  }
  
  prop_severe <- check_and_format_input(prop_severe, n_age, n_vacc, n_risk)
  
  no_vacc_changes <- length(tt_vaccination_coverage)
  age_vaccination_beta_modifier <- if (length(age_vaccination_beta_modifier) == 1) {
    array(age_vaccination_beta_modifier, dim = S0_dims)
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, updates = age_vaccination_beta_modifier)
  }
  
  short_term_waning <- if (length(short_term_waning) == 1) {
    array(short_term_waning, dim = c(n_vacc))
  } else {
    array_from_df(dim1 = n_vacc, updates = short_term_waning)
  }
  
  long_term_waning <- if (length(long_term_waning) == 1) {
    array(long_term_waning, dim = c(n_vacc))
  } else {
    array_from_df(dim1 = n_vacc, updates = long_term_waning)
  }
  
  vaccination_coverage <- if (length(vaccination_coverage) == 1) {
    array(vaccination_coverage, dim = c(n_age, n_vacc, n_risk, no_vacc_changes))
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, dim4 = no_vacc_changes, updates = vaccination_coverage)
  }
  
  no_moving_risk_changes <- length(tt_moving_risk)
  moving_risk_values <- if (length(moving_risk_values) == 1) {
    array(moving_risk_values, dim = c(n_age, n_vacc, n_risk, no_moving_risk_changes))
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, dim4 = no_moving_risk_changes, updates = moving_risk_values)
  }
  
  moving_risk_distribution_values <- if (length(moving_risk_distribution_values) == 1) {
    array(moving_risk_distribution_values, dim = c(n_age, n_vacc, n_risk, no_moving_risk_changes))
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, dim4 = no_moving_risk_changes, updates = moving_risk_distribution_values)
  }
  
  no_migration_changes <- length(tt_migration)
  migration_in_number <- if (length(migration_in_number) == 1) {
    array(migration_in_number, dim = c(n_age, n_vacc, n_risk, no_migration_changes))
  } else {
    array_from_df(dim1 = n_age, dim2 = n_vacc, dim3 = n_risk, dim4 = no_migration_changes, updates = migration_in_number)
  }
  
  migration_distribution_values <- if (length(migration_distribution_values) == 1) {
    array(migration_distribution_values, dim = c(6, no_migration_changes))
  } else {
    array_from_df(dim1 = 6, dim2 = no_migration_changes, updates = migration_distribution_values)
  }
  
  initial_background_death <- check_and_format_input(initial_background_death, n_age, n_risk)
  no_birth_changes <- length(tt_birth_changes)
  no_death_changes <- length(tt_death_changes)
  
  crude_birth <- if (length(crude_birth) == 1) {
    array(crude_birth, dim = c(n_risk, no_birth_changes))
  } else {
    array_from_df(dim1 = n_risk, dim2 = no_birth_changes, updates = crude_birth)
  }
  
  crude_death <- if (length(crude_death) == 1) {
    array(crude_death, dim = c(n_age, n_risk, no_death_changes))
  } else {
    array_from_df(dim1 = n_age, dim2 = n_risk, dim3 = no_death_changes, updates = crude_death)
  }

  aging_rate <- check_and_format_input(aging_rate, n_age)
  aging_rate[n_age] <- 0
  
  #R0
  no_R0_changes <- length(tt_R0)
  
  # Export list -------------------------------------------------------------
  
  export_list <- list(
    
    #Dimensions
    n_age = n_age,
    n_vacc = n_vacc,
    n_risk = n_risk,
    
    #Initial values
    S0 = S0,
    I0 = I0,
    Rpop0 = Rpop0,
    
    #Add in contact matrix
    contact_matrix = contact_matrix,
    
    #Disease parameters
    #Incubation rate
    incubation_rate = incubation_rate,
    #Recovery rate
    recovery_rate = recovery_rate,
    #Recovery rate for severe infections
    severe_recovery_rate = severe_recovery_rate,
    #Prop infections that are severe
    prop_severe = prop_severe,
    #Proportion of severe infections that have complications
    prop_complications = prop_complications,
    #Natural protection waning
    natural_immunity_waning = natural_immunity_waning,
    cfr_severe  = cfr_severe,
    #Vaccination
    vaccination_coverage = vaccination_coverage,
    tt_vaccination_coverage = tt_vaccination_coverage,
    no_vacc_changes = no_vacc_changes,
    age_vaccination_beta_modifier = age_vaccination_beta_modifier,
    short_term_waning = short_term_waning,
    long_term_waning = long_term_waning,
    cfr_normal = cfr_normal,
    #R0
    R0 = R0,
    tt_R0 = tt_R0,
    no_R0_changes = no_R0_changes,
    seeded = seeded,
    tt_seeded = tt_seeded,
    no_seeded_changes = no_seeded_changes, 
    
    initial_FOI = initial_FOI,
    user_specified_foi = user_specified_foi,
    
    #Births, deaths and aging
    # Aging rate by compartment
    aging_rate = aging_rate,
    # Set initial background death rate
    initial_background_death = initial_background_death,
    #Set simple births and deaths
    simp_birth_death = simp_birth_death,
    #Changing birth rate if  simp_birth_death != 1
    tt_birth_changes = tt_birth_changes,
    tt_death_changes = tt_death_changes,
    no_birth_changes = no_birth_changes,
    no_death_changes = no_death_changes,
    #Crude birth rate if simp_birth_death != 1
    crude_birth = crude_birth,
    crude_death = crude_death,

    #Weight given to maternal protection
    protection_weight_vacc = protection_weight_vacc,
    protection_weight_rec = protection_weight_rec,
    #Age group where maternal protection ends
    age_maternal_protection_ends = age_maternal_protection_ends,
    #Reproductive ages
    repro_low = repro_low,
    repro_high = repro_high,
    #Movement between risk groups
    tt_moving_risk = tt_moving_risk ,
    no_moving_risk_changes = no_moving_risk_changes, 
    moving_risk_values = moving_risk_values,
    moving_risk_distribution_values = moving_risk_distribution_values,
    cfr_normal,
    #Migration
    tt_migration = tt_migration,
    no_migration_changes = no_migration_changes,
    migration_in_number = migration_in_number,
    migration_distribution_values = migration_distribution_values,
    
    #Demographic fitting
    death_modifier = 1,
    fertility_modifier = 1,
    foi_turn_off_when_vaccinating = foi_turn_off_when_vaccinating,
    migration_represent_current_pop = migration_represent_current_pop
    
  )
  
  # Checks and balances -----------------------------------------------------
  if(test == T){
    
    #These must be above 0 and an integer
    above_zero_and_an_integer <- export_list[c("n_age", "n_vacc", "n_risk", "age_maternal_protection_ends", "repro_low", "repro_high")]
    
    #Check if the sum is above 0 and an integer
    sum_above_zero_and_an_integer <- export_list[c("S0")]
    
    #These must be non-negative and integers
    non_neg_int <- export_list[c("tt_vaccination_coverage", "no_vacc_changes", "tt_R0", "no_R0_changes", "tt_birth_changes", "tt_death_changes", "no_birth_changes", "no_death_changes", "repro_low", "repro_high", "I0", "seeded", "tt_seeded", "tt_moving_risk", "no_moving_risk_changes", "tt_migration", "no_migration_changes", "user_specified_foi", "foi_turn_off_when_vaccinating", "migration_represent_current_pop")]
    
    #These must be probabilities
    probability <- export_list[c("incubation_rate", "recovery_rate", "severe_recovery_rate", "prop_severe", "prop_complications", "vaccination_coverage", "age_vaccination_beta_modifier", "initial_background_death", "crude_birth", "crude_death", "protection_weight_vacc", "protection_weight_rec", "aging_rate", "natural_immunity_waning", "moving_risk_values", "moving_risk_distribution_values", "migration_distribution_values", "death_modifier", "fertility_modifier", "short_term_waning", "long_term_waning", "cfr_normal", "cfr_severe", "initial_FOI")]
    
    #Non-negative
    non_negative <- export_list[c("R0")]
    
    #Not above a reference value
    above_value_age <- export_list[c("repro_low", "repro_high")]
    
    #Must be an integer
    integer <- export_list[c("migration_in_number")]
    
    #Run checks
    a <- check_parameter(above_zero_and_an_integer, check_above_zero = T, check_integer = T)
    b <- check_parameter(probability, check_probability = T)
    c <- check_parameter(non_negative, check_non_negative = T)
    d <- check_parameter(above_value_age, check_above_zero = T, check_not_above_reference_value = n_age)
    e <- check_parameter(non_neg_int, check_non_negative = T, check_integer = T)
    f <- check_parameter(sum_above_zero_and_an_integer, check_if_sum_above_zero = T, check_integer = T)
    g <- check_parameter(integer, check_integer = T)
    
    #All tests
    all_failures <- rbind(a, b, c, d, e, f, g)
    
    if(nrow(all_failures) > 0) {
      warning("The following parameters failed checks:\n", paste(capture.output(print(all_failures, row.names = FALSE)), collapse = "\n"))
    } else export_list
  } else export_list
}