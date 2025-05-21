library(shiny)
library(tidyverse)
library(odin2)
library(dust2)
library(bslib)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(later)
library(shinydashboard)

#Load packages
pacman::p_load(
  odin2,
  rio,
  here,
  dust2,
  tidyverse,
  reshape2,
  collapse,
  janitor,
  ggpubr,
  patchwork,
  squire.page,
  patchwork,
  data.table,
  ggthemr
)

## Setup and Gather Data ------------
options(scipen = 999)
population_all <- import("data/processed/WPP/age_both.csv")
routine_vaccination_data <- import("data/coverage-data_updated.xlsx")
prior_cases <- import("output/model_run/MSF/processed/full_cases.csv")
starting_immunity <- import("output/model_run/MSF/processed/susceptibility.csv")

# Case plot
full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")

#Rdata files
all_Rdata_loaded <- readRDS("output/model_run/MSF/processed/full_rds.rds")

# Countries of interest
data_available <- t(Reduce(data.frame, strsplit(names(all_Rdata_loaded ), "_"))) %>%
  as.data.frame() %>%
  setnames(
    c("iso", "disease", "R0")
  ) %>%
  mutate(country = countrycode::countrycode(
    sourcevar = iso,
    origin = "iso3c",
    destination = "country.name.en"
  )) %>%
  mutate(
    country = case_when(
      country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      country == "Myanmar (Burma)" ~ "Myanmar",
      country == "Palestinian Territories" ~ "Palestine",
      TRUE ~ country
    )
  )

countries <- data.frame(
  name = unique(data_available$country),
  iso = unique(data_available$iso)
)

# Diseases of interest
diseases_of_interest <- data.frame(
  disease = c("Diphtheria", "Measles", "Pertussis"),
  default_R0 = c(4, 15, 12), 
  max_R0 = c(7, 18, 15),
  min_R0 = c(1, 12, 9)
)


#Import functions
invisible(sapply(list.files(
  "R/functions",
  full.names = T,
  pattern = ".R",
  recursive = T
), function(x)
  source(x)))

#Import model
model <- odin2::odin("model/stochastic_model_v1.R")

# Plot 1 ----------------------

# Poster Plot 1 on Demographics and susceptibility ---------

plot_one <- function(country, n, disease, r0, vertical = F) {
  iso3c <- countrycode::countrycode(country, "country.name.en", "iso3c")
  dis_match <- c("diphtheria", "measles", "pertussis")[match(disease, c("Diphtheria", "Measles", "Pertussis"))]
  
  #UN demographics
  population <- subset(population_all, iso3 == iso3c) %>%
    select(x0:x100) %>%
    tail(1) %>%
    gather() %>%
    rename(age = key, population = value) %>%
    mutate(age = as.numeric(gsub("x", "", age)),
           population = population / sum(population) * n)
  
  #Prior vaccination coverage
  routine_subset <- routine_vaccination_data %>%
    subset(
      CODE == iso3c &
        grepl(disease, ANTIGEN_DESCRIPTION, ignore.case = T) &
        COVERAGE_CATEGORY == "WUENIC"
    ) %>%
    clean_names() %>%
    select(code, name, year, antigen_description, coverage)
  
  #Full case data
  
  #Full starting immunity
  starting_immunity <- starting_immunity %>%
    subset(iso == iso3c & disease == dis_match & R0 == r0) %>%
    mutate(status = factor(
      status,
      levels = c(
        "Susceptible",
        "Vaccine protected",
        "Exposure protected",
        "Vaccine and exposure protected"
      )
    ))
  
  ## Plotting ----------------
  ggthemr::ggthemr("fresh", text_size = 18)
  
  # Demographic plot
  demographics_gg <- ggplot(data = population, mapping = aes(x = age, y = population)) +
    geom_bar(stat = "identity",
             color = "#65ADC2",
             fill = "#65ADC2") +
    labs(x = "Age", y = "Population Size") +
    scale_y_continuous(expand = expansion(mult = c(0, .05)), labels = scales::comma) +
    #scale_x_continuous(expand = c(0, 0)) +
    theme(
      axis.text = element_text(size = 14),
      text = element_text(color = "black", family = "Helvetica"),
      axis.title = element_text(size = 18),
      axis.line = element_line(color = "black")
    )
  
  cases_gg <- ggplot(
    data = full_disease_df %>%
      subset(iso3 == iso3c & disease_description == disease),
    mapping = aes(x = year, y = cases)
  ) +
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "WHO Reported Annual \nConfirmed Cases") +
    scale_y_continuous(expand = expansion(mult = c(0, .05)), labels = scales::comma) +
    scale_x_continuous(limits = c(1995, 2024)) +
    theme(
      axis.text = element_text(size = 14),
      text = element_text(color = "black", family = "Helvetica"),
      axis.title = element_text(size = 18),
      axis.line = element_line(color = "black")
    )
  
  protection_cols <- c(
    "Vaccine-derived Immunity" = "#A8DADC",
    # soft teal
    "Infection-derived Immunity" = "#457B9D",
    # muted blue
    "Vaccine and Infection-derived Immunity" = "#F1FAEE",
    # off-white
    "Susceptible" = "#E63946"                     # strong red
  )
  
  
  vaccination_gg <- ggplot(
    data = routine_subset,
    mapping = aes(x = year, y = coverage, color = antigen_description)
  ) +
    geom_line(lwd = 1.5) +
    guides(color = guide_legend(nrow = 2, title = "")) +
    labs(x = "Year", y = "WHO Reported Annual \nVaccination Coverage (%)") +
    theme(legend.position = "top", ) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_continuous(limits = c(1995, 2024)) +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      text = element_text(color = "black", family = "Helvetica"),
      axis.line = element_line(color = "black")
    )
  
  susceptibility_gg <- ggplot(
    data = starting_immunity %>%
      mutate(
        status = recode(
          status,
          "Vaccine protected" = "Vaccine-derived Immunity",
          "Exposure protected" = "Infection-derived Immunity",
          "Vaccine and exposure protected" = "Vaccine and Infection-derived Immunity"
        )
      ) %>%
      mutate(status = factor(status, levels = names(protection_cols))) %>%
      subset(iso == iso3c & disease == dis_match),
    mapping = aes(x = as.numeric(age), y = prop, fill = status)
  ) +
    geom_bar(stat = "identity", width = 1) +
    labs(x = "Age", y = "Proportion of Population Susceptible", fill = "") +
    guides(fill = guide_legend(nrow = 2, title = "")) +
    theme(
      legend.position = "top",
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      legend.spacing = unit(2, "cm"),
      text = element_text(color = "black", family = "Helvetica"),
      panel.grid.major = element_line(colour = "grey80"),
      axis.line = element_line(color = "black")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = protection_cols)
  
  if(vertical == T){
    total_information <- (vaccination_gg + cases_gg + demographics_gg) / (susceptibility_gg +
                                                                            theme(legend.position = "right")) +
      guides(fill = guide_legend(ncol = 1))
  } else {
    total_information <- (vaccination_gg / cases_gg / demographics_gg) | susceptibility_gg
  }
  
  return(total_information)
  
}


# Plot 2 -----------------------------
generate_model_data <- function(country, n, disease, r0, user_df){
  # get the iso3c and disease match
  iso3c <- countrycode::countrycode(country, "country.name.en", "iso3c")
  dis_match <- c("diphtheria", "measles", "pertussis")[match(disease, c("Diphtheria", "Measles", "Pertussis"))]
  
  # Pull starting parameters
  param_use <- all_Rdata_loaded[[which(names(all_Rdata_loaded) == paste(c(iso3c, dis_match, r0), collapse = "_"))]]
  
  # Create our params for the simulation
  generate_params <- function(param_use, user_df, years, update_vacc = FALSE){
    
    pars <- param_use
    
    # create fwd vaccination
    new_vaccination <- abind::abind(replicate(years, pars$vaccination_coverage, simplify = FALSE),
                                    along = 4)
    
    # and for intervention
    if (update_vacc) {
      for (i in seq_along(user_df$Vaccination.coverage)) {
        new_vaccination[, 1:3, 1, i] <- pars$vaccination_coverage[, 1:3, 1, 1]  * user_df$Vaccination.coverage[i]
      }
    }
    
    # update our parameters
    pars$vaccination_coverage <- new_vaccination
    pars$tt_vaccination_coverage <- seq(0, by = 365, length.out = years)
    pars$no_vacc_changes <- length(pars$tt_vaccination_coverage)
    
    # create fwd seeds
    new_seeded <- abind::abind(replicate(years, pars$seeded, simplify = FALSE), along = 4)
    for (i in seq_along(user_df$seed)) {
      new_seeded[18, 1, 1, i] <- user_df$seed[i]
    }
    
    # update seed parameters
    pars$tt_seeded <- pars$tt_seeded <- seq(0, by = 365, length.out = years)
    pars$seeded <- new_seeded
    pars$no_seeded_changes <- length(pars$tt_seeded)
    
    # create our total population
    total_pop <- sum(pars$S0 + pars$I0 + pars$Rpop0)
    scaler <- n / total_pop
    pars$S0 <- pars$S0 * scaler
    pars$I0 <- pars$I0 * scaler
    pars$Rpop0 <- pars$Rpop0 * scaler
    
    # return parameters
    return(pars)
    
  }
  
  # Run number and length
  n_runs <- 3
  years <- nrow(user_df)
  
  # parameters for simulations
  no_change_params <- generate_params(param_use, user_df, years, FALSE)
  user_params <- generate_params(param_use, user_df, years, TRUE)
  
  #Run model
  model_no_change <- run_model(
    odin_model = model,
    params = no_change_params,
    time = 365 * years,
    no_runs = n_runs
  )
  model_no_change$version <- "No change"
  
  model_change <- run_model(
    odin_model = model,
    params = user_params,
    time = 365 * years,
    no_runs = n_runs
  )
  model_change$version <- "Reduction in coverage"
  
  # combine models
  combo <- rbind(model_no_change, model_change)
  
  combo
  
}

plot_two <- function(combo, vertical = F) {
  
  #Calculate susceptibility
  new_cases_go <- combo %>%
    subset(age == "All" & state == "new_case") %>%
    group_by(time, version) %>%
    summarise(
      value_min = get_95CI(x = value, type = "low"),
      value_max = get_95CI(x = value, type = "high"),
      value = median(value)
    ) %>%
    mutate(value_min = pmax(value_min, 0))
  
  #Summary statistics
  # TODO turn these into buttons and text in final out
  sum_stats <- combo %>%
    subset(age == "All" & state == "new_case") %>%
    group_by(run, version) %>%
    summarise(value = sum(value)) %>%
    mutate(over_100 = value > 100)
  
  sum_stats_size <- sum_stats %>%
    group_by(version) %>%
    summarise(
      value_min = pmax(get_95CI(x = value, type = "low"), 0),
      value_max = pmax(get_95CI(x = value, type = "high"), 0),
      value = mean(value),
    )
  
  # Create Case Plot
  ggthemr::ggthemr("fresh", text_size = 18)
  case_diff <- ggplot(
    data = sum_stats_size,
    mapping = aes(
      x = version,
      y = value,
      ymin = value_min,
      ymax = value_max,
      color = version
    )
  ) +
    geom_point() +
    geom_errorbar() +
    theme(
      axis.text = element_text(size = 14),
      text = element_text(color = "black", family = "Helvetica"),
      axis.title = element_text(size = 18),
      axis.line = element_line(color = "black")
    ) +
    labs(x = "", y = "Total Cases", color = "") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = c(
      "No change" = "gray30",
      "Reduction in coverage" = "#E84646"
    )) +
    theme(legend.position = "none", axis.text.x = element_blank())
  
  
  # And get the outbreak plot
  new_cases_go$year  <- as.Date((
    as.Date("2024-01-01") + lubridate::dyears(new_cases_go$time / 365)
  ))
  
  # setting to these names as clearer
  new_cases_go$version <- recode(new_cases_go$version,
                                 "No change" = "Pre-Conflict Vaccination Coverage",
                                 "Reduction in coverage" = "Routine Immunisation Stopped")
  outbreak_plot <- ggplot(
    data = new_cases_go,
    mapping = aes(
      x = year,
      y = value,
      ymin = value_min,
      ymax = value_max,
      color = version,
      fill = version
    )
  ) +
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.25) +
    theme(
      axis.text = element_text(size = 14),
      text = element_text(color = "black", family = "Helvetica"),
      axis.title = element_text(size = 18),
      axis.line = element_line(color = "black")
    ) +
    labs(
      x = "Year",
      y = "Daily Case Incidence",
      fill = "",
      color = ""
    ) +
    scale_x_date(date_labels = "%Y") +
    scale_y_continuous(expand = expansion(mult = c(0, .05)), labels = scales::comma) +
    theme(legend.position = "top") +
    scale_color_manual(
      values = c(
        "Pre-Conflict Vaccination Coverage" = "gray30",
        "Routine Immunisation Stopped" = "tomato1"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Pre-Conflict Vaccination Coverage" = "gray30",
        "Routine Immunisation Stopped" = "tomato1"
      )
    )
  
  combo_plot <- outbreak_plot +
    inset_element(case_diff, 0.625, 0.4, .99, .9)
  
  # Poster Plot 2a on Declines ---------
  susceptibility_data_all <- subset(combo, state %in% c("S", "E", "I", "R", "Is", "Rc") &
                                      age != "All") %>%
    mutate(
      status = case_when(
        state == "S" & vaccination == 1 ~ "Susceptible",
        state == "S" & vaccination > 1 ~ "Vaccine protected",
        state %in% c("S", "E", "I", "R", "Is", "Rc") &
          vaccination == 1 ~ "Exposure protected",
        state %in% c("S", "E", "I", "R", "Is", "Rc") &
          vaccination > 1 ~ "Vaccine and exposure protected"
      ),
      status = factor(
        status,
        levels = c(
          "Susceptible",
          "Vaccine protected",
          "Exposure protected",
          "Vaccine and exposure protected"
        )
      )
    ) %>%
    group_by(time, version, status) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    group_by(time, version) %>%
    mutate(
      coverage = value / sum(value, na.rm = T),
      coverage = case_when(is.nan(coverage) ~ 0, !is.nan(coverage) ~ coverage)
    )
  
  #Plot
  vac_protect_all <- susceptibility_data_all %>%
    mutate(status_simple = case_when(
      grepl("Vaccine", status) ~ "Vaccinated",!grepl("Vaccine", status) ~ "Unvaccinated"
    )) %>%
    group_by(time, version, status_simple) %>%
    summarise(value = sum(value)) %>%
    group_by(time, version) %>%
    mutate(prop = value / sum(value))
  
  
  vac_protect_all$year  <- as.Date((
    as.Date("2024-01-01") + lubridate::dyears(vac_protect_all$time / 365)
  ))
  
  range <- range(vac_protect_all %>%
                   subset(status_simple == "Vaccinated" & version == "Reduction in coverage") %>%
                   pull(prop) * 100)
  
  percent_vaccinated <- ggplot(
    data = vac_protect_all %>%
      subset(version == "Reduction in coverage"),
    mapping = aes(
      x = year,
      y = prop * 100,
      fill = status_simple,
      color = status_simple
    )
  ) +
    geom_bar(stat = "identity", width = 1) +
    theme(
      axis.text = element_text(size = 14),
      text = element_text(color = "black", family = "Helvetica"),
      axis.title = element_text(size = 18),
      axis.line = element_line(color = "black"),
      legend.position = "top",
    ) +
    scale_x_date(date_labels = "%Y") +
    scale_y_continuous(expand = expansion(mult = c(0, .05)), labels = scales::comma) +
    scale_fill_manual(values = ggthemr::swatch()[c(4, 2)]) +
    scale_color_manual(values = ggthemr::swatch()[c(4, 2)]) +
    labs(
      fill = "",
      color = "",
      x = "Year",
      y = "Population Vaccination Coverage (%)"
    ) + 
    geomtextpath::geom_texthline(
      yintercept = subset(
        vac_protect_all,
        status_simple == "Vaccinated" &
          version == "Reduction in coverage" &
          time == 1
      ) %>% pull(prop) * 100,
      linetype = "dashed",
      label = "Pre-Conflict Vacination Coverage",
      family = "Helvetica",
      size = 4,
      color = "black"
    ) +
    coord_cartesian(ylim = c(min(range), max(range)))
  
  p1 <- percent_vaccinated + theme(plot.margin = margin(5, 5, 5, 5, "pt"))
  p2 <- combo_plot + theme(plot.margin = margin(5, 5, 5, 5, "pt"))
  
  if(vertical == F){
    total_outbreaks <- p1 + p2 + plot_layout(widths = c(1, 1.4))
  } else {
    total_outbreaks <- (p1 / p2) 
  }
  
  return(total_outbreaks)
  
}

summary_stats <- function(combo){
  
  susceptibility_data_all <- subset(combo, 
                                    time %in% c(1, max(time)) & 
                                      state %in% c("S", "E", "I", "R", "Is", "Rc") &
                                      age != "All") %>%
    mutate(
      status = case_when(
        state == "S" & vaccination == 1 ~ "No vaccine",
        state == "S" & vaccination > 1 ~ "Vaccine protected",
        state %in% c("S", "E", "I", "R", "Is", "Rc") &
          vaccination == 1 ~ "No vaccine",
        state %in% c("S", "E", "I", "R", "Is", "Rc") &
          vaccination > 1 ~ "Vaccine protected"
      ),
      status = factor(
        status,
        levels = c(
          "Vaccine protected",
          "No vaccine"
        )
      )
    ) %>%
    group_by(time, version, status) %>%
    summarise(value = sum(value)) %>%
    group_by(time, version) %>%
    mutate(
      coverage = value / sum(value, na.rm = T),
      coverage = case_when(is.nan(coverage) ~ 0, !is.nan(coverage) ~ coverage)
    )
  
  diff <- subset(susceptibility_data_all, version %in% c("Reduction in coverage") & status == "Vaccine protected")
  diff_percent <- round(subset(diff, time == min(time)) %>% pull(coverage) - subset(diff, time == max(time)) %>% pull(coverage) * 100, 1)
  
  direction <- ifelse(diff_percent >= 0, "increase", "decrease")
  
  button_one <- paste0(
    abs(diff_percent), "% ", direction, " in the\npopulation protected by vaccination."
  )
  
  #Calculate susceptibility
  new_cases_go <- combo %>%
    subset(age == "All" & state == "new_case") %>%
    group_by(time, version) %>%
    summarise(
      value_min = get_95CI(x = value, type = "low"),
      value_max = get_95CI(x = value, type = "high"),
      value = median(value)
    ) %>%
    mutate(value_min = pmax(value_min, 0)) %>%
    group_by(version) %>%
    summarise(
      value = sum(value),
      value_min = sum(value_min),
      value_max = sum(value_max)
    )
  
  reduction <- subset(new_cases_go, version == "Reduction in coverage")
  status_quo <- subset(new_cases_go, version == "No change")
  direction <- ifelse(reduction$value >= status_quo$value, "increase", "decrease")
  
  button_two <- gsub("NaN", "0",
                      paste0(round(reduction$value/status_quo$value * 100, 1), 
                             "% (95% CI ", round(reduction$value_min/status_quo$value_min * 100, 1),
                             "-", round(reduction$value_max/status_quo$value_max * 100, 1), 
                             ")\n", direction, " in cases."))
  
  c(button_one,
    button_two)
  
}
