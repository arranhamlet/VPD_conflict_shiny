library(shiny)
library(tidyverse)
library(odin2)
library(dust2)
library(bslib)
library(DT)
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
  data.table
)

# Countries of interest
countries <- data.frame(
  name = c("Syria", "Palestine", "Yemen", "Nigeria", "Ethiopia", "Sudan",
           "Myanmar", "Papa New Guinea", "Afghanistan", "Venezuela", "Haiti", "Guatemala",
           "Chad", "Democratic Republic of the Congo", "Burkina Faso", "Somalia", "The United Kingdom"),
  iso = c("SYR", "PSE", "YEM", "NGA", "ETH", "SDN", "MMR", "PNG", "AFG", "VEN", "HTI", "GTM",
          "TCD", "DRC", "BFA", "SOM", "GBR")
)

# Diseases of interest
diseases_of_interest <- data.frame(
  disease = c("Diphtheria", "Measles", "Pertussis"),
  default_R0 = c(3, 15, 8), 
  max_R0 = c(7, 18, 15)
)

# Plot 1 ----------------------

# Poster Plot 1 on Demographics and susceptibility ---------

## Setup and Gather Data ------------
options(scipen = 999)
population_all <- import("data/processed/WPP/age_both.csv")

#Import functions
invisible(sapply(list.files("R/functions", full.names = T, pattern = ".R", recursive = T), function(x) source(x)))

#Import model
model <- odin2::odin("model/stochastic_model_v1.R")

plot_one <- function(country, n, disease, r0){
  
  iso3c <- countrycode::countrycode(country, "country.name.en", "iso3c")
  dis_match <- c("diphtheria", "measles", "pertussis")[match(disease, c("Diphtheria", "Measles", "Pertussis"))]
  
  #UN demographics
  population_all <- import("data/processed/WPP/age_both.csv")
  population <- subset(population_all, iso3 == iso3c) %>% 
    select(x0:x100) %>% 
    tail(1) %>%
    gather() %>%
    rename(age = key, population = value) %>%
    mutate(age = as.numeric(gsub("x", "", age)),
           population = population/sum(population)*n)
  
  #Prior vaccination coverage
  routine_vaccination_data <- import("data/coverage-data_updated.xlsx")
  routine_subset <- routine_vaccination_data %>%
    subset(CODE == iso3c & 
             grepl(disease, ANTIGEN_DESCRIPTION, ignore.case = T) & COVERAGE_CATEGORY == "WUENIC") %>%
    clean_names() %>%
    select(code, name, year, antigen_description, coverage)
  
  #Full case data
  prior_cases <- import("output/model_run/MSF/processed/full_cases.csv")
  
  #Full starting immunity
  starting_immunity <- import("output/model_run/MSF/processed/susceptibility.csv")
  starting_immunity <- starting_immunity %>%
    mutate(status = factor(
      status, levels = c("Susceptible", "Vaccine protected", "Exposure protected", "Vaccine and exposure protected")
    ))
  
  #Rdata files
  all_Rdata <- list.files("output/model_run/MSF/processed/", pattern = ".rds", full.names = T)
  all_Rdata_names <- sapply(strsplit(all_Rdata, "/"), function(e) gsub(".rds", "", last(e)))
  all_Rdata_loaded <- sapply(all_Rdata, function(x) import(x), simplify = FALSE)
  names(all_Rdata_loaded) <- all_Rdata_names
  
  ## Plotting ----------------
  ggthemr::ggthemr("fresh", text_size = 18)
  
  # Demographic plot
  demographics_gg <- ggplot(
    data = population,
    mapping = aes(x = age,
                  y = population)
  ) +
    geom_bar(stat = "identity", color = "#65ADC2", fill = "#65ADC2") +
    labs(x = "Age",
         y = "Population Size") +
    scale_y_continuous(expand = expansion(mult = c(0, .05)), labels = scales::comma) +
    #scale_x_continuous(expand = c(0, 0)) +
    theme(
      axis.text = element_text(size = 14), 
      text = element_text(color = "black", family = "Helvetica"),
      axis.title = element_text(size = 18), 
      axis.line = element_line(color = "black")
    ) 
  
  # Case plot
  full_disease_df <- import("data/processed/WHO/reported_cases_data.csv")
  
  cases_gg <- ggplot(
    data = full_disease_df %>%
      subset(iso3 == iso3c & disease_description == disease),
    mapping = aes(
      x = year,
      y = cases
    )
  ) +
    geom_bar(stat = "identity") +
    labs(x = "Year",
         y = "WHO Reported Annual \nConfirmed Cases") +
    scale_y_continuous(expand = expansion(mult = c(0, .05)), labels = scales::comma) +
    scale_x_continuous(limits = c(1995, 2024)) +
    theme(
      axis.text = element_text(size = 14), 
      text = element_text(color = "black", family = "Helvetica"),
      axis.title = element_text(size = 18), 
      axis.line = element_line(color = "black")
    )
  
  protection_cols <- c(
    "Vaccine-derived Immunity" = "#A8DADC",       # soft teal
    "Infection-derived Immunity" = "#457B9D",     # muted blue
    "Vaccine and Infection-derived Immunity" = "#F1FAEE", # off-white
    "Susceptible" = "#E63946"                     # strong red
  )
  
  
  vaccination_gg <- ggplot(
    data = routine_subset,
    mapping = aes(
      x = year,
      y = coverage,
      color = antigen_description
    )
  ) +
    geom_line(lwd = 1.5) +
    guides(color = guide_legend(nrow = 1, title = "")) +
    labs(
      x = "Year",
      y = "WHO Reported Annual \nVaccination Coverage (%)"
    ) +
    theme(
      legend.position = "top",
    ) +
    coord_cartesian(
      ylim = c(0, 100)
    ) +
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
      filter(R0 == r0) %>% 
      mutate(status = recode(status, 
                             "Vaccine protected" = "Vaccine-derived Immunity", 
                             "Exposure protected" = "Infection-derived Immunity",
                             "Vaccine and exposure protected" = "Vaccine and Infection-derived Immunity")) %>%
      mutate(status = factor(status, levels = names(protection_cols))) %>% 
      subset(iso == iso3c & disease == dis_match),
    mapping = aes(
      x = as.numeric(age),
      y = prop,
      fill = status
    )
  ) +
    geom_bar(
      stat = "identity",
      width = 1
    ) +
    labs(
      x = "Age",
      y = "Proportion of Population Susceptible",
      fill = ""
    ) +
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
  
  
  patch <- demographics_gg/ cases_gg/ vaccination_gg
  total_information <- ggarrange(patch, susceptibility_gg, ncol = 2, widths = c(1, 1))
  
  return(total_information)
  
}
