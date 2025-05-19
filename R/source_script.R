library(shiny)
library(tidyverse)
library(odin2)
library(dust2)
library(bslib)
library(DT)

# Countries of interest
countries <- data.frame(
  name = c("Syria", "Palestine", "Yemen", "Nigeria", "Ethiopia", "Sudan",
           "Myanmar", "Papa New Guinea", "Afghanistan", "Venezuela", "Haiti", "Guatemala",
           "Chad", "Democratic Republic of the Congo", "Burkina Faso", "Somalia", "The United Kingdom"),
  iso = c("SYR", "PSE", "YEM", "NGA", "ETH", "SDN", "MMR", "PNG", "AFG", "VEN", "HTI", "GTM",
          "TCD", "DRC", "BFA", "SOM", "GBR")
) %>%
  subset(
    iso %in% c("SYR", "PSE", "YEM",
               "NGA", "ETH", "SDN")
  )

# Diseases of interest
diseases_of_interest <- data.frame(
  disease = c("Pertussis", "Measles", "Diphtheria"),
  default_R0 = c(12, 15, 4),
  min_R0 = c(9, 12, 1),
  max_R0 = c(15, 18, 7)
)