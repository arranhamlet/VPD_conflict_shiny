library(shiny)
library(tidyverse)
library(odin2)
library(dust2)

#Countries of interest
countries <- data.frame(
  name = c("Syria", "Palestine", "Yemen",
           "Nigeria", "Ethiopia", "Sudan",
           "Myanmar", "Papa New Guinea", "Afghanistan",
           "Venezuela", "Haiti", "Guatemala"),
  iso = c("SYR", "PSE", "YEM",
          "NGA", "ETH", "SDN",
          "MMR", "PNG", "AFG",
          "VEN", "HTI", "GTM")
)

diseases_of_interest <- data.frame(
  disease = c("Diphtheria", "Measles", "Pertussis"),
  default_R0 = c(15, 12, 3)
)

#Set up pages
pageWithSidebar(
  headerPanel("Model set up"),
  sidebarPanel(
    selectInput("country", "Country", countries$name),
    numericInput("popsize", "Population size", 1000, min = 1, max = 2000000000),
    selectInput("disease", "Disease of interest", diseases_of_interest$disease, selected = "Diphtheria"),
    uiOutput("r0_input"),
    sliderInput("years", "Years of simulation", 1, min = 1, max = 5),
    DTOutput("input_coverage_table")
  )
)