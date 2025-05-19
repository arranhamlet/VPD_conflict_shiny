
# Load required libraries
library(shiny)
library(bslib)
library(DT)

# Source helper functions and data
source("R/source_script.R")

# Define theme
jameel_theme <- bs_theme(
  version = 5,
  base_font = "Imperial Sans",
  heading_font = "Imperial Sans Display",
  primary = "#0000cd",
  secondary = "#ab1940",
  success = "#0000ae",
  bg = "#fdfdfd",
  fg = "#021c35",
  font_scale = 1
)

# Add font CSS
jameel_theme <- bs_add_rules(jameel_theme, "
  @font-face {
    font-family: 'Imperial Sans';
    src: url('ImperialSansText-Regular.ttf') format('truetype');
    font-weight: normal;
    font-style: normal;
  }
  @font-face {
    font-family: 'Imperial Sans Display';
    src: url('ImperialSansDisplay-Medium.ttf') format('truetype');
    font-weight: 500;
    font-style: normal;
  }
")

# UI
ui <- page_sidebar(
  title = div(
    class = "d-flex align-items-center",
    img(src = "imperial_ji_logo.png", height = "50px", style = "margin-right: 15px;"),
    div(h4("Jameel Institute Crisis Vaccination Planner (JICVP)",
           class = "mb-0", style = "font-weight: 500; color: #0000cd;"))
  ),
  theme = jameel_theme,
  sidebar_width = "420px",
  sidebar = sidebar(  # ✅ THIS IS THE KEY FIX
    width = '20%',
    div(
      class = "sidebar-custom p-3 bg-light border rounded",
      style = "display: flex; flex-direction: column; height: auto; overflow: visible;",
      selectInput("country", "Country", countries$name, selected =  "Nigeria") %>%
        tooltip("Country determines demographics and contact patterns."),
      numericInput("popsize", "Population size", 10000, min = 1, max = 2e9) %>%
        tooltip("This is the population size you want to run the simulation on."),
      
      selectInput("disease", "Disease of interest", diseases_of_interest$disease, selected = "Diphtheria") %>%
        tooltip("Disease determines transmission patterns and vaccination, both historic and future."),
      uiOutput("r0_input"),
      sliderInput("years", "Years of simulation", 1, min = 1, max = 5) %>%
        tooltip("How many years you want to simulate into the future."),
      
      h4("Future events"),
      DTOutput("input_coverage_table") %>%
        tooltip("Here you can input different introductions of diease, and future vaccination activities, relative to current levels."),
      
      div(style = "margin-top: 15px;"),
      actionButton("run_model", "Run simulations",
                   icon("play"),
                   style = "color: #fff; background-color: #ab1940; border-color: #021c35") %>%
        tooltip("When you are ready, click go!")
    )
  ),
  navset_card_underline(
    nav_panel("Model Setup", h4("Main panel output will go here.")),
    nav_panel("Model outputs", h4("Results tab content here.")),
    nav_panel("Methods", h4("Methods tab content here."))
  )
)


# Server
server <- function(input, output, session) {
  last_n <- reactiveVal(3)
  
  current_data <- reactiveVal(
    data.frame(Year = 0:2, seed = rep(0, 3), `Vaccination coverage` = rep(0, 3))
  )
  
  observeEvent(input$years, {
    n_new <- input$years
    n_old <- last_n()
    df_old <- current_data()
    
    if (n_new > n_old) {
      added_rows <- data.frame(
        Year = n_old:(n_new - 1),
        seed = rep(0, n_new - n_old),
        `Vaccination coverage` = rep(0, n_new - n_old)
      )
      df_new <- rbind(df_old, added_rows)
    } else {
      df_new <- head(df_old, n_new)
    }
    
    current_data(df_new)
    last_n(n_new)
  })
  
  output$input_coverage_table <- renderDT({
    datatable(
      current_data(),
      editable = list(target = "cell", disable = list(columns = c(0))),
      rownames = FALSE,
      colnames = c("Year", "Seeded infections", "Vaccination relative to starting (%)"),
      options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        scrollX = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0:2),
          list(className = 'dt-head-wrap', targets = "_all")
        )
      ),
      escape = FALSE
    )
  }, server = FALSE)
  
  observeEvent(input$input_coverage_table_cell_edit, {
    info <- input$input_coverage_table_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- suppressWarnings(as.numeric(info$value))
    
    if (!is.na(v)) {
      df <- current_data()
      if (j == 2 && v >= 0) df[i, j] <- v
      if (j == 3 && v >= 0 && v <= 100) df[i, j] <- v
      current_data(df)
    }
  })
  
  output$r0_input <- renderUI({
    selected_disease <- input$disease
    default_r0 <- diseases_of_interest$default_R0[
      diseases_of_interest$disease == selected_disease
    ]
    min_R0 <- diseases_of_interest$min_R0[
      diseases_of_interest$disease == selected_disease
    ]
    max_R0 <- diseases_of_interest$max_R0[
      diseases_of_interest$disease == selected_disease
    ]
    
    sliderInput("r0", "Basic reproductive number (R0)", value = default_r0, min = min_R0, max = max_R0) %>%
      tooltip("R0 determines how infectious your disease is.")
  })
}

# Run the app
shinyApp(ui, server)
