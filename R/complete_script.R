
# Import functions and data through source script -------------------------

source("R/source_script.R")


# UI ----------------------------------------------------------------------


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

# Add @font-face CSS rules for custom fonts in /www/
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


ui <- fluidPage(
  # theme = jameel_theme,
  theme = bs_theme(present = "litera"),
  # Custom styling for sidebar + DataTable scroll
  tags$head(
    tags$style(HTML("
    .sidebar-custom {
      font-size: 0.85rem;
    }
    .sidebar-custom .form-control,
    .sidebar-custom .selectize-input {
      font-size: 0.85rem;
    }
    .sidebar-custom h4 {
      font-size: 1rem;
      margin-top: 1rem;
    }
    .dataTables_wrapper {
      width: 100% !important;
    }
    .dataTables_scrollBody {
      max-height: 200px !important;
      overflow-y: auto !important;
    }
    .dt-head-wrap {
      white-space: normal !important;
      word-wrap: break-word;
    }
  "))
  ),
  div(
    class = "d-flex align-items-center p-3 mb-3 border-bottom",
    img(src = "imperial_ji_logo.png", height = "50px", style = "margin-right: 15px;"),
    div(
      h2("Jameel Institute Crisis Vaccination Planner (JICVP)", class = "mb-0", style = "font-weight: 500; color: #0000cd;")
    )
  ),
  navset_card_underline(
    nav_panel("Model Setup",
              layout_columns(
                col_widths = c(2, 8),
                
                # Sidebar
                div(
                  class = "sidebar-custom p-3 bg-light border rounded",
                  
                  # h4("Demographic characteristics"),
                  selectInput("country", "Country", countries$name),
                  numericInput("popsize", "Population size", 1000, min = 1, max = 2e9),
                  
                  # h4("Disease parameters"),
                  selectInput("disease", "Disease of interest", diseases_of_interest$disease, selected = "Diphtheria"),
                  uiOutput("r0_input"),
                  
                  # h4("Years of simulation"),
                  sliderInput("years", "Years of simulation", 1, min = 1, max = 5),
                  
                  h4("Future events"),
                  div(style = "height: 220px;",
                      DTOutput("input_coverage_table")
                  ),
                  
                  actionButton("run_model", "Run simulations",
                               icon("play"), 
                               style="color: #fff; background-color: #ab1940; border-color: #021c35")
                  
                ),
                
                # Main content
                div(
                  class = "p-3",
                  h4("Main panel output will go here.")
                )
              )
    ),
    
    nav_panel("Model outputs",
              h4("Results tab content here.")
    ),
    
    nav_panel("Methods",
              h4("Methods tab content here.")
    )
  )
)

# Server ------------------------------------------------------------------

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
      colnames = c("Year", "Seeded infections", "Vaccination <br>relative to starting (%)"),
      options = list(
        dom = 't',
        ordering = FALSE,
        scrollY = 200,
        paging = FALSE,
        scroller = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0:1),
          list(width = '50px', targets = 0),  # narrower Year column
          list(width = '200px', targets = 1), # allow coverage column more space
          list(className = 'dt-head-wrap', targets = "_all")  # wrap all headers
        )
      ),
      escape = FALSE  # needed to allow <br> in colnames
    )
  }, server = FALSE)
  
  observeEvent(input$input_coverage_table_cell_edit, {
    info <- input$input_coverage_table_cell_edit
    i <- info$row
    j <- info$col + 1  # DT is 0-indexed, R is 1-indexed
    v <- suppressWarnings(as.numeric(info$value))
    
    if (!is.na(v)) {
      df <- current_data()
      
      # Column 2: seed (no upper bound)
      if (j == 2 && v >= 0) {
        df[i, j] <- v
      }
      
      # Column 3: vaccination coverage (0–100%)
      if (j == 3 && v >= 0 && v <= 100) {
        df[i, j] <- v
      }
      
      current_data(df)
    }
  })
  
  output$r0_input <- renderUI({
    selected_disease <- input$disease
    default_r0 <- diseases_of_interest$default_R0[
      diseases_of_interest$disease == selected_disease
    ]
    
    numericInput("r0", "Basic reproductive number (R0)", value = default_r0)
  })
}

shinyApp(ui, server)
