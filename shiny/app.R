

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

# create one uniqu sidebar panel here first:
# Sidebar
create_sidebar_panel <- function() {
  div(
    class = "sidebar-custom p-3 bg-light border rounded",
    style = "display: flex; flex-direction: column; height: auto;",
    
    selectInput("country", "Country", selected = "Palestine", choices = countries$name),
    uiOutput("pop_input"),
    selectInput("disease", "Disease of interest", choices = diseases_of_interest$disease, selected = "Measles"),
    uiOutput("r0_input"),
    
    sliderInput("years", "Years of simulation", 3, min = 1, max = 5),
    
    h4("Future events"),
    div(
      style = "display: flex; flex-direction: column;",
      DTOutput("input_coverage_table"),
      div(style = "margin-top: 15px;"),
      actionButton("run_model", "Run simulations",
                   icon("play"), 
                   style = "color: #fff; background-color: #ab1940; border-color: #021c35")
    )
  )
}

ui <- fluidPage(
  # theme = jameel_theme,
  theme = bs_theme(present = "litera"),
  # Custom styling for sidebar + DataTable scroll
  tags$head(
    tags$style(HTML("
      .sidebar-custom {
        font-size: 0.85rem;
        min-width: 215px;  /* Add this to enforce minimum sidebar width */
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
      .value-box-title {
        display: none !important;
      }
      .dataTables_scrollBody {
        max-height: 200px !important;
        overflow-y: auto !important;
      }
      .dt-head-wrap {
        white-space: normal !important;
        word-wrap: break-word;
      }
      #sidebar-panel {
        resize: horizontal;
        overflow: auto;
        min-width: 215px;
        max-width: 500px;
      }")),
    tags$script('
      $(document).on("shiny:connected", function(e) {
        function updateDimensions() {
          // Get the element by its selector
          const element = document.querySelector(".card.bslib-card.bslib-mb-spacing.html-fill-item.html-fill-container");

          if (element) {
            const elementRect = element.getBoundingClientRect();
            const elementWidth = elementRect.width;
            const elementHeight = elementRect.height;
            const windowHeight = window.innerHeight;

            // Calculate 80% of window height
            const eightyPercentWindowHeight = windowHeight * 0.85;

            // Determine the minimum height
            const calculatedHeight = Math.min(elementHeight, eightyPercentWindowHeight);

            // Send the dimensions to Shiny
            Shiny.setInputValue("dimension", [elementWidth, calculatedHeight]);
          } else {
            console.error("Element not found with the specified selector.");
          }
        }

        // Initial dimension setting
        updateDimensions();

        // Resize listener safely set after shiny:connected
        $(window).on("resize", function() {
          updateDimensions();
        });
      });')
  ),
  div(
    class = "d-flex align-items-center justify-content-between p-3 mb-3 border-bottom",
    
    # Title on left
    div(
      h2("Jameel Institute Crisis Vaccination Planner (JICVP)", 
         class = "mb-0", 
         style = "font-weight: 500; color: #0000cd; font-size: x-large;")
    ),
    
    # Logo on right
    img(src = "imperial_ji_logo.png", height = "50px")
  ),
  
  layout_columns(
    col_widths = c(2,10),
    create_sidebar_panel(),
    navset_card_underline(
      nav_panel("Model Setup", plotOutput("model_plot") %>% withSpinner(color = "#E5E4E2")),
      nav_panel("Model Outputs",
                div(
                  style = "margin-bottom: 2rem;",
                  plotOutput("results_plot", height = "600px") %>% withSpinner(color = "#E5E4E2"),
                  br(),
                  br(),
                  fluidRow(
                    column(width = 3, offset = 1, uiOutput("susceptibility_info")),
                    column(width = 3, offset = 3, uiOutput("case_info"))
                  )
                )
      ),
      nav_panel("About", uiOutput("ui_overview"))
    )
  )
  
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  ## FUTRE VALUE UI -----------------------------------------------------
  last_n <- reactiveVal(3)
  
  current_data <- reactiveVal(
    data.frame(Year = 0:2, seed = rep(0, 3), 'Vaccination coverage' = rep(0, 3))
  )
  
  observeEvent(input$years, {
    n_new <- input$years
    n_old <- last_n()
    df_old <- current_data()
    
    if (n_new > n_old) {
      added_rows <- data.frame(
        Year = n_old:(n_new - 1),
        seed = rep(0, n_new - n_old),
        'Vaccination coverage' = rep(0, n_new - n_old)
      )
      df_new <- rbind(df_old, added_rows)
    } else {
      df_new <- head(df_old, n_new)
    }
    
    current_data(df_new)
    last_n(n_new)
  })
  
  # TODO: User limits on values for table
  output$input_coverage_table <- renderDT({
    datatable(
      current_data(),
      editable = list(target = "cell", disable = list(columns = c(0))),
      rownames = FALSE,
      colnames = c("Year", "Seeded infections", "Vaccination <br>relative to starting (%)"),
      options = list(
        dom = 't',
        ordering = FALSE,
        #scrollY = 200,
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
  
  
  ## REACTIVE UI INPUTS -----------------------------------------------------
  output$pop_input <- renderUI({
    country <- input$country
    iso3c <- countrycode::countrycode(country, "country.name.en", "iso3c")
    n <- subset(population_all, iso3 == iso3c) %>% 
      select(x0:x100) %>% 
      tail(1) %>%
      gather() %>%
      rename(age = key, population = value) %>%
      mutate(age = as.numeric(gsub("x", "", age)),
             population = population*1000) %>% 
      pull(population) %>% 
      sum() %>% round()
    shinyWidgets::autonumericInput(
      inputId = "popsize",
      label = "Population size",
      value = n,
      min = 1,
      max = 2e9,
      decimalPlaces = 0,
      digitGroupSeparator = ",",
      decimalCharacter = ".",
      currencySymbol = "",
      currencySymbolPlacement = "s",
      style = "text-align: left;"
    )
    
  })
  
  output$r0_input <- renderUI({
    selected_disease <- input$disease
    default_r0 <- diseases_of_interest$default_R0[
      diseases_of_interest$disease == selected_disease
    ]
    max_r0 <- diseases_of_interest$max_R0[
      diseases_of_interest$disease == selected_disease
    ]
    min_r0 <- diseases_of_interest$min_R0[
      diseases_of_interest$disease == selected_disease
    ]
    
    numericInput("r0", "Basic reproductive number (R0)", value = default_r0, min = min_r0, max = max_r0)
  })
  
  ## PLOT Scaling
  dimension_debounced <- debounce(reactive(input$dimension), 2000)
  
  # Calculate your scaling factor
  plot_scale <- reactive({
    req(input$dimension)
    req(dimension_debounced())
    width <- dimension_debounced()[1]
    ideal_width <- 1500
    scale <- min(max(width / ideal_width, 0.6), 1.2)
    return(scale)
  })
  
  ## PLOT 1 -----------------------------------------------------
  # Observe the button click to trigger the plot generation
  plot_output <- eventReactive(input$run_model, {
    
    # Call your custom function and generate plot based on inputs
    plot_one(country = input$country,
             n = input$popsize,
             disease = input$disease,
             r0 = input$r0)
    
  })
  
  output$model_plot <- renderPlot({
    basepl <- plot_output()
    scale_factor <- plot_scale()
    
    adjusted_plot <- basepl & theme(
      text = element_text(size = 18 * scale_factor, family = "Helvetica"),
      axis.title = element_text(size = 18 * scale_factor),
      axis.text = element_text(size = 14 * scale_factor),
      legend.text = element_text(size = 16 * scale_factor),
      plot.title = element_text(size = 20 * scale_factor)
    )
    
    adjusted_plot
  }, 
  height = function() {
    req(input$dimension)
    req(dimension_debounced())
    0.9 * dimension_debounced()[2]
  },
  width = function() {
    req(input$dimension)    
    req(dimension_debounced())
    0.95 * dimension_debounced()[1]
  })
  
  ## PLOT 2-----------------------------------------------------
  # Observe the button click to trigger the plot generation
  model_data <- eventReactive(input$run_model, {
    df <- current_data()
    
    generate_model_data(
      country = input$country,
      n = input$popsize,
      disease = input$disease,
      r0 = input$r0,
      user_df = df
    )
    
  })
  
  stats <- reactive({
    req(model_data())
    summary_stats(model_data())
  })
  
  plot_results <- reactive({
    req(model_data())  # Wait until model_data is available
    plot_two(model_data())
  })
  
  output$susceptibility_info <- renderUI({
    req(stats())
    value_box(
      value = tags$div(stats()[1], style = "font-weight: bold; font-size: 1.5rem;"),
      title = "",
      theme = value_box_theme(
        bg = "#ab1940",
        fg = "white"
      )
    )
  })
  
  output$case_info <- renderUI({
    req(stats())
    value_box(
      value = tags$div(stats()[2], style = "font-weight: bold; font-size: 1.5rem;"),
      theme = value_box_theme(
        bg = "#ab1940",
        fg = "white"
      ),
      title = ""
    )
  })
  
  
  output$results_plot <- renderPlot({
    basepl2 <- plot_results()
    scale_factor <- plot_scale()
    
    adjusted_plot2 <- basepl2 & theme(
      text = element_text(size = 18 * scale_factor, family = "Helvetica"),
      axis.title = element_text(size = 18 * scale_factor),
      axis.text = element_text(size = 14 * scale_factor),
      legend.text = element_text(size = 16 * scale_factor),
      plot.title = element_text(size = 20 * scale_factor)
    )
    
    adjusted_plot2
  }, 
  height = function() {
    req(input$dimension)
    req(dimension_debounced())
    0.7 * dimension_debounced()[2]
  },
  width = function() {
    req(input$dimension)    
    req(dimension_debounced())
    0.95 * dimension_debounced()[1]
  })
  
  # make sure these run in the background in case the user is on the other tab
  outputOptions(output, "results_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "model_plot", suspendWhenHidden = FALSE, priority = 1)
  outputOptions(output, "case_info", suspendWhenHidden = FALSE, priority = 1)
  outputOptions(output, "susceptibility_info", suspendWhenHidden = FALSE, priority = 1)
  
  ## ABOUT -----------------------------------------------------
  ## ABOUT -----------------------------------------------------
  
  # Fetch markdown about
  output$ui_overview <- renderUI({
    includeMarkdown("www/about.md")
  })
  
}

shinyApp(ui, server)