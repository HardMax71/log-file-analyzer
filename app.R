# app.R

# Load Required Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(waiter)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)
library(zoo)
library(tools)
library(stringr)
library(readr)  # Required for log_parser.R

# Source All Module Files
files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
sapply(files, source)

# UI Definition
ui <- tagList(
  useWaiter(),  # Initialize Waiter
  
  # Show a loading spinner immediately upon app load
  waiter_show_on_load(
    html = tagList(
      spin_fading_circles(),  # Spinner without color argument
      br(),
      span("Loading sample logs and initializing...", class = "waiter-text")  # Apply CSS class for styling
    ),
    color = "#3498db" 
  ),
  
  tags$head(
    tags$script("
      $(document).on('click', '.sidebar-toggle', function() {
        setTimeout(function() {
          $('body').toggleClass('sidebar-collapse');
          $(window).trigger('resize');
        }, 100);
      });
      
      $(document).ready(function() {
        Shiny.addCustomMessageHandler('shinydashboard-sidebar-update', function(message) {
          var $activeTab = $('.tab-pane.active');
          $activeTab.removeClass('active').addClass('active');
          $(window).trigger('resize');
        });
      });
    "),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")  # Link to custom CSS
  ),
  
  dashboardPage(
    skin = "blue",
    title = NULL,
    options = list(sidebarExpandOnHover = TRUE),
    
    dashboardHeader(
      title = span(
        icon("terminal"), 
        span("Log Analyzer", class = "app-title")
      ),
      titleWidth = 300
    ),
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        id = "sidebar",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Timeline Analysis", tabName = "timeline", icon = icon("chart-line")),
        menuItem("Error Analysis", tabName = "errors", icon = icon("exclamation-triangle")),
        menuItem("Comparisons", tabName = "compare", icon = icon("balance-scale"))
      ),
      br(),
      uploadLogUI("uploadLog")  # Upload Module UI
    ),
    
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(tabName = "overview", overviewUI("overview")),
        tabItem(tabName = "timeline", timelineUI("timeline")),
        tabItem(tabName = "errors", errorAnalysisUI("errors")),
        tabItem(tabName = "compare", comparisonUI("compare"))
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Initialize Reactive Values
  rv <- reactiveValues(
    logs = NULL,
    filtered_logs = NULL,
    selected_timerange = NULL,
    selected_sources = NULL,
    selected_levels = NULL,
    error_threshold = 0.1,
    current_tab = NULL,
    refresh_trigger = NULL
  )
  
  # Load Default Data on Startup
  observe({
    req(is.null(rv$logs))  # Ensure this runs only once
    
    # Parse the Sample Logs Using the Custom Parser
    default_data <- tryCatch(
      {
        parse_log_file("./data/sample_logs.txt")
      },
      error = function(e) {
        showNotification(paste("Error loading default data:", e$message), type = "error")
        NULL
      }
    )
    
    # Proceed if Data is Successfully Parsed
    if (!is.null(default_data) && nrow(default_data) > 0) {
      rv$logs <- default_data
      rv$filtered_logs <- default_data
      
      # Initialize All Modules After Data Load
      # Using withProgress to provide feedback during module initialization
      withProgress(message = "Initializing modules...", value = 0, {
        n_modules <- 4  # Total Number of Modules
        
        overviewServer("overview", rv)
        incProgress(1 / n_modules)
        
        timelineServer("timeline", rv)
        incProgress(1 / n_modules)
        
        errorAnalysisServer("errors", rv)
        incProgress(1 / n_modules)
        
        comparisonServer("compare", rv)
        incProgress(1 / n_modules)
      })
      
      # Trigger a Refresh if Necessary
      rv$refresh_trigger <- runif(1)
      
      # Hide the Initial Loading Spinner
      waiter::waiter_hide()
    } else {
      showNotification("Error: Default data is empty or could not be loaded.", type = "error")
      waiter::waiter_hide()  # Ensure the spinner is hidden even if there's an error
    }
  })
  
  # Track Tab Changes
  observeEvent(input$sidebar, {
    rv$current_tab <- input$sidebar
  }, ignoreInit = TRUE)
  
  # Upload Module Server
  logs_data <- uploadLogServer("uploadLog")
  
  # Handle Uploaded Data
  observeEvent(logs_data(), {
    req(logs_data())
    
    # Show Upload Processing Spinner
    waiter::waiter_show(
      html = tagList(
        spin_fading_circles(),  # Spinner without color argument
        br(),
        span("Uploading and processing logs...", class = "waiter-text")  # Apply CSS class for styling
      ),
      color = "#f0f0f0"  # Background Color: Light Gray
    )
    
    # Process Uploaded Data
    uploaded_data <- tryCatch(
      {
        logs_data()
      },
      error = function(e) {
        showNotification(paste("Error processing uploaded data:", e$message), type = "error")
        NULL
      }
    )
    
    # Update Reactive Values if Data is Valid
    if (!is.null(uploaded_data) && nrow(uploaded_data) > 0) {
      rv$logs <- uploaded_data
      rv$filtered_logs <- uploaded_data
      rv$refresh_trigger <- runif(1)
      
      # Optionally, re-initialize modules or trigger reactive updates if necessary
    } else {
      showNotification("Error: Uploaded data is empty or could not be processed.", type = "error")
    }
    
    # Hide the Upload Processing Spinner
    waiter::waiter_hide()
  }, ignoreInit = TRUE)
  
  # Force Tab Refresh When Needed
  observeEvent(rv$refresh_trigger, {
    req(rv$current_tab)
    session$sendCustomMessage(
      type = "shinydashboard-sidebar-update",
      message = list(
        inputId = "sidebar",
        selected = rv$current_tab
      )
    )
  }, ignoreInit = TRUE)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
