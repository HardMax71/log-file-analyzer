# app.R
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
library(zoo)  # For rollmean function
library(tools)  # For capitalize function
library(stringr)  # For string manipulation

# Source all module files
files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
sapply(files, source)

updateTabItems <- function(session, inputId, selected) {
  session$sendCustomMessage(
    type = "shinydashboard-sidebar-update",
    message = list(
      inputId = inputId,
      selected = selected
    )
  )
}

# UI Definition
ui <- tagList(
  useWaiter(), # Loading screen
  tags$script("
    $(document).ready(function() {
      $('.sidebar-toggle').on('click', function() {
        if ($('body').hasClass('sidebar-collapse')) {
          $('body').removeClass('sidebar-collapse');
        } else {
          $('body').addClass('sidebar-collapse');
        }
      });
      
      Shiny.addCustomMessageHandler('shinydashboard-sidebar-update', function(message) {
        // Force rerender of the active tab content
        var $activeTab = $('.tab-pane.active');
        $activeTab.removeClass('active').addClass('active');
        
        // Trigger window resize to redraw plots
        setTimeout(function() {
          $(window).trigger('resize');
        }, 100);
      });
    });
  "),
  dashboardPage(
    skin = "blue",
    title = NULL,
    options = list(sidebarExpandOnHover = TRUE),
    
    # Header
    dashboardHeader(
      title = span(
        icon("terminal"), 
        span("Log Analyzer", class = "app-title"),
      ),
      titleWidth = 300
    ),
    
    # Sidebar
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
      uploadLogUI("uploadLog")
    ),
    
    # Body
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      useShinyjs(),
      tabItems(
        # Overview Tab
        tabItem(
          tabName = "overview",
          overviewUI("overview")
        ),
        
        # Timeline Tab
        tabItem(
          tabName = "timeline",
          timelineUI("timeline")
        ),
        
        # Errors Tab
        tabItem(
          tabName = "errors",
          errorAnalysisUI("errors")
        ),
        
        # Comparison Tab
        tabItem(
          tabName = "compare",
          comparisonUI("compare")
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Initialize reactive values
  rv <- reactiveValues(
    logs = NULL,
    filtered_logs = NULL,
    selected_timerange = NULL,
    selected_sources = NULL,
    selected_levels = NULL,
    error_threshold = 0.1,
    current_tab = NULL
  )
  
  # Track tab changes
  observeEvent(input$sidebar, {
    rv$current_tab <- input$sidebar
  })
  
  # Upload module
  logs_data <- uploadLogServer("uploadLog")
  
  # Update reactive values when new data is uploaded
  observeEvent(logs_data(), {
    rv$logs <- logs_data()
    rv$filtered_logs <- logs_data()
    
    # Trigger global refresh
    rv$refresh_trigger <- runif(1)
  })
  
  # Initialize all modules unconditionally
  overviewServer("overview", rv)
  timelineServer("timeline", rv)
  errorAnalysisServer("errors", rv)
  comparisonServer("compare", rv)
  
  # Force tab refresh when needed
  observeEvent(rv$refresh_trigger, {
    req(rv$current_tab)
    updateTabItems(session, "sidebar", rv$current_tab)
  })
}

# Run the app
shinyApp(ui = ui, server = server)