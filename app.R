library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(shinyjs)
library(bslib)
library(plotly)

source("log_parser.R")

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Log File Analyzer",
  sidebar = sidebar(
    fileInput("logFile", "Choose Log File", accept = c("text/plain", ".log", ".txt")),
    selectInput("selectedColumn1", "Select First Column", choices = NULL),
    selectInput("selectedColumn2", "Select Second Column (Optional)", choices = c(None = "", NULL)),
    uiOutput("plotTypeSelector"),
    div(
      style = "height: 200px; overflow-y: auto;",
      uiOutput("filterSelector")
    ),
    actionButton("reloadBtn", "Reload Data", icon = icon("refresh"), class = "btn-primary"),
    actionButton("cleanBtn", "Clean Data", icon = icon("trash"), class = "btn-danger")
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Log Analysis"),
      uiOutput("recentFilesDropdown"),
      plotlyOutput("logPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  log_data <- reactiveVal(NULL)
  recent_files <- reactiveVal(character(0))
  
  observeEvent(input$logFile, {
    # Update recent files list
    current_files <- recent_files()
    new_file <- input$logFile$datapath
    if (!new_file %in% current_files) {
      current_files <- c(new_file, current_files)
      if (length(current_files) > 5) current_files <- current_files[1:5]
      recent_files(current_files)
    }
    
    tryCatch({
      df <- parse_log_file(input$logFile$datapath)
      if (is.data.frame(df) && nrow(df) > 0) {
        log_data(df)
        updateSelectInput(session, "selectedColumn1", choices = names(df))
        updateSelectInput(session, "selectedColumn2", choices = c(None = "", names(df)))
      } else {
        stop("Parsed data is empty or invalid")
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to parse log file:", e$message),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      log_data(NULL)
      updateSelectInput(session, "selectedColumn1", choices = NULL)
      updateSelectInput(session, "selectedColumn2", choices = c(None = "", NULL))
    })
  })
  
  observeEvent(input$reloadBtn, {
    req(input$logFile)
    log_data(parse_log_file(input$logFile$datapath))
    updateSelectInput(session, "selectedColumn1", choices = names(log_data()))
    updateSelectInput(session, "selectedColumn2", choices = c(None = "", names(log_data())))
  })
  
  observeEvent(input$cleanBtn, {
    log_data(NULL)
    reset("logFile")
    updateSelectInput(session, "selectedColumn1", choices = NULL)
    updateSelectInput(session, "selectedColumn2", choices = c(None = "", NULL))
  })
  
  output$plotTypeSelector <- renderUI({
    req(input$selectedColumn1)
    choices <- c("Bar Chart", "Line Chart", "Scatter Plot")
    if (input$selectedColumn2 != "") {
      choices <- c(choices, "Heatmap", "Bubble Chart")
    }
    radioButtons("plotType", "Select Plot Type", choices = choices)
  })
  
  output$filterSelector <- renderUI({
    req(log_data(), input$selectedColumn1)
    choices <- unique(log_data()[[input$selectedColumn1]])
    
    tagList(
      div(
        style = "max-height: 200px; overflow-y: auto; resize: vertical; min-height: 100px;",
        checkboxGroupInput("filter", "Filter", 
                           choices = choices,
                           selected = choices[1:min(5, length(choices))])
      ),
      tags$script(HTML("
      $(document).ready(function() {
        $('#filter').parent().css({
          'border': '1px solid #ccc',
          'padding': '10px',
          'border-radius': '5px'
        });
        $('#filter').parent().append('<div style=\"height: 10px; cursor: ns-resize; background-color: #f0f0f0; border-top: 1px solid #ccc;\"></div>');
      });
    "))
    )
  })
  
  output$logPlot <- renderPlotly({
    req(log_data(), input$selectedColumn1, input$plotType)
    
    tryCatch({
      filtered_data <- log_data()
      if (!is.null(input$filter) && length(input$filter) > 0) {
        if (inherits(filtered_data[[input$selectedColumn1]], "POSIXct")) {
          # Convert filter values to POSIXct for datetime columns
          converted_filter <- as.POSIXct(input$filter, format="%Y-%m-%d %H:%M:%S", tz="UTC")
          filtered_data <- filtered_data %>% filter(.data[[input$selectedColumn1]] %in% converted_filter)
        } else {
          filtered_data <- filtered_data %>% filter(.data[[input$selectedColumn1]] %in% input$filter)
        }
      }
      
      
      if (input$selectedColumn2 == "") {
        # Single column analysis
        p <- ggplot(filtered_data, aes_string(x = input$selectedColumn1))
        
        if (input$plotType == "Bar Chart") {
          p <- p + geom_bar(fill = "steelblue", color = "black")
        } else if (input$plotType == "Line Chart") {
          p <- p + stat_count(geom = "line", color = "steelblue") +
            stat_count(geom = "point", color = "darkblue")
        } else if (input$plotType == "Scatter Plot") {
          p <- p + stat_count(geom = "point", color = "steelblue")
        }
        
        p <- p + labs(y = "Count")
      } else {
        # Two column analysis
        p <- ggplot(filtered_data, aes_string(x = input$selectedColumn1, y = input$selectedColumn2))
        
        if (input$plotType == "Heatmap") {
          p <- p + stat_bin2d(aes(fill = after_stat(count))) + scale_fill_viridis_c()
        } else if (input$plotType == "Bubble Chart") {
          p <- p + stat_sum(aes(size = after_stat(n)), geom = "point", alpha = 0.6) +
            scale_size_continuous(range = c(1, 20))
        } else {
          p <- p + geom_point(color = "steelblue", size = 3)
        }
      }
      
      p <- p + theme_minimal(base_size = 14) +
        labs(title = paste("Analysis of", input$selectedColumn1, 
                           if(input$selectedColumn2 != "") paste("vs", input$selectedColumn2) else ""),
             x = input$selectedColumn1,
             y = if(input$selectedColumn2 != "") input$selectedColumn2 else "Count") +
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
      
      ggplotly(p)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to generate plot:", e$message),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      plotly_empty()
    })
  })
  
}

shinyApp(ui = ui, server = server)