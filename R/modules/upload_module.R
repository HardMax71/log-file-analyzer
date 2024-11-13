# R/modules/upload_module.R
uploadLogUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "upload-controls",
      fileInput(
        ns("logFile"), 
        "Upload Log File",
        accept = c("text/plain", ".log", ".txt"),
        multiple = FALSE
      ),
      actionButton(
        ns("loadSample"), 
        "Load Sample Data",
        icon = icon("file-alt"),
        class = "btn-info"
      )
    ),
    uiOutput(ns("uploadStatus"))
  )
}

uploadLogServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    logs_data <- reactiveVal(NULL)
    
    # Handle file upload
    observeEvent(input$logFile, {
      req(input$logFile)
      
      # Show processing notification
      id <- showNotification(
        span(icon("terminal", class = "fa-spin"), "Processing log file..."),
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      
      # Process file
      tryCatch({
        file_path <- input$logFile$datapath
        logs <- parse_log_file(file_path)
        logs_data(logs)
        
        removeNotification(id)
        showNotification(
          span(icon("check"), "File successfully loaded"),
          type = "message"
        )
        
        output$uploadStatus <- renderUI({
          div(class = "text-success",
              icon("check"), "File loaded")
        })
        
      }, error = function(e) {
        removeNotification(id)
        showNotification(
          span(icon("exclamation-circle"), "Error loading file"),
          type = "error"
        )
        
        output$uploadStatus <- renderUI({
          div(class = "text-danger",
              icon("times"), "Error:", e$message)
        })
      })
    })
    
    # Handle sample data loading
    observeEvent(input$loadSample, {
      # Show processing notification
      id <- showNotification(
        span(icon("terminal", class = "fa-spin"), "Loading sample data..."),
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      
      tryCatch({
        logs <- parse_log_file("data/sample_logs.txt")
        logs_data(logs)
        
        removeNotification(id)
        showNotification(
          span(icon("check"), "Sample data loaded"),
          type = "message"
        )
        
        output$uploadStatus <- renderUI({
          div(class = "text-success",
              icon("check"), "Sample data loaded")
        })
        
      }, error = function(e) {
        removeNotification(id)
        showNotification(
          span(icon("exclamation-circle"), "Error loading sample data"),
          type = "error"
        )
        
        output$uploadStatus <- renderUI({
          div(class = "text-danger",
              icon("times"), "Error:", e$message)
        })
      })
    })
    
    return(logs_data)
  })
}