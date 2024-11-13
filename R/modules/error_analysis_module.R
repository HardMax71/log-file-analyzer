# R/modules/error_analysis_module.R
errorAnalysisUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        width = 12,
        status = "danger",
        solidHeader = TRUE,
        title = "Error Overview",
        fluidRow(
          valueBoxOutput(ns("totalErrors"), width = 3),
          valueBoxOutput(ns("errorRate"), width = 3),
          valueBoxOutput(ns("criticalErrors"), width = 3),
          valueBoxOutput(ns("avgResolutionTime"), width = 3)
        )
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        title = "Error Distribution by Source",
        plotlyOutput(ns("errorBySource"))
      ),
      box(
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        title = "Error Severity Distribution",
        plotlyOutput(ns("errorSeverity"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        status = "danger",
        solidHeader = TRUE,
        title = "Error Timeline",
        fluidRow(
          column(3,
                 selectInput(ns("timeAggregation"), "Time Aggregation",
                             choices = c("Hourly" = "hours",
                                         "Daily" = "days",
                                         "Weekly" = "weeks",
                                         "Monthly" = "months"))
          )
        ),
        plotlyOutput(ns("errorTimeline"))
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        status = "info",
        solidHeader = TRUE,
        title = "Top Error Messages",
        plotlyOutput(ns("topErrors"))
      ),
      box(
        width = 6,
        status = "info",
        solidHeader = TRUE,
        title = "Client IP Heat Map",
        plotlyOutput(ns("clientHeatmap"))
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        title = "Error Resolution Time by Source",
        plotlyOutput(ns("resolutionBySource"))
      ),
      box(
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        title = "Hour of Day Analysis",
        plotlyOutput(ns("hourlyPattern"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Error Details",
        DTOutput(ns("errorTable"))
      )
    )
  )
}

errorAnalysisServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Common color palette
    error_colors <- c(
      "CRITICAL" = "#721C24",
      "ERROR" = "#DC3545",
      "WARN" = "#FFC107"
    )
    
    # Reactive expression for error logs
    error_logs <- reactive({
      req(rv$logs)
      rv$logs %>%
        filter(level %in% c("ERROR", "CRITICAL", "WARN"))
    })
    
    # Observe logs changes and update all outputs
    observeEvent(rv$logs, {
      req(error_logs())
      
      # Value boxes
      output$totalErrors <- renderValueBox({
        valueBox(
          nrow(error_logs()),
          "Total Error Events",
          icon = icon("exclamation-circle"),
          color = "red"
        )
      })
      
      output$errorRate <- renderValueBox({
        error_rate <- nrow(error_logs()) / nrow(rv$logs) * 100
        valueBox(
          sprintf("%.2f%%", error_rate),
          "Error Rate",
          icon = icon("percent"),
          color = if(error_rate > rv$error_threshold * 100) "red" else "yellow"
        )
      })
      
      output$criticalErrors <- renderValueBox({
        critical_count <- sum(error_logs()$level == "CRITICAL")
        valueBox(
          critical_count,
          "Critical Errors",
          icon = icon("radiation"),
          color = "red"
        )
      })
      
      output$avgResolutionTime <- renderValueBox({
        tryCatch({
          avg_resolution <- calculate_resolution_time(rv$logs)
          valueBox(
            if(avg_resolution > 0) sprintf("%.1f min", avg_resolution) else "N/A",
            "Avg Resolution Time",
            icon = icon("clock"),
            color = "purple"
          )
        }, error = function(e) {
          valueBox(
            "N/A",
            "Avg Resolution Time",
            icon = icon("clock"),
            color = "purple"
          )
        })
      })
      
      # Plots
      output$errorBySource <- renderPlotly({
        plot_error_by_source(error_logs())
      })
      
      output$errorSeverity <- renderPlotly({
        plot_error_severity(error_logs())
      })
      
      output$errorTimeline <- renderPlotly({
        req(input$timeAggregation)
        plot_error_timeline(error_logs(), input$timeAggregation)
      })
      
      output$topErrors <- renderPlotly({
        plot_top_errors(error_logs())
      })
      
      output$clientHeatmap <- renderPlotly({
        plot_client_heatmap(error_logs())
      })
      
      output$resolutionBySource <- renderPlotly({
        plot_resolution_by_source(error_logs())
      })
      
      output$hourlyPattern <- renderPlotly({
        plot_hourly_error_pattern(error_logs())
      })
      
      # Error details table
      output$errorTable <- renderDT({
        desired_cols <- c("timestamp", "level", "source", "message", "request_id", "client_ip")
        table_data <- error_logs() %>%
          select(any_of(desired_cols)) %>%
          arrange(desc(timestamp))
        
        datatable(table_data,
                  options = list(
                    pageLength = 10,
                    order = list(list(0, 'desc')),
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    scrollX = TRUE
                  ),
                  extensions = 'Buttons'
        ) %>%
          formatStyle(
            'level',
            backgroundColor = styleEqual(
              c('ERROR', 'CRITICAL', 'WARN'),
              c('#dc3545', '#dc3545', '#ffc107')
            ),
            color = styleEqual(
              c('ERROR', 'CRITICAL', 'WARN'),
              c('white', 'white', 'black')
            )
          )
      })
    })
  })
}


