# R/modules/comparison_module.R
comparisonServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Observe logs changes
    observeEvent(rv$logs, {
      req(rv$logs)
      
      # Dynamic comparison controls
      output$comparisonControls <- renderUI({
        req(input$compareType)
        switch(input$compareType,
               "time" = dateRangeInput(session$ns("timeRanges"), 
                                       "Select Time Ranges",
                                       start = min(as.Date(rv$logs$timestamp)), 
                                       end = max(as.Date(rv$logs$timestamp))),
               "source" = selectizeInput(session$ns("sources"), 
                                         "Select Sources to Compare",
                                         choices = unique(rv$logs$source),
                                         selected = unique(rv$logs$source)[1],
                                         multiple = TRUE),
               "level" = selectizeInput(session$ns("levels"),
                                        "Select Levels to Compare",
                                        choices = unique(rv$logs$level),
                                        selected = unique(rv$logs$level)[1],
                                        multiple = TRUE)
        )
      })
      
      # Comparison data and plots
      comparison_data <- reactive({
        req(input$compareType, input$metricType)
        
        params <- list(
          time_ranges = if(input$compareType == "time" && !is.null(input$timeRanges)) {
            as.POSIXct(input$timeRanges)
          } else NULL,
          sources = if(input$compareType == "source" && !is.null(input$sources)) {
            input$sources
          } else NULL,
          levels = if(input$compareType == "level" && !is.null(input$levels)) {
            input$levels
          } else NULL
        )
        
        generate_comparison_data(rv$logs, input$compareType, input$metricType, params)
      })
      
      # Update all outputs
      output$comparisonPlot <- renderPlotly({
        req(comparison_data())
        if (nrow(comparison_data()) == 0) {
          plot_ly() %>%
            layout(title = "No data available for comparison",
                   annotations = list(
                     x = 0.5,
                     y = 0.5,
                     text = "Select different comparison parameters",
                     showarrow = FALSE
                   ))
        } else {
          plot_comparison(comparison_data(), input$compareType, input$metricType)
        }
      })
      
      output$statSummary <- renderPrint({
        req(comparison_data())
        if (nrow(comparison_data()) == 0) {
          cat("No data available for statistical summary")
        } else {
          calculate_stat_summary(comparison_data())
        }
      })
      
      output$trendAnalysis <- renderPrint({
        req(comparison_data())
        if (nrow(comparison_data()) == 0) {
          cat("No data available for trend analysis")
        } else {
          analyze_trends(comparison_data())
        }
      })
      
      output$comparisonTable <- renderDT({
        req(comparison_data())
        if (nrow(comparison_data()) == 0) {
          data.frame(
            "Category" = character(0),
            "Value" = numeric(0)
          )
        } else {
          format_comparison_table(comparison_data())
        }
      })
    })
  })
}

comparisonUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Comparison Controls",
        
        fluidRow(
          column(4,
                 selectInput(ns("compareType"),
                             "Compare By",
                             choices = c(
                               "Time Periods" = "time",
                               "Log Sources" = "source",
                               "Log Levels" = "level"
                             ))
          ),
          column(4,
                 selectInput(ns("metricType"),
                             "Metric",
                             choices = c(
                               "Log Volume" = "volume",
                               "Error Rate" = "error_rate",
                               "Average Response Time" = "response_time"
                             ))
          ),
          column(4,
                 uiOutput(ns("comparisonControls"))
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 8,
        status = "info",
        solidHeader = TRUE,
        title = "Comparison Plot",
        plotlyOutput(ns("comparisonPlot"))
      ),
      box(
        width = 4,
        status = "warning",
        solidHeader = TRUE,
        title = "Statistical Summary",
        verbatimTextOutput(ns("statSummary")),
        hr(),
        h4("Trend Analysis"),
        verbatimTextOutput(ns("trendAnalysis"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        status = "success",
        solidHeader = TRUE,
        title = "Detailed Comparison",
        DTOutput(ns("comparisonTable"))
      )
    )
  )
}

# Supporting functions for comparison module
generate_comparison_data <- function(logs, compare_type, metric_type, params) {
  if (nrow(logs) == 0) return(data.frame())
  
  result <- switch(compare_type,
                   "time" = {
                     if (is.null(params$time_ranges)) return(data.frame())
                     logs %>%
                       mutate(period = if_else(
                         timestamp >= params$time_ranges[1] & timestamp <= params$time_ranges[2],
                         "Period 1", "Period 2"
                       )) %>%
                       group_by(period) %>%
                       summarise(
                         value = case_when(
                           metric_type == "volume" ~ n(),
                           metric_type == "error_rate" ~ mean(level %in% c("ERROR", "CRITICAL")) * 100,
                           metric_type == "response_time" ~ mean(response_time, na.rm = TRUE)
                         ),
                         .groups = "drop"
                       )
                   },
                   "source" = {
                     if (is.null(params$sources)) return(data.frame())
                     logs %>%
                       filter(source %in% params$sources) %>%
                       group_by(source) %>%
                       summarise(
                         value = case_when(
                           metric_type == "volume" ~ n(),
                           metric_type == "error_rate" ~ mean(level %in% c("ERROR", "CRITICAL")) * 100,
                           metric_type == "response_time" ~ mean(response_time, na.rm = TRUE)
                         ),
                         .groups = "drop"
                       ) %>%
                       rename(category = source)
                   },
                   "level" = {
                     if (is.null(params$levels)) return(data.frame())
                     logs %>%
                       filter(level %in% params$levels) %>%
                       group_by(level) %>%
                       summarise(
                         value = case_when(
                           metric_type == "volume" ~ n(),
                           metric_type == "error_rate" ~ mean(level %in% c("ERROR", "CRITICAL")) * 100,
                           metric_type == "response_time" ~ mean(response_time, na.rm = TRUE)
                         ),
                         .groups = "drop"
                       ) %>%
                       rename(category = level)
                   }
  )
  
  return(result)
}

plot_comparison <- function(data, compare_type, metric_type) {
  # Color palette
  colors <- c(
    "Period 1" = "#6C757D",
    "Period 2" = "#007BFF",
    "ERROR" = "#DC3545",
    "WARNING" = "#FFC107",
    "INFO" = "#17A2B8",
    "DEBUG" = "#6C757D"
  )
  
  # Y-axis label based on metric type
  y_label <- switch(metric_type,
                    "volume" = "Number of Logs",
                    "error_rate" = "Error Rate (%)",
                    "response_time" = "Average Response Time (ms)"
  )
  
  plot_ly(data) %>%
    add_trace(
      x = ~category,
      y = ~value,
      type = "bar",
      marker = list(
        color = if(compare_type == "level") {
          colors[data$category]
        } else {
          "#007BFF"
        }
      ),
      text = ~round(value, 2),
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~paste(
        "Category:", category,
        "<br>Value:", round(value, 2)
      )
    ) %>%
    layout(
      title = list(
        text = paste("Comparison of", y_label),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "",
        showgrid = FALSE
      ),
      yaxis = list(
        title = y_label,
        showgrid = TRUE,
        gridcolor = 'rgba(0,0,0,0.1)'
      ),
      showlegend = FALSE,
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
}

calculate_stat_summary <- function(data) {
  summary_stats <- summary(data$value)
  var_stats <- var(data$value)
  sd_stats <- sd(data$value)
  
  cat("Statistical Summary:\n\n")
  cat("Min:", round(summary_stats[1], 2), "\n")
  cat("Median:", round(summary_stats[3], 2), "\n")
  cat("Mean:", round(mean(data$value), 2), "\n")
  cat("Max:", round(summary_stats[6], 2), "\n")
  cat("Std Dev:", round(sd_stats, 2), "\n")
  cat("Variance:", round(var_stats, 2), "\n")
}

analyze_trends <- function(data) {
  if (nrow(data) < 2) {
    return("Insufficient data for trend analysis")
  }
  
  trend <- diff(data$value)
  trend_direction <- if(mean(trend) > 0) "increasing" else "decreasing"
  
  cat("Trend Analysis:\n\n")
  cat("Direction:", trend_direction, "\n")
  cat("Average change:", round(mean(abs(trend)), 2), "\n")
  cat("Max change:", round(max(abs(trend)), 2), "\n")
}

format_comparison_table <- function(data) {
  datatable(
    data,
    options = list(
      pageLength = 10,
      searching = FALSE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    extensions = 'Buttons'
  ) %>%
    formatRound('value', 2)
}