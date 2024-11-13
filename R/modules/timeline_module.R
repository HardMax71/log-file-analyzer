# R/modules/timeline_module.R

timelineUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Time Controls",
        
        fluidRow(
          column(3,
                 dateRangeInput(ns("dateRange"), "Select Date Range",
                                start = Sys.Date() - 7,  # Default to last 7 days
                                end = Sys.Date())
          ),
          column(3,
                 selectizeInput(ns("timeInterval"), "Time Interval",
                                choices = c("Minute" = "mins",
                                            "Hour" = "hours",
                                            "Day" = "days",
                                            "Week" = "weeks"))
          ),
          column(3,
                 checkboxGroupInput(ns("includedLevels"), "Log Levels",
                                    choices = c("CRITICAL", "ERROR", "WARN", "INFO", "DEBUG"),
                                    selected = c("CRITICAL", "ERROR", "WARN", "INFO", "DEBUG"))
          ),
          column(3,
                 selectizeInput(ns("groupBy"), "Group By",
                                choices = c("Level" = "level",
                                            "Source" = "source"),
                                selected = "level")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        status = "info",
        solidHeader = TRUE,
        title = "Log Volume Overview",
        plotlyOutput(ns("timelinePlot"), height = "400px")
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        title = "Daily Activity Patterns",
        plotlyOutput(ns("hourlyPattern"), height = "350px")
      ),
      box(
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        title = "Weekly Distribution",
        plotlyOutput(ns("dailyPattern"), height = "350px")
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        status = "success",
        solidHeader = TRUE,
        title = "Anomaly Detection",
        fluidRow(
          column(3,
                 numericInput(ns("anomalyThreshold"), 
                              "Anomaly Threshold (std dev)", 
                              value = 2, min = 1, max = 5, step = 0.5)
          ),
          column(3,
                 selectInput(ns("anomalyWindow"), 
                             "Analysis Window",
                             choices = c("12 Hours" = 12,
                                         "24 Hours" = 24,
                                         "48 Hours" = 48),
                             selected = 24)
          ),
          column(6,
                 plotlyOutput(ns("anomalyPlot"), height = "350px")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Time-based Analysis",
        DTOutput(ns("timeAnalysisTable"))
      )
    )
  )
}

timelineServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Update date range when logs change
    observe({
      req(rv$logs)
      date_range <- range(rv$logs$timestamp)
      updateDateRangeInput(session, "dateRange",
                           start = as.Date(date_range[1]),
                           end = as.Date(date_range[2]))
    })
    
    # Set default selected levels if none are selected
    observe({
      req(input$includedLevels)
      if (length(input$includedLevels) == 0) {
        updateCheckboxGroupInput(session, "includedLevels",
                                 selected = c("CRITICAL", "ERROR", "WARN"))
      }
    })
    
    # Filtered logs based on time controls with defaults
    filtered_timeline_logs <- reactive({
      req(rv$logs, input$dateRange, input$includedLevels)
      
      selected_levels <- if (length(input$includedLevels) == 0) {
        c("CRITICAL", "ERROR", "WARN")
      } else {
        input$includedLevels
      }
      
      start_date <- as.POSIXct(input$dateRange[1])
      end_date <- as.POSIXct(paste(input$dateRange[2], "23:59:59"))
      
      rv$logs %>%
        filter(
          timestamp >= start_date,
          timestamp <= end_date,
          level %in% selected_levels
        )
    })
    
    # Timeline plot
    output$timelinePlot <- renderPlotly({
      req(filtered_timeline_logs(), input$timeInterval, input$groupBy)
      
      if(input$groupBy == "level") {
        plot_timeline(filtered_timeline_logs(), input$timeInterval)
      } else {
        plot_timeline_grouped(filtered_timeline_logs(), 
                              input$timeInterval, 
                              input$groupBy)
      }
    })
    
    # Hourly pattern plot
    output$hourlyPattern <- renderPlotly({
      req(filtered_timeline_logs())
      plot_hourly_pattern(filtered_timeline_logs())
    })
    
    # Daily pattern plot
    output$dailyPattern <- renderPlotly({
      req(filtered_timeline_logs())
      plot_daily_pattern(filtered_timeline_logs())
    })
    
    # Anomaly detection plot
    output$anomalyPlot <- renderPlotly({
      req(filtered_timeline_logs(), input$anomalyThreshold, input$anomalyWindow)
      plot_anomalies(
        filtered_timeline_logs(), 
        threshold = input$anomalyThreshold,
        window = as.numeric(input$anomalyWindow)
      )
    })
    
    # Time analysis table
    output$timeAnalysisTable <- renderDT({
      req(filtered_timeline_logs())
      
      analysis <- filtered_timeline_logs() %>%
        mutate(
          hour = hour(timestamp),
          wday = wday(timestamp, label = TRUE)
        ) %>%
        group_by(wday, hour) %>%
        summarise(
          total_logs = n(),
          error_rate = mean(level %in% c("CRITICAL", "ERROR")) * 100,
          unique_sources = n_distinct(source),
          .groups = 'drop'
        ) %>%
        arrange(wday, hour)
      
      datatable(
        analysis,
        options = list(
          pageLength = 10,
          order = list(list(0, 'asc'), list(1, 'asc')),
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      ) %>%
        formatRound("error_rate", 2) %>%
        formatStyle(
          "error_rate",
          background = styleColorBar(c(0, 100), '#DC3545'),
          color = 'white'
        )
    })
  })
}

# Additional plot function for grouped timeline
plot_timeline_grouped <- function(logs, interval, group_by) {
  if (nrow(logs) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  logs_agg <- logs %>%
    mutate(interval = floor_date(timestamp, unit = interval)) %>%
    group_by(interval, !!sym(group_by)) %>%
    summarise(n = n(), .groups = 'drop')
  
  plot_ly(data = logs_agg) %>%
    add_trace(
      x = ~interval,
      y = ~n,
      color = as.formula(paste0("~", group_by)),
      colors = if(group_by == "level") level_colors else NULL,
      type = "scatter",
      mode = "lines",
      stackgroup = "one",
      hoverinfo = "text",
      text = ~paste(
        "Time:", format(interval, "%Y-%m-%d %H:%M"),
        "<br>", group_by, ":", get(group_by),
        "<br>Count:", n
      )
    ) %>%
    layout(
      title = list(
        text = paste("Log Activity by", tools::toTitleCase(group_by)),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Number of Logs",
        showgrid = TRUE,
        gridcolor = 'rgba(0,0,0,0.1)',
        zeroline = FALSE
      ),
      hovermode = "x unified",
      showlegend = TRUE,
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
}