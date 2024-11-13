# R/modules/overview_module.R

overviewUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      valueBoxOutput(ns("totalLogs"), width = 3),
      valueBoxOutput(ns("uniqueSources"), width = 3),
      valueBoxOutput(ns("logLevels"), width = 3),
      valueBoxOutput(ns("timeSpan"), width = 3)
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Log Level Distribution",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("levelDist"))
      ),
      box(
        width = 6,
        title = "Source Distribution",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("sourceDist"))
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Log Volume Timeline",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput(ns("volumeTimeline"))
      ),
      box(
        width = 6,
        title = "Log Level Patterns",
        status = "info",
        solidHeader = TRUE,
        plotlyOutput(ns("levelPatterns"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Recent Logs",
        status = "info",
        solidHeader = TRUE,
        DTOutput(ns("recentLogs"))
      )
    )
  )
}

overviewServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Common color palette
    level_colors <- c(
      "CRITICAL" = "#721C24",
      "ERROR" = "#DC3545",
      "WARNING" = "#FFC107",
      "INFO" = "#6C757D",
      "DEBUG" = "#ADB5BD"
    )
    
    # Reactive expression for logs
    logs <- reactive({
      req(rv$logs)
      rv$logs
    })
    
    # Observe logs changes and update all outputs
    observeEvent(logs(), {
      # Value boxes
      output$totalLogs <- renderValueBox({
        valueBox(
          nrow(logs()),
          "Total Logs",
          icon = icon("list"),
          color = "blue"
        )
      })
      
      output$uniqueSources <- renderValueBox({
        valueBox(
          length(unique(logs()$source)),
          "Unique Sources",
          icon = icon("server"),
          color = "purple"
        )
      })
      
      output$logLevels <- renderValueBox({
        valueBox(
          length(unique(logs()$level)),
          "Log Levels",
          icon = icon("tags"),
          color = "yellow"
        )
      })
      
      output$timeSpan <- renderValueBox({
        time_diff <- difftime(max(logs()$timestamp), 
                              min(logs()$timestamp), 
                              units = "hours")
        valueBox(
          sprintf("%.1f hours", as.numeric(time_diff)),
          "Time Span",
          icon = icon("clock"),
          color = "green"
        )
      })
      
      # Level distribution plot
      output$levelDist <- renderPlotly({
        level_data <- logs() %>%
          count(level) %>%
          mutate(
            percentage = n / sum(n) * 100,
            hover_text = sprintf(
              "Level: %s<br>Count: %d<br>Percentage: %.1f%%",
              level, n, percentage
            )
          )
        
        plot_ly(level_data,
                labels = ~level,
                values = ~n,
                type = "pie",
                textposition = "inside",
                textinfo = "percent",
                hoverinfo = "text",
                text = ~hover_text,
                marker = list(
                  colors = level_colors[level_data$level],
                  line = list(color = '#FFFFFF', width = 2)
                ),
                hole = 0.4
        ) %>%
          layout(
            showlegend = TRUE,
            annotations = list(
              list(
                text = "Log\nLevels",
                showarrow = FALSE,
                font = list(size = 20)
              )
            ),
            paper_bgcolor = 'rgba(0,0,0,0)',
            plot_bgcolor = 'rgba(0,0,0,0)'
          )
      })
      
      # Source distribution plot
      output$sourceDist <- renderPlotly({
        top_sources <- logs() %>%
          count(source) %>%
          top_n(8, n) %>%
          pull(source)
        
        plot_data <- logs() %>%
          filter(source %in% top_sources) %>%
          count(source, level) %>%
          group_by(source) %>%
          mutate(
            total = sum(n),
            percentage = n / total * 100,
            hover_text = sprintf(
              "Source: %s<br>Level: %s<br>Count: %d<br>Percentage: %.1f%%",
              source, level, n, percentage
            )
          )
        
        plot_ly(plot_data,
                x = ~reorder(source, -total),
                y = ~n,
                color = ~level,
                colors = unname(level_colors),
                type = "bar",
                text = ~hover_text,
                hoverinfo = "text"
        ) %>%
          layout(
            barmode = "stack",
            xaxis = list(
              title = "",
              tickangle = 30,
              showgrid = FALSE
            ),
            yaxis = list(
              title = "Number of Logs",
              showgrid = TRUE,
              gridcolor = 'rgba(0,0,0,0.1)'
            ),
            showlegend = TRUE,
            paper_bgcolor = 'rgba(0,0,0,0)',
            plot_bgcolor = 'rgba(0,0,0,0)'
          )
      })
      
      # Volume Timeline
      output$volumeTimeline <- renderPlotly({
        timeline_data <- logs() %>%
          mutate(hour = floor_date(timestamp, "hour")) %>%
          count(hour) %>%
          arrange(hour)
        
        timeline_data$ma <- rollmean(timeline_data$n, 3, fill = NA, align = "right")
        
        plot_ly() %>%
          add_trace(
            data = timeline_data,
            x = ~hour,
            y = ~n,
            type = "scatter",
            mode = "lines",
            line = list(color = '#6C757D', width = 1),
            opacity = 0.3,
            name = "Actual",
            hoverinfo = "text",
            text = ~sprintf("Time: %s<br>Count: %d", format(hour), n)
          ) %>%
          add_trace(
            data = timeline_data,
            x = ~hour,
            y = ~ma,
            type = "scatter",
            mode = "lines",
            line = list(color = '#6C757D', width = 2),
            name = "Moving Average",
            hoverinfo = "text",
            text = ~sprintf("Time: %s<br>Average: %.1f", format(hour), ma)
          ) %>%
          layout(
            xaxis = list(
              title = "",
              showgrid = FALSE
            ),
            yaxis = list(
              title = "Log Volume",
              showgrid = TRUE,
              gridcolor = 'rgba(0,0,0,0.1)'
            ),
            showlegend = TRUE,
            paper_bgcolor = 'rgba(0,0,0,0)',
            plot_bgcolor = 'rgba(0,0,0,0)'
          )
      })
      
      # Level Patterns
      output$levelPatterns <- renderPlotly({
        pattern_data <- logs() %>%
          mutate(hour = hour(timestamp)) %>%
          count(hour, level) %>%
          complete(hour = 0:23, level, fill = list(n = 0))
        
        plot_ly(pattern_data,
                x = ~hour,
                y = ~n,
                color = ~level,
                colors = unname(level_colors),
                type = "scatter",
                mode = "lines+markers",
                marker = list(size = 6),
                line = list(width = 1.5)
        ) %>%
          layout(
            xaxis = list(
              title = "Hour of Day",
              dtick = 1,
              ticktext = paste0(0:23, "h"),
              tickvals = 0:23,
              showgrid = FALSE
            ),
            yaxis = list(
              title = "Number of Logs",
              showgrid = TRUE,
              gridcolor = 'rgba(0,0,0,0.1)'
            ),
            showlegend = TRUE,
            paper_bgcolor = 'rgba(0,0,0,0)',
            plot_bgcolor = 'rgba(0,0,0,0)'
          )
      })
      
      # Recent Logs Table
      output$recentLogs <- renderDT({
        logs() %>%
          arrange(desc(timestamp)) %>%
          head(100) %>%
          select(timestamp, level, source, message) %>%
          datatable(
            options = list(
              pageLength = 10,
              order = list(list(0, 'desc')),
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            ),
            extensions = 'Buttons'
          ) %>%
          formatStyle(
            'level',
            backgroundColor = styleEqual(
              c('CRITICAL', 'ERROR', 'WARNING', 'INFO', 'DEBUG'),
              level_colors
            ),
            color = styleEqual(
              c('CRITICAL', 'ERROR', 'WARNING', 'INFO', 'DEBUG'),
              c('white', 'white', 'black', 'white', 'white')
            )
          )
      })
    })
  })
}


