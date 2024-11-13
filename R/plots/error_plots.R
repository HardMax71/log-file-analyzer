# R/plots/error_plots.R

# Common color scheme for better visual hierarchy and consistency
error_colors <- list(
  CRITICAL = "#721C24",  # Dark red
  ERROR = "#DC3545",     # Bright red
  WARN = "#FFC107",      # Yellow
  background = "#F8F9FA", # Light gray background
  grid = "rgba(0,0,0,0.1)",
  text = "#2C3E50"       # Dark blue-gray for text
)

plot_error_severity <- function(logs) {
  severity_data <- logs %>%
    count(level) %>%
    mutate(percentage = n/sum(n) * 100)
  
  plot_ly(severity_data) %>%
    add_trace(
      labels = ~level,
      values = ~n,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(
        "Level:", level,
        "<br>Count:", n,
        "<br>Percentage:", sprintf("%.1f%%", percentage)
      ),
      marker = list(
        colors = c(
          "CRITICAL" = error_colors$CRITICAL,
          "ERROR" = error_colors$ERROR,
          "WARN" = error_colors$WARN
        )
      ),
      hole = 0.4  # Makes it a donut chart
    ) %>%
    layout(
      showlegend = TRUE,
      paper_bgcolor = error_colors$background,
      plot_bgcolor = error_colors$background,
      font = list(color = error_colors$text),
      margin = list(t = 50, b = 50),
      annotations = list(
        list(
          text = "Error\nSeverity",
          showarrow = FALSE,
          font = list(size = 16)
        )
      )
    )
}

plot_error_by_source <- function(logs) {
  # Get top 10 sources by error count
  source_data <- logs %>%
    count(source, level) %>%
    group_by(source) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    arrange(desc(total)) %>%
    filter(source %in% (group_by(., source) %>% 
                          summarise(total = sum(n)) %>% 
                          arrange(desc(total)) %>% 
                          head(10) %>% 
                          pull(source)))
  
  plot_ly(source_data) %>%
    add_trace(
      x = ~reorder(source, -total),
      y = ~n,
      color = ~level,
      colors = c(
        "CRITICAL" = error_colors$CRITICAL,
        "ERROR" = error_colors$ERROR,
        "WARN" = error_colors$WARN
      ),
      type = "bar",
      hoverinfo = "text",
      text = ~paste(
        "Source:", source,
        "<br>Level:", level,
        "<br>Count:", n,
        "<br>Total Errors:", total
      )
    ) %>%
    layout(
      barmode = "stack",
      showlegend = TRUE,
      paper_bgcolor = error_colors$background,
      plot_bgcolor = error_colors$background,
      font = list(color = error_colors$text),
      margin = list(l = 50, r = 50, b = 100, t = 50),
      xaxis = list(
        title = "",
        tickangle = 45,
        showgrid = FALSE,
        ticktext = ~substr(source_data$source, 1, 20),
        tickvals = ~reorder(source_data$source, -source_data$total)
      ),
      yaxis = list(
        title = "Number of Errors",
        showgrid = TRUE,
        gridcolor = error_colors$grid
      ),
      hovermode = "closest"
    )
}

plot_top_errors <- function(logs) {
  top_errors <- logs %>%
    mutate(error_type = str_extract(message, "^[^:]+")) %>%
    count(error_type, level) %>%
    group_by(error_type) %>%
    summarise(
      count = sum(n),
      level = first(level)
    ) %>%
    arrange(desc(count)) %>%
    head(10)
  
  plot_ly(top_errors) %>%
    add_trace(
      x = ~reorder(error_type, count),
      y = ~count,
      type = "bar",
      marker = list(
        color = case_when(
          top_errors$level == "CRITICAL" ~ error_colors$CRITICAL,
          top_errors$level == "ERROR" ~ error_colors$ERROR,
          TRUE ~ error_colors$WARN
        )
      ),
      text = ~count,
      textposition = "auto",  # Shows count inside bars
      hoverinfo = "text",
      hovertext = ~paste(
        "Error:", error_type,
        "<br>Count:", count,
        "<br>Level:", level
      )
    ) %>%
    layout(
      showlegend = FALSE,
      paper_bgcolor = error_colors$background,
      plot_bgcolor = error_colors$background,
      font = list(color = error_colors$text),
      margin = list(l = 50, r = 50, b = 150, t = 50),
      xaxis = list(
        title = "",
        tickangle = 45,
        showgrid = FALSE,
        ticktext = ~paste0(substr(error_type, 1, 10), "..."),  # First 10 chars + ...
        tickvals = ~reorder(error_type, count)
      ),
      yaxis = list(
        title = "Occurrences",
        showgrid = TRUE,
        gridcolor = error_colors$grid
      )
    )
}

plot_client_heatmap <- function(logs) {
  heatmap_data <- logs %>%
    mutate(
      hour = hour(timestamp),
      day = wday(timestamp, label = TRUE)
    ) %>%
    count(hour, day)
  
  # Custom color scale from light to dark red
  custom_colors <- colorRamp(c("#FEE0E0", error_colors$CRITICAL))
  
  plot_ly(heatmap_data) %>%
    add_trace(
      x = ~hour,
      y = ~day,
      z = ~n,
      type = "heatmap",
      colorscale = list(
        c(0, "rgb(254,224,224)"),
        c(0.5, error_colors$ERROR),
        c(1, error_colors$CRITICAL)
      ),
      hovertemplate = paste(
        "Hour: %{x}:00<br>",
        "Day: %{y}<br>",
        "Errors: %{z}<br>",
        "<extra></extra>"
      )
    ) %>%
    layout(
      paper_bgcolor = error_colors$background,
      plot_bgcolor = error_colors$background,
      font = list(color = error_colors$text),
      margin = list(l = 50, r = 50, b = 50, t = 50),
      xaxis = list(
        title = "Hour of Day",
        ticktext = paste0(0:23, ":00"),
        tickvals = 0:23,
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Day of Week",
        showgrid = FALSE
      )
    )
}

plot_resolution_by_source <- function(logs) {
  resolution_data <- logs %>%
    group_by(source) %>%
    summarise(
      avg_resolution = mean(
        as.numeric(difftime(lead(timestamp), timestamp, units = "mins")),
        na.rm = TRUE
      )
    ) %>%
    arrange(desc(avg_resolution)) %>%
    head(10)  # Top 10 sources
  
  plot_ly(resolution_data) %>%
    add_trace(
      x = ~reorder(source, -avg_resolution),
      y = ~avg_resolution,
      type = "bar",
      marker = list(
        color = error_colors$ERROR,
        line = list(color = error_colors$CRITICAL, width = 1)
      ),
      hoverinfo = "text",
      text = ~paste(
        round(avg_resolution, 1), "min"
      )
    ) %>%
    layout(
      paper_bgcolor = error_colors$background,
      plot_bgcolor = error_colors$background,
      font = list(color = error_colors$text),
      margin = list(l = 50, r = 50, b = 100, t = 50),
      xaxis = list(
        title = "",
        tickangle = 45,
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Average Resolution Time (minutes)",
        showgrid = TRUE,
        gridcolor = error_colors$grid
      )
    )
}

plot_hourly_error_pattern <- function(logs) {
  pattern_data <- logs %>%
    mutate(hour = hour(timestamp)) %>%
    count(hour, level) %>%
    complete(hour = 0:23, level = unique(level), fill = list(n = 0))
  
  plot_ly(pattern_data) %>%
    add_trace(
      x = ~hour,
      y = ~n,
      color = ~level,
      colors = c(
        "CRITICAL" = error_colors$CRITICAL,
        "ERROR" = error_colors$ERROR,
        "WARN" = error_colors$WARN
      ),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 8),
      line = list(shape = "spline"),
      hoverinfo = "text",
      text = ~paste(
        "Hour:", sprintf("%02d:00", hour),
        "<br>Level:", level,
        "<br>Count:", n
      )
    ) %>%
    layout(
      showlegend = TRUE,
      paper_bgcolor = error_colors$background,
      plot_bgcolor = error_colors$background,
      font = list(color = error_colors$text),
      margin = list(l = 50, r = 50, b = 50, t = 50),
      xaxis = list(
        title = "",
        ticktext = sprintf("%02d:00", 0:23),
        tickvals = 0:23,
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Number of Errors",
        showgrid = TRUE,
        gridcolor = error_colors$grid
      )
    )
}

plot_error_timeline <- function(logs, time_aggregation = "hours") {
  time_aggregation <- time_aggregation %||% "hours"  # Default if NULL
  
  timeline_data <- logs %>%
    mutate(interval = floor_date(timestamp, unit = time_aggregation)) %>%
    count(interval, level) %>%
    complete(
      interval = seq(min(interval), max(interval), by = time_aggregation),
      level = unique(level),
      fill = list(n = 0)
    )
  
  plot_ly(timeline_data) %>%
    add_trace(
      x = ~interval,
      y = ~n,
      color = ~level,
      colors = c(
        "CRITICAL" = error_colors$CRITICAL,
        "ERROR" = error_colors$ERROR,
        "WARN" = error_colors$WARN
      ),
      type = "scatter",
      mode = "lines",
      line = list(width = 2),
      stackgroup = "one",
      hoverinfo = "text",
      text = ~paste(
        "Time:", format(interval, "%Y-%m-%d %H:%M"),
        "<br>Level:", level,
        "<br>Count:", n
      )
    ) %>%
    layout(
      showlegend = TRUE,
      paper_bgcolor = error_colors$background,
      plot_bgcolor = error_colors$background,
      font = list(color = error_colors$text),
      margin = list(l = 50, r = 50, b = 50, t = 50),
      xaxis = list(
        title = "",
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Number of Errors",
        showgrid = TRUE,
        gridcolor = error_colors$grid
      ),
      hovermode = "x unified"
    )
}