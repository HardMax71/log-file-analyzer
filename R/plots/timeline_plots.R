# R/plots/timeline_plots.R

# Common color palette
level_colors <- c(
  "CRITICAL" = "#721C24",
  "ERROR" = "#DC3545",
  "WARNING" = "#FFC107", 
  "INFO" = "#6C757D",
  "DEBUG" = "#ADB5BD"
)

plot_timeline <- function(logs, interval = "hours") {
  if (nrow(logs) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  # Calculate interval sequence from timestamp
  logs_agg <- logs %>%
    mutate(interval = floor_date(timestamp, unit = interval)) %>%
    count(interval, level) %>%
    arrange(interval)
  
  # Fill gaps in time sequence
  full_seq <- seq(
    from = min(logs_agg$interval),
    to = max(logs_agg$interval),
    by = interval
  )
  
  logs_agg <- logs_agg %>%
    complete(
      interval = full_seq,
      level = unique(level),
      fill = list(n = 0)
    )
  
  plot_ly(data = logs_agg) %>%
    add_trace(
      x = ~interval,
      y = ~n,
      color = ~level,
      colors = level_colors,
      type = "scatter",
      mode = "lines",
      stackgroup = "one",
      hoverinfo = "text",
      text = ~paste(
        "Time:", format(interval, "%Y-%m-%d %H:%M"),
        "<br>Level:", level,
        "<br>Count:", n
      )
    ) %>%
    layout(
      title = list(text = "Log Activity Timeline"),
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "Number of Logs", showgrid = TRUE),
      showlegend = TRUE,
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
}

plot_hourly_pattern <- function(logs) {
  if (nrow(logs) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  hourly_pattern <- logs %>%
    mutate(hour = hour(timestamp)) %>%
    count(hour, level) %>%
    complete(hour = 0:23, level = unique(level), fill = list(n = 0))
  
  plot_ly(data = hourly_pattern) %>%
    add_trace(
      x = ~hour,
      y = ~n,
      color = ~level,
      colors = level_colors,
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 8),
      line = list(shape = "spline", smoothing = 1),
      hoverinfo = "text",
      text = ~paste(
        "Hour:", sprintf("%02d:00", hour),
        "<br>Level:", level,
        "<br>Count:", n
      )
    ) %>%
    layout(
      title = list(
        text = "24-Hour Log Pattern",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "",
        ticktext = sprintf("%02d:00", 0:23),
        tickvals = 0:23,
        showgrid = FALSE
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

plot_daily_pattern <- function(logs) {
  if (nrow(logs) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  daily_pattern <- logs %>%
    mutate(wday = wday(timestamp, label = TRUE)) %>%
    count(wday, level) %>%
    complete(wday, level = unique(level), fill = list(n = 0))
  
  plot_ly(data = daily_pattern) %>%
    add_trace(
      x = ~wday,
      y = ~n,
      color = ~level,
      colors = level_colors,
      type = "bar",
      hoverinfo = "text",
      text = ~paste(
        "Day:", wday,
        "<br>Level:", level,
        "<br>Count:", n
      )
    ) %>%
    layout(
      title = list(
        text = "Daily Log Distribution",
        font = list(size = 16)
      ),
      barmode = "stack",
      xaxis = list(
        title = "",
        showgrid = FALSE
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

plot_anomalies <- function(logs, threshold = 2, window = 24) {
  if (nrow(logs) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  hourly_counts <- logs %>%
    mutate(hour_interval = floor_date(timestamp, "hour")) %>%
    count(hour_interval) %>%
    arrange(hour_interval)
  
  if(nrow(hourly_counts) < window) {
    return(plot_ly() %>%
             layout(
               title = paste0("Insufficient data (need at least ", window, " hours)"),
               annotations = list(
                 x = 0.5,
                 y = 0.5,
                 text = paste0("Need at least ", window, " hours of data"),
                 showarrow = FALSE
               )
             ))
  }
  
  roll_stats <- hourly_counts %>%
    mutate(
      roll_mean = rollapply(n, window, mean, fill = NA, align = "right"),
      roll_sd = rollapply(n, window, sd, fill = NA, align = "right"),
      upper_bound = roll_mean + threshold * roll_sd,
      lower_bound = roll_mean - threshold * roll_sd
    ) %>%
    mutate(
      anomaly_type = case_when(
        n > upper_bound ~ "High",
        n < lower_bound ~ "Low",
        TRUE ~ "Normal"
      )
    )
  
  plot_ly() %>%
    add_trace(
      data = roll_stats,
      x = ~hour_interval,
      y = ~roll_mean,
      name = "Expected",
      type = "scatter",
      mode = "lines",
      line = list(color = "#6C757D", width = 2),
      hoverinfo = "text",
      text = ~paste(
        "Time:", format(hour_interval, "%Y-%m-%d %H:%M"),
        "<br>Expected:", round(roll_mean, 1)
      )
    ) %>%
    add_trace(
      data = roll_stats,
      x = ~hour_interval,
      ymin = ~lower_bound,
      ymax = ~upper_bound,
      type = "scatter",
      mode = "lines",
      fill = "tonexty",
      name = "Normal Range",
      fillcolor = 'rgba(108, 117, 125, 0.2)',
      line = list(color = 'transparent'),
      showlegend = FALSE
    ) %>%
    add_trace(
      data = roll_stats,
      x = ~hour_interval,
      y = ~n,
      name = "Actual",
      type = "scatter",
      mode = "lines",
      line = list(color = '#ADB5BD', width = 1),
      hoverinfo = "text",
      text = ~paste(
        "Time:", format(hour_interval, "%Y-%m-%d %H:%M"),
        "<br>Count:", n
      )
    ) %>%
    add_trace(
      data = subset(roll_stats, anomaly_type == "High"),
      x = ~hour_interval,
      y = ~n,
      name = "High Anomaly",
      type = "scatter",
      mode = "markers",
      marker = list(color = '#DC3545', size = 10),
      hoverinfo = "text",
      text = ~paste(
        "Time:", format(hour_interval, "%Y-%m-%d %H:%M"),
        "<br>Count:", n,
        "<br>Expected:", round(roll_mean, 1)
      )
    ) %>%
    add_trace(
      data = subset(roll_stats, anomaly_type == "Low"),
      x = ~hour_interval,
      y = ~n,
      name = "Low Anomaly", 
      type = "scatter",
      mode = "markers",
      marker = list(color = '#FFC107', size = 10),
      hoverinfo = "text",
      text = ~paste(
        "Time:", format(hour_interval, "%Y-%m-%d %H:%M"),
        "<br>Count:", n,
        "<br>Expected:", round(roll_mean, 1)
      )
    ) %>%
    layout(
      title = list(
        text = "Anomaly Detection",
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