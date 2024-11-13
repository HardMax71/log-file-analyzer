# R/utils.R
calculate_resolution_time <- function(logs) {
  if (nrow(logs) == 0) return(0)
  
  # Calculate time between error and next success
  resolution_times <- logs %>%
    arrange(timestamp) %>%
    group_by(source) %>%
    # Create a temporary data frame with only INFO timestamps
    group_modify(function(group_data, group_key) {
      info_times <- group_data %>%
        filter(level == "INFO") %>%
        pull(timestamp)
      
      error_data <- group_data %>%
        filter(level %in% c("ERROR", "CRITICAL")) %>%
        mutate(
          next_success = sapply(timestamp, function(error_time) {
            success_times <- info_times[info_times > error_time]
            if (length(success_times) > 0) min(success_times) else NA
          }),
          resolution_time = as.numeric(difftime(next_success, timestamp, units = "mins"))
        )
      
      data.frame(
        error_time = error_data$timestamp,
        resolution_time = error_data$resolution_time
      )
    }) %>%
    ungroup()
  
  # Calculate mean resolution time
  mean_resolution <- resolution_times %>%
    pull(resolution_time) %>%
    mean(na.rm = TRUE)
  
  # Return 0 if no valid resolution times found
  if (is.na(mean_resolution)) return(0)
  
  return(mean_resolution)
}

generate_comparison_data <- function(logs, compare_type, metric_type, params) {
  result <- switch(compare_type,
                   "time" = compare_time_periods(logs, params$time_ranges, metric_type),
                   "source" = compare_sources(logs, params$sources, metric_type),
                   "level" = compare_levels(logs, params$levels, metric_type)
  )
  
  # Ensure consistent column names
  names(result) <- c("category", "value")
  result
}

format_comparison_table <- function(data) {
  if(nrow(data) == 0) {
    return(data.frame(
      Category = character(0),
      Value = numeric(0)
    ))
  }
  
  datatable(data,
            colnames = c("Category", "Value"),
            options = list(
              pageLength = 10,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            ),
            extensions = 'Buttons'
  )
}

calculate_metric <- function(data, metric_type) {
  switch(metric_type,
         "count" = nrow(data),
         "error_rate" = mean(data$level %in% c("ERROR", "CRITICAL", "WARN")) * 100,
         "resolution_time" = calculate_resolution_time(data)
  )
}


compare_time_periods <- function(logs, time_ranges, metric_type) {
  if(is.null(time_ranges)) {
    end_time <- max(logs$timestamp)
    start_time <- end_time - days(1)
    
    data.frame(
      category = "Last 24 hours",
      value = calculate_metric(
        logs %>% filter(timestamp >= start_time, timestamp <= end_time),
        metric_type
      )
    )
  } else {
    data.frame(
      category = "Selected Period",
      value = calculate_metric(
        logs %>% filter(timestamp >= time_ranges[1], timestamp <= time_ranges[2]),
        metric_type
      )
    )
  }
}


compare_sources <- function(logs, sources, metric_type) {
  if(is.null(sources)) {
    sources <- unique(logs$source)
  }
  
  logs %>%
    filter(source %in% sources) %>%
    group_by(source) %>%
    summarise(
      value = calculate_metric(cur_data(), metric_type)
    ) %>%
    rename(category = source)
}

compare_levels <- function(logs, levels, metric_type) {
  if(is.null(levels)) {
    levels <- unique(logs$level)
  }
  
  logs %>%
    filter(level %in% levels) %>%
    group_by(level) %>%
    summarise(
      value = calculate_metric(cur_data(), metric_type)
    ) %>%
    rename(category = level)
}

calculate_stat_summary <- function(data) {
  if(nrow(data) == 0) return("No data available")
  
  summary_stats <- data %>%
    summarise(
      Mean = mean(value, na.rm = TRUE),
      Median = median(value, na.rm = TRUE),
      SD = sd(value, na.rm = TRUE),
      Min = min(value, na.rm = TRUE),
      Max = max(value, na.rm = TRUE)
    )
  
  print(summary_stats, digits = 2)
}

analyze_trends <- function(data) {
  if(nrow(data) < 2) return("Insufficient data for trend analysis")
  
  tryCatch({
    trend <- lm(value ~ seq_len(nrow(data)), data = data)
    slope <- coef(trend)[2]
    direction <- if(slope > 0) "increasing" else "decreasing"
    
    sprintf("Overall trend is %s (slope: %.3f)", direction, slope)
  }, error = function(e) {
    "Error calculating trend"
  })
}

plot_comparison <- function(data, compare_type, metric_type) {
  if(nrow(data) == 0) {
    return(plot_ly() %>%
             layout(title = "No data available"))
  }
  
  plot_ly(data,
          x = ~category,
          y = ~value,
          type = "bar",
          name = "Comparison") %>%
    layout(
      title = paste("Comparison by", capitalize(compare_type)),
      xaxis = list(title = capitalize(compare_type)),
      yaxis = list(title = paste(capitalize(gsub("_", " ", metric_type)),
                                 get_metric_unit(metric_type)))
    )
}

get_metric_unit <- function(metric_type) {
  switch(metric_type,
         "count" = "(count)",
         "error_rate" = "(%)",
         "resolution_time" = "(minutes)",
         "")
}

capitalize <- function(x) {
  # Simple title case conversion
  tools::toTitleCase(gsub("_", " ", tolower(x)))
}