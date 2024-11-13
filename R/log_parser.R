library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

parse_log_file <- function(file_path) {
  # Read the log file
  logs <- read_lines(file_path)
  
  # Function to extract metadata from JSON-like string
  extract_metadata <- function(metadata_str) {
    # Return empty list if metadata is missing or malformed
    if (is.na(metadata_str) || !grepl("\\{.*\\}", metadata_str)) {
      return(list())
    }
    
    # Extract content between curly braces and split by pipes
    metadata_pairs <- metadata_str %>%
      str_extract("\\{(.*)\\}") %>%
      str_remove_all("^\\{|\\}$") %>%
      str_split("\\|") %>%
      `[[`(1) %>%
      trimws()
    
    # Process each key-value pair
    metadata_list <- lapply(metadata_pairs, function(pair) {
      if (!grepl("=", pair)) return(NULL)
      
      parts <- str_split(pair, "=", n = 2)[[1]]
      if (length(parts) != 2) return(NULL)
      
      key <- trimws(parts[1])
      value <- trimws(parts[2]) %>%
        str_remove_all("^\"|\"$")  # Remove surrounding quotes
      
      if (key == "") return(NULL)
      setNames(list(value), key)
    })
    
    # Combine all non-NULL metadata entries
    metadata_list <- Filter(Negate(is.null), metadata_list)
    if (length(metadata_list) == 0) return(list())
    
    do.call(c, metadata_list)
  }
  
  # Parse each log entry
  parsed_logs <- lapply(logs, function(log) {
    # Parse main components using regex
    pattern <- paste0(
      "^(\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}\\.\\d+\\.\\d+)\\s+", # timestamp
      "\\[(\\w+)\\]\\s+",    # level
      "([^:]+):\\s+",        # source
      "(.+?)\\s+",           # message
      "\\{(.+)\\}$"          # metadata
    )
    
    matches <- str_match(log, pattern)
    
    if (length(matches) < 6 || is.na(matches[1])) {
      return(NULL)
    }
    
    # Clean and parse timestamp
    timestamp_str <- matches[2] %>%
      str_replace("(\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2})\\.\\d+\\.\\d+", "\\1")
    
    timestamp <- ymd_hms(timestamp_str)
    level <- matches[3]
    source <- matches[4]
    message <- matches[5]
    
    # Extract metadata
    metadata <- extract_metadata(matches[6])
    
    # Create base list with main components
    base_list <- list(
      timestamp = timestamp,
      level = level,
      source = source,
      message = message
    )
    
    # Combine with metadata
    c(base_list, metadata)
  })
  
  # Remove NULL entries and convert to data frame
  parsed_logs <- Filter(Negate(is.null), parsed_logs)
  if (length(parsed_logs) == 0) {
    stop("No valid log entries found")
  }
  
  # Convert to data frame and clean up
  log_df <- bind_rows(lapply(parsed_logs, as.data.frame)) %>%
    mutate(
      timestamp = as.POSIXct(timestamp),
      level = factor(level),
      source = as.character(source),
      message = as.character(message),
      date = as.Date(timestamp),
      hour = hour(timestamp),
      minute = minute(timestamp),
      second = second(timestamp)
    ) %>%
    select(
      timestamp, level, source, message,
      request_id = matches("request_id"),
      client_ip = matches("client_ip"),
      user_agent = matches("user_agent"),
      date, hour, minute, second,
      everything()
    )
  
  # Convert specific columns to factors if they exist
  factor_columns <- c("level", "source", "request_id", "client_ip")
  for (col in factor_columns) {
    if (col %in% names(log_df)) {
      log_df[[col]] <- factor(log_df[[col]])
    }
  }
  
  return(log_df)
}
