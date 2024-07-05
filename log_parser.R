library(readr)
library(dplyr)
library(lubridate)

parse_log_file <- function(file_path) {
  # Read the log file
  logs <- read_lines(file_path)
  
  # Parse the log entries
  parsed_logs <- lapply(logs, function(log) {
    parts <- strsplit(log, " ")[[1]]
    datetime <- parse_datetime(paste(parts[1], parts[2]))
    log_type <- gsub("\\[|\\]", "", parts[3])
    issuer <- gsub(":", "", parts[4])
    message <- paste(parts[5:length(parts)], collapse = " ")
    
    # Extract user_id and transaction_id from message
    user_id <- gsub(".*user_id: ([0-9]+).*", "\\1", message)
    transaction_id <- gsub(".*transaction_id: ([^ ]+).*", "\\1", message)
    
    list(
      datetime = datetime,
      log_type = log_type,
      issuer = issuer,
      message = message,
      user_id = user_id,
      transaction_id = transaction_id
    )
  })
  
  # Convert to data frame
  log_df <- do.call(rbind, lapply(parsed_logs, as.data.frame))
  
  return(log_df)
}