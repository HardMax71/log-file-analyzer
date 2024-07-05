# Set seed for reproducibility
set.seed(123)

# Function to generate random datetime within a range
random_datetime <- function(start, end) {
  start_time <- as.POSIXct(start)
  end_time <- as.POSIXct(end)
  random_time <- as.POSIXct(runif(1, as.numeric(start_time), as.numeric(end_time)), origin="1970-01-01")
  return(format(random_time, "%Y-%m-%d %H:%M:%S"))
}

# Generate sample data
n_logs <- 1000
log_types <- c("INFO", "WARNING", "ERROR", "DEBUG")
issuers <- c("App", "DatabaseService", "SecurityModule", "PaymentGateway", "UserService")

# Generate logs
logs <- data.frame(
  datetime = sapply(1:n_logs, function(x) random_datetime("2024-07-01 00:00:00", "2024-07-05 23:59:59")),
  log_type = sample(log_types, n_logs, replace = TRUE),
  issuer = sample(issuers, n_logs, replace = TRUE),
  user_id = sample(1000:9999, n_logs, replace = TRUE),
  transaction_id = sapply(1:n_logs, function(x) paste0("TR", sample(10000:99999, 1))),
  status = sample(c("success", "failure", "pending"), n_logs, replace = TRUE),
  error_code = sample(c(NA, "E001", "E002", "E003", "E004"), n_logs, replace = TRUE, prob = c(0.7, 0.075, 0.075, 0.075, 0.075))
)

# Function to create log message
create_log_message <- function(row) {
  msg <- switch(row["log_type"],
                "INFO" = paste("User action completed -", ifelse(row["status"] == "success", "successfully", paste("with status:", row["status"]))),
                "WARNING" = "Potential issue detected",
                "ERROR" = paste("Error occurred -", ifelse(is.na(row["error_code"]), "unknown error", paste("error code:", row["error_code"]))),
                "DEBUG" = "Debug information"
  )
  return(paste(msg, "- user_id:", row["user_id"], "- transaction_id:", row["transaction_id"]))
}

# Create log entries
log_entries <- apply(logs, 1, function(row) {
  paste(row["datetime"], 
        paste0("[", row["log_type"], "]"), 
        paste0(row["issuer"], ":"), 
        create_log_message(row))
})

# Get the directory of the current script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the script's location
setwd(script_dir)

# Write logs to file in the script's directory
log_file_path <- file.path(script_dir, "sample_logs.txt")
writeLines(log_entries, log_file_path)

print(paste("Log file created at:", log_file_path))