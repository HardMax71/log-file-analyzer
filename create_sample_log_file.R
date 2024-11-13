# Enhanced Log File Generator
library(lubridate)
library(stringi)

# Set seed for reproducibility
set.seed(123)

# Configuration constants
N_LOGS <- 5000

# Expanded system components and services
COMPONENTS <- list(
  web = c("nginx", "apache", "haproxy"),
  database = c("postgresql", "mongodb", "redis", "mysql"),
  application = c("user-service", "payment-service", "auth-service", "notification-service", "api-gateway"),
  infrastructure = c("kubernetes", "docker", "aws-lambda", "ec2-instance"),
  security = c("waf", "firewall", "auth0", "cert-manager"),
  monitoring = c("prometheus", "grafana", "elasticsearch")
)

# Log levels with weighted probabilities
LOG_LEVELS <- c(
  "DEBUG" = 0.1,
  "INFO" = 0.6,
  "WARN" = 0.15,
  "ERROR" = 0.1,
  "CRITICAL" = 0.05
)

# HTTP status codes and their descriptions
HTTP_CODES <- c(
  "200" = "OK",
  "201" = "Created",
  "301" = "Moved Permanently",
  "400" = "Bad Request",
  "401" = "Unauthorized",
  "403" = "Forbidden",
  "404" = "Not Found",
  "429" = "Too Many Requests",
  "500" = "Internal Server Error",
  "503" = "Service Unavailable"
)

# Database error codes
DB_ERRORS <- c(
  "PG001" = "Out of memory",
  "PG023" = "Too many connections",
  "MY1045" = "Access denied",
  "MY1213" = "Deadlock found",
  "MG101" = "Authentication failed",
  "RD001" = "Connection refused"
)

# Generate realistic IP addresses
generate_ip <- function(n) {
  sapply(1:n, function(x) {
    paste(sample(1:255, 4, replace = TRUE), collapse = ".")
  })
}

# Generate realistic user agents
USER_AGENTS <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15",
  "Mozilla/5.0 (iPhone; CPU iPhone OS 14_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0 Mobile/15E148 Safari/604.1",
  "Mozilla/5.0 (Linux; Android 11; SM-G991B) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.120 Mobile Safari/537.36"
)

# Function to generate random datetime with more realistic patterns
generate_datetime <- function(n, start_date, end_date) {
  # Create base timestamps
  base_times <- seq(from = as.POSIXct(start_date),
                    to = as.POSIXct(end_date),
                    length.out = n)
  
  # Add some randomness but maintain chronological order
  jitter <- runif(n, -300, 300) # +/- 5 minutes jitter
  times <- base_times + jitter
  sort(times)
}

# Function to generate realistic messages based on component and level
generate_message <- function(component, level, http_code = NULL, db_error = NULL) {
  messages <- switch(level,
                     "DEBUG" = c(
                       paste("Processing request for", sample(c("user authentication", "payment verification", "data sync", "cache update"), 1)),
                       paste("Cache hit ratio:", round(runif(1), 2)),
                       paste("Query execution time:", round(runif(1, 0.001, 2), 3), "seconds")
                     ),
                     "INFO" = c(
                       paste("Successfully processed transaction", stri_rand_strings(1, 10, pattern = "[A-Z0-9]")),
                       paste("User", sample(1000:9999, 1), "logged in successfully"),
                       paste("API request completed in", round(runif(1, 0.1, 2), 3), "seconds")
                     ),
                     "WARN" = c(
                       "High memory usage detected",
                       "Slow query performance",
                       "Rate limit threshold approaching",
                       paste("Retry attempt", sample(1:3, 1), "of 3")
                     ),
                     "ERROR" = c(
                       paste("Database connection failed:", ifelse(!is.null(db_error), db_error, "unknown error")),
                       paste("HTTP request failed with status", ifelse(!is.null(http_code), http_code, "unknown status")),
                       "Authentication token expired",
                       "Service timeout exceeded"
                     ),
                     "CRITICAL" = c(
                       "System health check failed",
                       "Critical security breach detected",
                       "Database cluster unavailable",
                       "Memory allocation failed"
                     )
  )
  
  sample(messages, 1)
}

# Main log generation function
generate_logs <- function(n_logs) {
  # Generate timestamps with realistic patterns
  timestamps <- generate_datetime(n_logs, "2024-01-01 00:00:00", "2024-01-31 23:59:59")
  
  # Generate component sources
  components <- unlist(COMPONENTS)
  sources <- sample(components, n_logs, replace = TRUE)
  
  # Generate log levels with specified probabilities
  levels <- sample(names(LOG_LEVELS), n_logs, replace = TRUE, prob = unname(LOG_LEVELS))
  
  # Generate IPs and User Agents
  ips <- generate_ip(n_logs)
  user_agents <- sample(USER_AGENTS, n_logs, replace = TRUE)
  
  # Generate request IDs
  request_ids <- sapply(1:n_logs, function(x) paste0("req-", stri_rand_strings(1, 8, pattern = "[A-Z0-9]")))
  
  # Initialize empty vectors for messages and metadata
  messages <- character(n_logs)
  metadata <- character(n_logs)
  
  # Generate messages and metadata for each log entry
  for (i in 1:n_logs) {
    # Generate HTTP codes and DB errors when appropriate
    http_code <- if(grepl("nginx|apache|api", sources[i]) && levels[i] %in% c("ERROR", "WARN")) {
      sample(names(HTTP_CODES), 1)
    } else NULL
    
    db_error <- if(grepl("postgresql|mongodb|mysql", sources[i]) && levels[i] %in% c("ERROR", "CRITICAL")) {
      sample(names(DB_ERRORS), 1)
    } else NULL
    
    # Generate message
    messages[i] <- generate_message(sources[i], levels[i], http_code, db_error)
    
    # Generate metadata
    metadata[i] <- paste(
      paste0("request_id=", request_ids[i]),
      paste0("client_ip=", ips[i]),
      paste0("user_agent=\"", user_agents[i], "\""),
      if(!is.null(http_code)) paste0("status_code=", http_code),
      if(!is.null(db_error)) paste0("error_code=", db_error),
      sep = " | "
    )
  }
  
  # Combine all elements into log entries
  log_entries <- paste(
    format(timestamps, "%Y-%m-%d %H:%M:%S.%OS3"),
    paste0("[", levels, "]"),
    paste0(sources, ":"),
    messages,
    paste0("{", metadata, "}"),
    sep = " "
  )
  
  return(log_entries)
}

# Generate logs
log_entries <- generate_logs(N_LOGS)

# Write logs to file
log_file_path <- "sample_logs.txt"
writeLines(log_entries, log_file_path)
print(paste("Enhanced log file created at:", log_file_path))

# Generate some statistics
cat("\nLog Generation Statistics:\n")
cat("Total log entries:", length(log_entries), "\n")
cat("Log level distribution:\n")
print(table(factor(sapply(strsplit(log_entries, "\\[|\\]"), function(x) x[2]))) / length(log_entries))
