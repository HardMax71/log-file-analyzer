# R/plots/distribution_plots.R
plot_level_distribution <- function(logs) {
  p <- logs %>%
    count(level) %>%
    plot_ly(x = ~level, y = ~n, type = "bar",
            marker = list(color = ~case_when(
              level %in% c("ERROR", "CRITICAL") ~ "red",
              level == "WARNING" ~ "orange",
              TRUE ~ "steelblue"
            ))) %>%
    layout(title = "Log Level Distribution",
           xaxis = list(title = "Log Level"),
           yaxis = list(title = "Count"))
  return(p)
}

plot_source_distribution <- function(logs) {
  p <- logs %>%
    count(source) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    plot_ly(x = ~reorder(source, n), y = ~n, type = "bar",
            marker = list(color = "steelblue")) %>%
    layout(title = "Top 10 Sources",
           xaxis = list(title = "Source",
                        tickangle = 45),
           yaxis = list(title = "Count"))
  return(p)
}