# R/modules/loading_module.R

loadingUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "loading-indicator",
      style = "text-align: center; padding: 20px;",
      icon("terminal", class = "fa-spin fa-3x"),
      h4("Loading Data...", style = "margin-top: 15px;")
    )
  )
}

loadingServer <- function(input, output, session) {
  # No server logic needed for this simple module
}