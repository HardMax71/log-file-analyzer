# global.R
# Load required packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
library(waiter)

# Source all R files
lapply(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE), source)