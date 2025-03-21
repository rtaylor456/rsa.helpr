library(shiny)
library(data.table)
library(readxl)
library(DT)
library(bit64)

# source("global.R")  # Load shared libraries and functions
source("ui.R")      # Load UI
source("server.R")  # Load server

# Run the application
shinyApp(ui = ui, server = server)
