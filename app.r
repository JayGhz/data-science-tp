options(shiny.maxRequestSize = 30 * 1024 ^ 2)

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

source("src/ui_components.r")

ui <- dashboardPage(
  dashboardHeader(title = "Data Science - TP"),
  sidebar_ui,
  body_ui
)

source("src/server_logic.R")

server <- function(input, output, session) {
  server_logic(input, output, session)
}

shinyApp(ui, server)