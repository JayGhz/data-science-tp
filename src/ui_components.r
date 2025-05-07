library(shiny)
library(shinydashboard)

sidebar_ui <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    tags$head(
      tags$style(HTML("
        .sidebar-menu > li > a {
          padding: 15px 15px 15px 15px;
          font-size: 16px;
        }
        .sidebar-menu > li > a > i {
          margin-right: 15px;
        }
      "))
    ),
    menuItem(
      "Carga de Datos",
      tabName = "carga_datos",
      icon = icon("upload")
    ),
    menuItem(
      "Inspeccionar Datos",
      tabName = "inspeccionar_datos",
      icon = icon("search")
    )
  )
)

body_ui <- dashboardBody(
  tabItems(
    tabItem(tabName = "carga_datos",
      fluidRow(
        box(
          title = "Cargar Archivos",
          width = 12,
          fileInput("file1", "Selecciona un archivo CSV", accept = c(".csv"))
        ),
        box(
          title = "Tabla de datos de reservas hoteleras",
          width = 12,
          DT::dataTableOutput("data_table")
        )
      )
    ),
    tabItem(tabName = "inspeccionar_datos",
      fluidRow(
        box(
          title = "Estructura de los Datos",
          width = 12,
          verbatimTextOutput("data_structure")
        ),
        box(
          title = "Resumen de los Datos",
          width = 12,
          verbatimTextOutput("data_summary")
        )
      )
    )
  )
)
