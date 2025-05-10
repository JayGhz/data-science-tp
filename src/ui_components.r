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
    ),
    menuItem(
      "Preprocesamiento de Datos",
      tabName = "preprocesamiento_datos",
      icon = icon("filter")
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
    ),
    tabItem(tabName = "preprocesamiento_datos",
      fluidRow(
        # Tratamiento de Valores Cero
        box(
          title = "Tratamiento de Valores Cero",
          width = 12,
          p("Reemplazar valores 0 con NA en columnas seleccionadas"),
          selectInput("columnas_cero", "Selecciona columnas donde reemplazar ceros:", choices = NULL, multiple = TRUE),
          actionButton("btn_reemplazar_ceros", "Reemplazar Ceros con NA", 
                       icon = icon("exchange-alt"), 
                       class = "btn-warning"),
          verbatimTextOutput("resultado_ceros")
        )
      ),
      fluidRow(
        # Identificación de Datos Faltantes
        box(
          title = "Identificación de Datos Faltantes",
          width = 6,
          actionButton("btn_identificar_na", "Identificar Datos Faltantes", 
                       icon = icon("search"), 
                       class = "btn-primary", 
                       style = "margin-bottom: 15px;"),
          verbatimTextOutput("resumen_na"),
          plotOutput("grafico_na", height = "300px")
        ),
        # Identificación de Datos Atípicos
        box(
          title = "Identificación de Datos Atípicos (Outliers)",
          width = 6,
          selectInput("columna_outliers", "Selecciona columna numérica:", choices = NULL),
          actionButton("btn_identificar_outliers", "Identificar Outliers", 
                       icon = icon("search"), 
                       class = "btn-primary", 
                       style = "margin-bottom: 15px;"),
          plotOutput("boxplot_outliers", height = "300px"),
          verbatimTextOutput("resumen_outliers")
        )
      ),
      fluidRow(
        # Tratamiento de Datos Faltantes
        box(
          title = "Tratamiento de Datos Faltantes",
          width = 6,
          selectInput("metodo_na", "Selecciona método de tratamiento:",
                     choices = c("Eliminar filas" = "eliminar",
                                "Imputar con media" = "media",
                                "Imputar con mediana" = "mediana",
                                "Imputar con moda" = "moda")),
          selectInput("columnas_na", "Selecciona columnas a tratar:", choices = NULL, multiple = TRUE),
          actionButton("btn_tratar_na", "Aplicar Tratamiento", 
                       icon = icon("wrench"), 
                       class = "btn-success"),
          verbatimTextOutput("resultado_na")
        ),
        # Tratamiento de Datos Atípicos
        box(
          title = "Tratamiento de Datos Atípicos",
          width = 6,
          selectInput("metodo_outliers", "Selecciona método de tratamiento:",
                     choices = c("Eliminar outliers" = "eliminar",
                                "Winsorización" = "winsor",
                                "Reemplazar con límites" = "limites")),
          actionButton("btn_tratar_outliers", "Aplicar Tratamiento", 
                       icon = icon("wrench"), 
                       class = "btn-success"),
          verbatimTextOutput("resultado_outliers")
        )
      )
    )
  )
)
