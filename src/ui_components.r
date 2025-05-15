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
      "Preprocesamiento",
      tabName = "preprocesamiento_datos",
      icon = icon("filter")
    ),
    menuItem(
      "Visualización",
      tabName = "visualizacion_datos",
      icon = icon("chart-bar")
    )
  )
)

body_ui <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$style(HTML("
      .main-header {
        position: fixed;
        width: 100%;
        z-index: 1030;
      }
      .main-sidebar {
        position: fixed;
        height: 100%;
        z-index: 1020;
      }
      .content-wrapper {
        padding-top: 50px;
      }
    "))
  ),
  tabItems(
    tabItem(
      tabName = "carga_datos",
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
    tabItem(
      tabName = "inspeccionar_datos",
      fluidRow(
        box(
          title = "Estructura de los Datos",
          width = 12,
          verbatimTextOutput("data_structure"),
          actionButton("btn_convertir_factores", "Convertir Variables Categoricas a Factores",
            icon = icon("exchange-alt"),
            class = "btn-primary",
            style = "margin-top: 15px;"
          ),
          div(
            style = "margin-top: 25px;",
            verbatimTextOutput("resultado_conversion")
          )
        )
      ),
      fluidRow(
        box(
          title = "Resumen de los Datos",
          width = 12,
          uiOutput("resumen_variables")
        )
      )
    ),
    tabItem(
      tabName = "preprocesamiento_datos",
      fluidRow(
        # Identificación de Datos Faltantes
        box(
          title = "Identificación de Datos Faltantes",
          width = 6,
          actionButton("btn_identificar_na", "Identificar Datos Faltantes",
            icon = icon("search"),
            class = "btn-primary",
            style = "margin-bottom: 15px;"
          ),
          verbatimTextOutput("resumen_na"),
          plotOutput("grafico_na", height = "300px")
        ),
        # Identificación de Datos Atípicos
        box(
          title = "Identificación de Datos Atípicos (Outliers)",
          width = 6,
          selectInput("columna_outliers", "Selecciona una columna numérica:", choices = NULL, multiple = FALSE),
          actionButton("btn_identificar_outliers", "Identificar Outliers",
            icon = icon("search"),
            class = "btn-primary",
            style = "margin-bottom: 15px;"
          ),
          plotOutput("boxplot_outliers", height = "300px"),
          verbatimTextOutput("resumen_outliers")
        )
      ),
      fluidRow(
        # Tratamiento de Datos Faltantes
        box(
          title = "Tratamiento de Datos Faltantes",
          width = 12,
          selectInput("metodo_na", "Selecciona método de tratamiento:",
            choices = c(
              "Eliminar filas" = "eliminar_filas",
              "Eliminar columnas" = "eliminar_columnas",
              "Imputar con media" = "media",
              "Imputar con mediana" = "mediana",
              "Imputar con moda" = "moda"
            )
          ),
          selectInput("columnas_na", "Selecciona columnas a tratar:", choices = NULL, multiple = TRUE),
          checkboxInput("todas_columnas_na", "Seleccionar todas las columnas", FALSE),
          actionButton("btn_tratar_na", "Aplicar Tratamiento",
            icon = icon("wrench"),
            class = "btn-success"
          ),
          verbatimTextOutput("resultado_na")
        ),
        # Tratamiento de Datos Atípicos
        box(
          title = "Tratamiento de Datos Atípicos",
          width = 12,
          selectInput("columnas_tratamiento_outliers", "Selecciona columnas a tratar:", choices = NULL, multiple = TRUE),
          checkboxInput("todas_columnas_outliers", "Seleccionar todas las columnas numéricas", FALSE),
          selectInput("metodo_outliers", "Selecciona método de tratamiento:",
            choices = c(
              "Eliminar outliers" = "eliminar",
              "Winsorización" = "winsor",
              "Reemplazar con límites" = "limites"
            )
          ),
          actionButton("btn_tratar_outliers", "Aplicar Tratamiento",
            icon = icon("wrench"),
            class = "btn-success"
          ),
          verbatimTextOutput("resultado_outliers")
        )
      ),
      fluidRow(
        box(
          title = "Resumen de Datos Limpios",
          width = 12,
          actionButton("btn_resumen_limpieza", "Ver Resumen de Datos Limpios",
            icon = icon("check-circle"),
            class = "btn-info",
            style = "margin-bottom: 15px;"
          ),
          verbatimTextOutput("resumen_limpieza")
        )
      )
    ),
    tabItem(
      tabName = "visualizacion_datos",
      fluidRow(
        box(
          title = "Análisis Visual de Reservas Hoteleras",
          width = 12,
          p("El análisis visual de los datos de reservas hoteleras nos permite identificar patrones, tendencias y preferencias de los clientes.
            Estas visualizaciones nos ayudan a tomar decisiones estratégicas para mejorar la gestión hotelera, optimizar recursos y
            aumentar la satisfacción del cliente.")
        )
      ),
      # Pregunta 1: Tipo de hotel
      fluidRow(
        box(
          title = "¿Cuántas reservas se realizan por tipo de hotel?",
          width = 6,
          actionButton("btn_grafico_tipo_hotel", "Generar Gráfico",
            icon = icon("chart-pie"),
            class = "btn-primary"
          ),
          plotOutput("grafico_tipo_hotel", height = "300px")
        ),
        box(
          title = "¿Es importante contar con espacios de estacionamiento?",
          width = 6,
          actionButton("btn_grafico_estacionamiento", "Generar Gráfico",
            icon = icon("car"),
            class = "btn-primary"
          ),
          plotOutput("grafico_estacionamiento", height = "300px")
        ),
        fluidRow(
          box(
            title = "¿Está aumentando la demanda con el tiempo?",
            width = 6,
            selectInput("anio_demanda", "Seleccionar año:",
              choices = c("Todos" = "todos", "2015" = "2015", "2016" = "2016", "2017" = "2017")
            ),
            actionButton("btn_grafico_demanda_tiempo", "Generar Gráfico",
              icon = icon("chart-line"),
              class = "btn-primary"
            ),
            plotOutput("grafico_demanda_tiempo", height = "300px")
          ),
          box(
            title = "¿Cuándo es menor la demanda de reservas?",
            width = 6,
            selectInput("anio_menor_demanda", "Seleccionar año:",
              choices = c("Todos" = "todos", "2015" = "2015", "2016" = "2016", "2017" = "2017")
            ),
            actionButton("btn_grafico_menor_demanda", "Generar Gráfico",
              icon = icon("chart-bar"),
              class = "btn-primary"
            ),
            plotOutput("grafico_menor_demanda", height = "300px")
          )
        )
      ),
      fluidRow(
        box(
          title = "¿Cuántas reservas incluyen niños y/o bebés?",
          width = 6,
          selectInput("tipo_hotel_ninos", "Tipo de hotel:",
            choices = c("Todos" = "todos", "Resort" = "Resort Hotel", "Ciudad" = "City Hotel")
          ),
          actionButton("btn_grafico_ninos", "Generar Gráfico",
            icon = icon("child"),
            class = "btn-primary"
          ),
          plotOutput("grafico_ninos", height = "300px")
        ),
        box(
          title = "¿Cuáles son las temporadas de reservas (alta, media, baja)?",
          width = 6,
          selectInput("tipo_hotel_temporada", "Tipo de hotel:",
            choices = c("Todos" = "todos", "Resort" = "Resort Hotel", "Ciudad" = "City Hotel")
          ),
          actionButton("btn_grafico_temporadas", "Generar Gráfico",
            icon = icon("calendar"),
            class = "btn-primary"
          ),
          plotOutput("grafico_temporadas", height = "300px")
        ),
      ),
      fluidRow(
        box(
          title = "¿En qué meses del año se producen más cancelaciones de reservas?",
          width = 12,
          selectInput("anio_cancelaciones", "Seleccionar año:",
            choices = c("Todos" = "todos", "2015" = "2015", "2016" = "2016", "2017" = "2017")
          ),
          actionButton("btn_grafico_cancelaciones", "Generar Gráfico",
            icon = icon("calendar-times"),
            class = "btn-primary"
          ),
          plotOutput("grafico_cancelaciones", height = "400px")
        )
      )
    )
  )
)
