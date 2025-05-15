server_logic <- function(input, output, session) {
  source("src/load_data.R", local = TRUE)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(scales)

  # Datos reactivos
  data <- reactive({
    req(input$file1)
    df <- load_data(input$file1$datapath)
    df
  })

  processed_data <- reactiveVal()

  observe({
    processed_data(data())
  })

  # Tabla de datos
  output$data_table <- DT::renderDataTable({
    req(data())
    DT::datatable(
      data(),
      options = list(
        pageLength = 5,
        scrollY = TRUE,
        scrollX = TRUE
      )
    )
  })

  # Estructura y resumen de datos
  output$data_structure <- renderPrint({
    req(data())
    str(data())
  })

  # Resultado de la conversión a factores
  output$resultado_conversion <- renderPrint({
    req(input$btn_convertir_factores, processed_data())
  })

  # Resumen de datos con una ventana por variable
  output$resumen_variables <- renderUI({
    req(processed_data())

    # Crear una lista de cajas, una para cada variable
    var_boxes <- lapply(names(processed_data()), function(var_name) {
      box(
        title = var_name,
        width = 6,
        renderPrint({{ var_data <- processed_data()[[var_name]]

          if (is.factor(var_data)) {
            var_data <- forcats::fct_lump_n(var_data, n = 5, other_level = "Others")
          }

          if (is.numeric(var_data)) {
            summary_stats <- summary(var_data, na.rm = TRUE)
            return(round(summary_stats, 1))
          } else {
            return(summary(var_data, na.rm = TRUE))
          } }})
      )
    })

    # Devolver las cajas en filas fluidas
    do.call(fluidRow, var_boxes)
  })

  # Convertir variables categoricas a factores
  observeEvent(input$btn_convertir_factores, {
    req(processed_data())

    # Obtener datos actuales
    current_data <- processed_data()

    # Lista de variables a convertir a factor
    factor_vars <- c(
      "hotel", "is_canceled", "meal", "country", "market_segment",
      "distribution_channel", "is_repeated_guest", "reserved_room_type",
      "assigned_room_type", "deposit_type", "customer_type",
      "reservation_status", "agent", "company", "arrival_date_month",
      "arrival_date_day_of_month", "arrival_date_year", "previous_bookings_not_canceled"
    )

    # Convertir variables a factor
    for (var in factor_vars) {
      if (var %in% names(current_data)) {
        current_data[[var]] <- as.factor(current_data[[var]])
      }
    }

    # Convertir reservation_status_date a Date
    if ("reservation_status_date" %in% names(current_data)) {
      current_data$reservation_status_date <- as.Date(current_data$reservation_status_date)
    }

    # Actualizar datos procesados
    processed_data(current_data)

    # Mostrar resultado
    output$resultado_conversion <- renderPrint({
      cat("Se han convertido las siguientes variables a factores:\n")
      vars <- factor_vars[factor_vars %in% names(current_data)]
      chunks <- split(vars, ceiling(seq_along(vars) / 5))
      cat(sapply(chunks, function(x) paste(x, collapse = ", ")), sep = "\n")
      cat("\n")

      if ("reservation_status_date" %in% names(current_data)) {
        cat("Se ha convertido 'reservation_status_date' a formato Date\n")
      }

      cat("\nEstructura actualizada de los datos:\n")
      str(processed_data())
    })
  })

  observe({
    req(processed_data())
    updateSelectInput(session, "columnas_na",
      choices = names(processed_data())
    )

    num_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
    updateSelectInput(session, "columna_outliers",
      choices = num_cols
    )

    updateSelectInput(session, "columnas_cero",
      choices = num_cols, selected = NULL
    )

    updateSelectInput(session, "columnas_tratamiento_outliers",
      choices = num_cols
    )
  })

  # Manejar selección de todas las columnas para outliers
  observeEvent(input$todas_columnas_outliers, {
    if (input$todas_columnas_outliers) {
      numeric_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
      updateSelectInput(session, "columnas_tratamiento_outliers", selected = numeric_cols)
    }
  })

  # IDENTIFICACIÓN DE DATOS FALTANTES
  observeEvent(input$btn_identificar_na, {
    req(processed_data())

    na_count <- colSums(is.na(processed_data()))
    na_percent <- round(na_count / nrow(processed_data()) * 100, 1)
    na_summary_out <- data.frame(
      NA_Count = na_count,
      NA_Percent = na_percent
    )
    na_summary <- data.frame(
      Variable = names(na_count),
      NA_Count = na_count,
      NA_Percent = na_percent
    )

    output$resumen_na <- renderPrint({
      na_summary_out[na_summary_out$NA_Count > 0, ]
    })

    output$grafico_na <- renderPlot({
      na_vars <- na_summary$Variable[na_summary$NA_Count > 0]

      if (length(na_vars) > 0) {
        na_df <- na_summary[na_summary$NA_Count > 0, ]
        ggplot(na_df, aes(x = reorder(Variable, -NA_Percent), y = NA_Percent)) +
          geom_bar(stat = "identity", fill = "#3c8dbc") +
          theme_minimal() +
          labs(x = "Variables", y = "Porcentaje de NA (%)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        # Si no hay NA
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(0, 0, "No se encontraron datos faltantes", cex = 1.5)
      }
    })
  })

  # TRATAMIENTO DE DATOS FALTANTES
  observeEvent(input$btn_tratar_na, {
    req(processed_data(), input$columnas_na, input$metodo_na)

    current_data <- processed_data()
    result_msgs <- list() # Inicializar lista de mensajes

    if (input$metodo_na == "eliminar_filas") {
      rows_before <- nrow(current_data)
      current_data <- current_data[complete.cases(current_data[, input$columnas_na]), ]
      rows_removed <- rows_before - nrow(current_data)
    } else if (input$metodo_na == "eliminar_columnas") {
      cols_before <- ncol(current_data)
      current_data <- current_data[, !(names(current_data) %in% input$columnas_na)]
      cols_removed <- cols_before - ncol(current_data)
      result_msg <- paste("Se eliminaron", cols_removed, "columna(s) con datos faltantes mayor al 90%.")
    } else {
      # Imputación
      for (col in input$columnas_na) {
        if (is.numeric(current_data[[col]])) {
          if (input$metodo_na == "media") {
            col_mean <- mean(current_data[[col]], na.rm = TRUE)
            current_data[[col]][is.na(current_data[[col]])] <- col_mean
          } else if (input$metodo_na == "mediana") {
            col_median <- median(current_data[[col]], na.rm = TRUE)
            current_data[[col]][is.na(current_data[[col]])] <- col_median
          }
        } else {
          if (input$metodo_na == "moda") {
            tab <- table(current_data[[col]], useNA = "no")
            col_mode <- names(tab)[which.max(tab)]
            current_data[[col]][is.na(current_data[[col]])] <- col_mode
          } else {
            tab <- table(current_data[[col]], useNA = "no")
            col_mode <- names(tab)[which.max(tab)]
            current_data[[col]][is.na(current_data[[col]])] <- col_mode
          }
        }
      }
    }

    # Actualizar datos procesados
    processed_data(current_data)

    # Mostrar resultado
    output$resultado_na <- renderPrint({
      cat("✓ Tratamiento de datos faltantes completado\n\n")
      cat("Metodo seleccionado:", input$metodo_na, "\n\n")
    })
  })

  # RESUMEN DE DATOS LIMPIOS
  observeEvent(input$btn_resumen_limpieza, {
    req(processed_data())

    current_data <- processed_data()

    # Contar NAs por columna
    na_count <- colSums(is.na(current_data))
    total_na <- sum(na_count)

    # Contar outliers en columnas numéricas
    num_cols <- names(current_data)[sapply(current_data, is.numeric)]
    outliers_count <- sapply(num_cols, function(col) {
      data <- current_data[[col]]
      q1 <- quantile(data, 0.25, na.rm = TRUE)
      q3 <- quantile(data, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      sum(data < (q1 - 1.5 * iqr) | data > (q3 + 1.5 * iqr), na.rm = TRUE)
    })
    total_outliers <- sum(outliers_count)

    output$resumen_limpieza <- renderPrint({
      cat("RESUMEN DE LIMPIEZA DE DATOS\n")
      cat("============================\n\n")

      cat("1. DATOS FALTANTES (NA)\n")
      cat("   Total de NA en el conjunto de datos:", total_na, "\n")
      if (total_na > 0) {
        cat("   Distribución de NA por columna:\n")
        na_cols <- na_count[na_count > 0]
        for (col in names(na_cols)) {
          cat(sprintf(
            "   - %s: %d NA (%.1f%%)\n",
            col, na_cols[col],
            na_cols[col] / nrow(current_data) * 100
          ))
        }
      }

      cat("\n2. DATOS ATÍPICOS (OUTLIERS)\n")
      cat("   Total de outliers detectados:", total_outliers, "\n")
      if (total_outliers > 0) {
        cat("   Distribución de outliers por columna numérica:\n")
        outliers_cols <- outliers_count[outliers_count > 0]
        for (col in names(outliers_cols)) {
          cat(sprintf(
            "   - %s: %d outliers (%.1f%%)\n",
            col, outliers_cols[col],
            outliers_cols[col] / nrow(current_data) * 100
          ))
        }
      }

      cat("\n3. DIMENSIONES ACTUALES\n")
      cat(sprintf(
        "   Filas: %d\n   Columnas: %d\n",
        nrow(current_data), ncol(current_data)
      ))
    })
  })

  # IDENTIFICACIÓN DE OUTLIERS
  observeEvent(input$btn_identificar_outliers, {
    req(processed_data(), input$columna_outliers)

    col_data <- processed_data()[[input$columna_outliers]]

    q1 <- quantile(col_data, 0.25, na.rm = TRUE)
    q3 <- quantile(col_data, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr

    outliers <- col_data[col_data < lower_bound | col_data > upper_bound]

    output$boxplot_outliers <- renderPlot({
      ggplot(processed_data(), aes_string(y = input$columna_outliers)) +
        geom_boxplot(fill = "#3c8dbc") +
        theme_minimal() +
        labs(
          title = paste("Boxplot de", input$columna_outliers),
          x = "", y = input$columna_outliers
        ) +
        theme(plot.title = element_text(hjust = 0.5))
    })

    # Mostrar resumen de outliers
    output$resumen_outliers <- renderPrint({
      cat("Límite inferior:", lower_bound, "\n")
      cat("Límite superior:", upper_bound, "\n")
      cat("Número de outliers:", length(outliers), "\n")
      cat("Porcentaje de outliers:", round(length(outliers) / length(col_data) * 100, 2), "%\n\n")

      # if (length(outliers) > 0) {
      #   cat("Primeros 10 valores outliers:\n")
      #   print(head(outliers, 10))
      # }
    })
  })

  # TRATAMIENTO DE OUTLIERS
  observeEvent(input$btn_tratar_outliers, {
    req(processed_data(), input$metodo_outliers)

    # Obtener columnas seleccionadas y método de tratamiento
    current_data <- processed_data()
    numeric_cols <- names(current_data)[sapply(current_data, is.numeric)]

    # Determinar qué columnas procesar
    cols <- if (input$todas_columnas_outliers) {
      numeric_cols
    } else {
      input$columnas_tratamiento_outliers
    }

    result_msgs <- list()

    # Procesar cada columna seleccionada
    for (col in cols) {
      # Calcular límites para outliers
      col_data <- current_data[[col]]
      q1 <- quantile(col_data, 0.25, na.rm = TRUE)
      q3 <- quantile(col_data, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr

      # Identificar outliers
      outlier_indices <- which(col_data < lower_bound | col_data > upper_bound)
      outlier_count <- length(outlier_indices)

      # Aplicar tratamiento según método seleccionado
      if (input$metodo_outliers == "eliminar") {
        if (outlier_count > 0) {
          if (!exists("indices_to_remove")) {
            indices_to_remove <- outlier_indices
          } else {
            indices_to_remove <- union(indices_to_remove, outlier_indices)
          }
        } else {
          result_msgs[[col]] <- paste("Columna:", col, "- No se encontraron outliers\n")
        }
      } else if (input$metodo_outliers == "winsor") {
        # Winsorización (reemplazar con percentiles 5 y 95)
        p05 <- quantile(current_data[[col]], 0.05, na.rm = TRUE)
        p95 <- quantile(current_data[[col]], 0.95, na.rm = TRUE)
        current_data[[col]][current_data[[col]] < p05] <- p05
        current_data[[col]][current_data[[col]] > p95] <- p95
      } else if (input$metodo_outliers == "limites") {
        # Reemplazar con límites
        current_data[[col]][current_data[[col]] < lower_bound] <- lower_bound
        current_data[[col]][current_data[[col]] > upper_bound] <- upper_bound
      }
    }

    # Si el método es eliminar, eliminar todas las filas con outliers al final
    if (input$metodo_outliers == "eliminar" && exists("indices_to_remove")) {
      current_data <- current_data[-indices_to_remove, ]
    }

    # Actualizar datos procesados
    processed_data(current_data)

    # Mostrar resultado
    output$resultado_outliers <- renderPrint({
      cat("✓ Tratamiento de outliers completado\n\n")
      cat("Método:", switch(input$metodo_outliers,
        "eliminar" = "Eliminación",
        "winsor" = "Winsorización (percentiles 5-95)",
        "limites" = "Reemplazo con límites IQR"
      ))
    })
  })

  # VISUALIZACIONES

  # Paleta de colores minimalista para gráficos
  colores_paleta <- c("#848de1", "#00CC99", "#0fa37e")


  # 1. Gráfico de tipo de hotel
  observeEvent(input$btn_grafico_tipo_hotel, {
    req(processed_data())

    hotel_counts <- processed_data() %>%
      count(hotel) %>%
      mutate(
        porcentaje = n / sum(n) * 100,
        etiqueta = paste0(hotel, "\n", round(porcentaje, 1), "%")
      )

    # Generar gráfico circular
    output$grafico_tipo_hotel <- renderPlot({
      ggplot(hotel_counts, aes(x = "", y = n, fill = hotel)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right",
          legend.title = element_blank()
        ) +
        scale_fill_manual(values = colores_paleta) +
        geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Distribución de Reservas por Tipo de Hotel")
    })
  })

  # 2. Gráfico de demanda en el tiempo
  observeEvent(input$btn_grafico_demanda_tiempo, {
    req(processed_data())

    # Preparar datos
    demanda_data <- processed_data() %>%
      mutate(fecha_llegada = as.Date(paste(arrival_date_year, arrival_date_month, arrival_date_day_of_month, sep = "-"), format = "%Y-%B-%d")) %>%
      mutate(mes_anio = format(fecha_llegada, "%Y-%m"))

    # Filtrar por año si es necesario
    if (input$anio_demanda != "todos") {
      demanda_data <- demanda_data %>% filter(arrival_date_year == input$anio_demanda)
    }

    # Agrupar por mes y contar reservas
    demanda_mensual <- demanda_data %>%
      count(mes_anio) %>%
      arrange(mes_anio)

    # Generar gráfico de línea
    output$grafico_demanda_tiempo <- renderPlot({
      # Crear vector de meses para mostrar solo trimestrales
      todos_meses <- unique(demanda_mensual$mes_anio)
      meses_trimestrales <- todos_meses[seq(1, length(todos_meses), by = 3)]

      ggplot(demanda_mensual, aes(x = mes_anio, y = n, group = 1)) +
        geom_line(color = colores_paleta[2], size = 1.2) +
        geom_point(color = colores_paleta[2], size = 3) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        # Escala personalizada para mostrar solo meses trimestrales
        scale_x_discrete(breaks = meses_trimestrales) +
        labs(
          title = "Evolución de la Demanda de Reservas",
          x = "Trimestre",
          y = "Número de Reservas"
        ) +
        scale_y_continuous(labels = comma)
    })
  })

  # 3. Gráfico de temporadas
  observeEvent(input$btn_grafico_temporadas, {
    req(processed_data())

    # Preparar datos
    temporadas_data <- processed_data()

    # Filtrar por tipo de hotel si es necesario
    if (input$tipo_hotel_temporada != "todos") {
      temporadas_data <- temporadas_data %>% filter(hotel == input$tipo_hotel_temporada)
    }

    # Agrupar por mes y contar reservas
    temporadas_mensual <- temporadas_data %>%
      count(arrival_date_month) %>%
      mutate(mes = factor(arrival_date_month, levels = month.name))

    # Calcular cuartiles para determinar temporadas
    cuartiles <- quantile(temporadas_mensual$n, probs = c(0.33, 0.66))

    temporadas_mensual <- temporadas_mensual %>%
      mutate(
        temporada = case_when(
          n <= cuartiles[1] ~ "Baja",
          n <= cuartiles[2] ~ "Media",
          TRUE ~ "Alta"
        ),
        temporada = factor(temporada, levels = c("Baja", "Media", "Alta"))
      )

    # Generar gráfico de barras con colores por temporada
    output$grafico_temporadas <- renderPlot({
      ggplot(temporadas_mensual, aes(x = mes, y = n, fill = temporada)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right",
          panel.grid.minor = element_blank()
        ) +
        scale_fill_manual(values = c("Baja" = "#00CC99", "Media" = "#0bbd90", "Alta" = "#0fa37e")) +
        labs(
          title = "Temporadas de Reservas por Mes",
          x = "Mes",
          y = "Número de Reservas",
          fill = "Temporada"
        )
    })
  })

  # 4. Gráfico de menor demanda
  observeEvent(input$btn_grafico_menor_demanda, {
    req(processed_data())

    # Preparar datos
    demanda_data <- processed_data()

    # Filtrar por año si es necesario
    if (input$anio_menor_demanda != "todos") {
      demanda_data <- demanda_data %>% filter(arrival_date_year == input$anio_menor_demanda)
    }

    # Agrupar por mes y contar reservas
    demanda_mensual <- demanda_data %>%
      count(arrival_date_month) %>%
      mutate(mes = factor(arrival_date_month, levels = month.name)) %>%
      arrange(n)

    # Destacar los meses con menor demanda (primer cuartil)
    cuartil_inferior <- quantile(demanda_mensual$n, 0.25)
    demanda_mensual <- demanda_mensual %>%
      mutate(destacado = n <= cuartil_inferior)

    # Generar gráfico de barras con meses de menor demanda destacados
    output$grafico_menor_demanda <- renderPlot({
      ggplot(demanda_mensual, aes(x = reorder(mes, n), y = n, fill = destacado)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "none",
          panel.grid.minor = element_blank()
        ) +
        scale_fill_manual(values = c("FALSE" = colores_paleta[1], "TRUE" = colores_paleta[2])) +
        labs(
          title = "Meses con Menor Demanda de Reservas",
          x = "Mes",
          y = "Número de Reservas"
        ) +
        coord_flip()
    })
  })

  # 5. Gráfico de niños y bebés
  observeEvent(input$btn_grafico_ninos, {
    req(processed_data())

    # Preparar datos
    ninos_data <- processed_data() %>%
      mutate(tiene_ninos = children > 0 | babies > 0)

    # Filtrar por tipo de hotel si es necesario
    if (input$tipo_hotel_ninos != "todos") {
      ninos_data <- ninos_data %>% filter(hotel == input$tipo_hotel_ninos)
    }

    # Calcular porcentajes
    ninos_summary <- ninos_data %>%
      count(tiene_ninos) %>%
      mutate(
        porcentaje = n / sum(n) * 100,
        etiqueta = paste0(
          ifelse(tiene_ninos, "Con niños/bebés", "Sin niños/bebés"),
          "\n", round(porcentaje, 1), "%"
        )
      )

    # Generar gráfico de dona
    output$grafico_ninos <- renderPlot({
      ggplot(ninos_summary, aes(x = 2, y = porcentaje, fill = tiene_ninos)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right",
          legend.title = element_blank()
        ) +
        scale_fill_manual(
          values = c("FALSE" = colores_paleta[1], "TRUE" = colores_paleta[2]),
          labels = c("FALSE" = "Sin niños/bebés", "TRUE" = "Con niños/bebés")
        ) +
        geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Reservas con Niños y/o Bebés") +
        xlim(0.5, 2.5) # Para crear efecto de dona
    })
  })

  # 6. Gráfico de estacionamiento
  observeEvent(input$btn_grafico_estacionamiento, {
    req(processed_data())

    # Preparar datos - asumiendo que required_car_parking_spaces indica necesidad de estacionamiento
    parking_data <- processed_data() %>%
      mutate(necesita_estacionamiento = required_car_parking_spaces > 0)

    # Calcular porcentajes por tipo de hotel
    parking_summary <- parking_data %>%
      group_by(hotel, necesita_estacionamiento) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(hotel) %>%
      mutate(porcentaje = count / sum(count) * 100) %>%
      ungroup()

    # Generar gráfico de barras agrupadas
    output$grafico_estacionamiento <- renderPlot({
      ggplot(parking_summary, aes(x = hotel, y = porcentaje, fill = necesita_estacionamiento)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "right",
          panel.grid.minor = element_blank()
        ) +
        scale_fill_manual(
          values = c("FALSE" = colores_paleta[1], "TRUE" = colores_paleta[2]),
          labels = c("FALSE" = "No necesita", "TRUE" = "Necesita")
        ) +
        labs(
          title = "Necesidad de Estacionamiento por Tipo de Hotel",
          x = "Tipo de Hotel",
          y = "Porcentaje de Reservas (%)",
          fill = "Estacionamiento"
        ) +
        geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
          position = position_dodge(width = 0.9),
          vjust = -0.5,
          size = 3.5
        )
    })
  })

  # 7. Gráfico de cancelaciones
  observeEvent(input$btn_grafico_cancelaciones, {
    req(processed_data())

    # Preparar datos
    cancelaciones_data <- processed_data() %>%
      filter(is_canceled == 1)

    # Filtrar por año si es necesario
    if (input$anio_cancelaciones != "todos") {
      cancelaciones_data <- cancelaciones_data %>% filter(arrival_date_year == input$anio_cancelaciones)
    }

    # Agrupar por mes y contar cancelaciones
    cancelaciones_mensual <- cancelaciones_data %>%
      count(arrival_date_month) %>%
      mutate(mes = factor(arrival_date_month, levels = month.name))

    # Calcular el porcentaje de cancelaciones respecto al total de reservas por mes
    total_reservas_mes <- processed_data() %>%
      count(arrival_date_month) %>%
      rename(total = n)

    cancelaciones_porcentaje <- cancelaciones_mensual %>%
      left_join(total_reservas_mes, by = "arrival_date_month") %>%
      mutate(porcentaje = n / total * 100)

    # Generar gráfico de barras con gradiente de color
    output$grafico_cancelaciones <- renderPlot({
      ggplot(cancelaciones_porcentaje, aes(x = mes, y = porcentaje, fill = porcentaje)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.grid.minor = element_blank()
        ) +
        scale_fill_gradient(low = colores_paleta[2], high = colores_paleta[3]) +
        labs(
          title = "Porcentaje de Cancelaciones por Mes",
          x = "Mes",
          y = "Porcentaje de Cancelaciones (%)",
          fill = "Porcentaje"
        ) +
        geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.5, size = 3.5)
    })
  })
}
