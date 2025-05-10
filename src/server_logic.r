server_logic <- function(input, output, session) {
  source("src/load_data.R", local = TRUE)
  library(ggplot2)
  library(dplyr)
  library(tidyr)

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

  output$data_summary <- renderPrint({
    req(data())
    summary(data())
  })

  observe({
    req(processed_data())
    updateSelectInput(session, "columnas_na", 
                     choices = names(processed_data()))
    
    num_cols <- names(processed_data())[sapply(processed_data(), is.numeric)]
    updateSelectInput(session, "columna_outliers", 
                     choices = num_cols)
    
    updateSelectInput(session, "columnas_cero", 
                     choices = num_cols, selected = NULL)
  })

  # IDENTIFICACIÓN DE DATOS FALTANTES
  observeEvent(input$btn_identificar_na, {
    req(processed_data())
    
    na_count <- colSums(is.na(processed_data()))
    na_percent <- round(na_count / nrow(processed_data()) * 100, 2)
    na_summary <- data.frame(
      Variable = names(na_count),
      NA_Count = na_count,
      NA_Percent = na_percent
    )
    
    output$resumen_na <- renderPrint({
      na_summary[na_summary$NA_Count > 0, ]
    })
    
    output$grafico_na <- renderPlot({
      na_vars <- na_summary$Variable[na_summary$NA_Count > 0]
      
      if(length(na_vars) > 0) {
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
    
    if(input$metodo_na == "eliminar") {
      rows_before <- nrow(current_data)
      current_data <- current_data[complete.cases(current_data[, input$columnas_na]), ]
      rows_removed <- rows_before - nrow(current_data)
      result_msg <- paste("Se eliminaron", rows_removed, "filas con datos faltantes.")
      
    } else {
      # Imputación
      for(col in input$columnas_na) {
        if(is.numeric(current_data[[col]])) {
          if(input$metodo_na == "media") {
            col_mean <- mean(current_data[[col]], na.rm = TRUE)
            current_data[[col]][is.na(current_data[[col]])] <- col_mean
            result_msg <- paste("Se imputaron los valores NA con la media de cada columna.")
          } else if(input$metodo_na == "mediana") {
            col_median <- median(current_data[[col]], na.rm = TRUE)
            current_data[[col]][is.na(current_data[[col]])] <- col_median
            result_msg <- paste("Se imputaron los valores NA con la mediana de cada columna.")
          }
        } else {
          if(input$metodo_na == "moda") {
            tab <- table(current_data[[col]], useNA = "no")
            col_mode <- names(tab)[which.max(tab)]
            current_data[[col]][is.na(current_data[[col]])] <- col_mode
            result_msg <- paste("Se imputaron los valores NA con la moda de cada columna.")
          } else {
            tab <- table(current_data[[col]], useNA = "no")
            col_mode <- names(tab)[which.max(tab)]
            current_data[[col]][is.na(current_data[[col]])] <- col_mode
            result_msg <- paste("Se imputaron los valores NA con el valor más frecuente.")
          }
        }
      }
    }
    
    # Actualizar datos procesados
    processed_data(current_data)
    
    # Mostrar resultado
    output$resultado_na <- renderPrint({
      cat(result_msg, "\n\n")
      cat("Dimensiones actuales del conjunto de datos: ", 
          nrow(current_data), "filas x", ncol(current_data), "columnas\n")
      cat("Resumen de NA restantes:\n")
      print(colSums(is.na(current_data)))
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
        labs(title = paste("Boxplot de", input$columna_outliers),
             x = "", y = input$columna_outliers) +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Mostrar resumen de outliers
    output$resumen_outliers <- renderPrint({
      cat("Resumen de outliers para la variable", input$columna_outliers, ":\n\n")
      cat("Límite inferior:", lower_bound, "\n")
      cat("Límite superior:", upper_bound, "\n\n")
      cat("Número de outliers:", length(outliers), "\n")
      cat("Porcentaje de outliers:", round(length(outliers)/length(col_data)*100, 2), "%\n\n")
      
      if(length(outliers) > 0) {
        cat("Primeros 10 valores outliers:\n")
        print(head(outliers, 10))
      }
    })
  })
  
  # TRATAMIENTO DE VALORES CERO
  observeEvent(input$btn_reemplazar_ceros, {
    req(processed_data(), input$columnas_cero)
    
    # Obtener datos actuales
    current_data <- processed_data()
    
    # Contador para seguimiento
    total_replaced <- 0
    
    # Reemplazar ceros con NA en las columnas seleccionadas
    for(col in input$columnas_cero) {
      # Verificar si la columna es numérica
      if(is.numeric(current_data[[col]])) {
        # Contar ceros antes de reemplazar
        zeros_count <- sum(current_data[[col]] == 0, na.rm = TRUE)
        
        # Reemplazar ceros con NA
        current_data[[col]][current_data[[col]] == 0] <- NA
        
        # Actualizar contador
        total_replaced <- total_replaced + zeros_count
      }
    }
    
    # Actualizar datos procesados
    processed_data(current_data)
    
    # Mostrar resultado
    output$resultado_ceros <- renderPrint({
      cat("Se reemplazaron", total_replaced, "valores 0 con NA en las columnas seleccionadas.\n\n")
      cat("Dimensiones actuales del conjunto de datos: ", 
          dim(processed_data())[1], "filas x", dim(processed_data())[2], "columnas\n")
    })
  })
  
  # TRATAMIENTO DE OUTLIERS
  observeEvent(input$btn_tratar_outliers, {
    req(processed_data(), input$columna_outliers, input$metodo_outliers)
    
    # Obtener datos actuales
    current_data <- processed_data()
    col <- input$columna_outliers
    
    # Calcular límites para outliers
    q1 <- quantile(current_data[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(current_data[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    # Identificar índices de outliers
    outlier_indices <- which(current_data[[col]] < lower_bound | current_data[[col]] > upper_bound)
    outlier_count <- length(outlier_indices)
    
    # Aplicar tratamiento según el método seleccionado
    if(input$metodo_outliers == "eliminar") {
      # Eliminar filas con outliers
      if(outlier_count > 0) {
        rows_before <- nrow(current_data)
        current_data <- current_data[-outlier_indices, ]
        result_msg <- paste("Se eliminaron", outlier_count, "filas con outliers.")
      } else {
        result_msg <- "No se encontraron outliers para eliminar."
      }
    } else if(input$metodo_outliers == "winsor") {
      # Winsorización (reemplazar con percentiles 5 y 95)
      p05 <- quantile(current_data[[col]], 0.05, na.rm = TRUE)
      p95 <- quantile(current_data[[col]], 0.95, na.rm = TRUE)
      current_data[[col]][current_data[[col]] < p05] <- p05
      current_data[[col]][current_data[[col]] > p95] <- p95
      result_msg <- paste("Se aplicó winsorización (percentiles 5-95) a", outlier_count, "valores.")
    } else if(input$metodo_outliers == "limites") {
      # Reemplazar con límites
      current_data[[col]][current_data[[col]] < lower_bound] <- lower_bound
      current_data[[col]][current_data[[col]] > upper_bound] <- upper_bound
      result_msg <- paste("Se reemplazaron", outlier_count, "outliers con los límites IQR.")
    }
    
    # Actualizar datos procesados
    processed_data(current_data)
    
    # Mostrar resultado
    output$resultado_outliers <- renderPrint({
      cat(result_msg, "\n\n")
      cat("Dimensiones actuales del conjunto de datos: ", 
          nrow(current_data), "filas x", ncol(current_data), "columnas\n")
      
      # Verificar si quedan outliers
      col_data <- current_data[[col]]
      q1_new <- quantile(col_data, 0.25, na.rm = TRUE)
      q3_new <- quantile(col_data, 0.75, na.rm = TRUE)
      iqr_new <- q3_new - q1_new
      lower_new <- q1_new - 1.5 * iqr_new
      upper_new <- q3_new + 1.5 * iqr_new
      
      outliers_new <- sum(col_data < lower_new | col_data > upper_new, na.rm = TRUE)
      cat("Outliers restantes:", outliers_new, "\n")
    })
  })
}
