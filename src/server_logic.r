server_logic <- function(input, output, session) {
  source("src/load_data.R", local = TRUE)

  data <- reactive({
    req(input$file1)
    df <- load_data(input$file1$datapath)
    df
  })

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

  output$data_structure <- renderPrint({
    req(data())
    str(data())
  })

  output$data_summary <- renderPrint({
    req(data())
    summary(data())
  })
}
