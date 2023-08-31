box::use(
  shiny[...],
  janitor[clean_names],
  dplyr[`%>%`, count],
  shinyjs[disabled],
  reactable[reactable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class="container-input-kontraktor",
    uiOutput(ns("kontr_input_ui")),
    actionButton(
      ns("edit_kontraktor"),
      label = "",
      title = "Click to edit",
      icon = icon("user-pen")
    )
  )
}

#' @export
server <- function(id, data, data_filtered) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    output$kontr_input_ui  <- renderUI({
      req(data_filtered())
      textInput(
        ns("kontraktor"),
        label = "Nama Kontraktor:",
        value = data_filtered()$`Nama Kontraktor`
      ) %>%
        disabled()
    })

    kontr_info  <- reactive({
      data %>%
        clean_names() %>%
        count(nama_kontraktor, name = "Jumlah Kavling")
    })

    observeEvent(input$edit_kontraktor, {
      req(kontr_info())
      req(data_filtered())

      showModal(
        modalDialog(
          tagList(
            selectInput(
              ns("kontr_edit"),
              label = "Nama kontraktor:",
              choices = c("Asep", "Buyung"),
              selected = data_filtered()$`Nama Kontraktor`
            ),
            reactable(kontr_info())
          )
        )
      )
    })
  })
}