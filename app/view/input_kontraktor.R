box::use(
  shiny[...],
  janitor[clean_names],
  dplyr[`%>%`, count, filter],
  shinyjs[useShinyjs, disabled],
  reactable[reactable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class="container-input-kontraktor",
    useShinyjs(),
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
        value = data_filtered()$nama_kontraktor
      ) %>%
        disabled()
    })

    kontr_info  <- reactive({
      data() %>%
        clean_names() %>%
        filter(!is.na(nama_kontraktor)) %>%
        count(nama_kontraktor, name = "Jumlah Kavling",)
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
              selected = data_filtered()$nama_kontraktor
            ),
            reactable(kontr_info())
          )
        )
      )
    })
  })
}