box::use(
  shiny[...],
  shinyFeedback[...],
  googlesheets4[sheet_append],
  janitor[clean_names],
  dplyr[`%>%`, count, filter, tibble],
  shinyjs[useShinyjs, disabled],
  reactable[reactable]
)

box::use(
  app/logic/utils_tracker[rename_sheet_cols]
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
server <- function(id, blok_id, cols_rules, data, sheet_id) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    data_filtered  <- reactive({
      req(blok_id())
      data() %>%
        filter(blok_id == blok_id())
    })

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
              selected = data_filtered()$nama_kontraktor
            ),
            reactable(kontr_info())
          ),
          footer = list(
            modalButton("Cancel"),
            actionButton(
              ns("submit"),
              "Submit",
              class = "btn btn-primary",
              style = "color: white"
            )
          )
        )
      )
    })

    out <- eventReactive(input$submit, {
      req(blok_id())
      req(input$kontr_edit)

      tibble(
        nama_kontraktor = input$kontr_edit,
        blok = strsplit(blok_id(), "[/]")[[1]][1],
        no_kavling = strsplit(blok_id(), "[/]")[[1]][2],
      ) %>%
      rename_sheet_cols(cols_rules, revert=TRUE, rearrange=TRUE)
    })

    observeEvent(out(), {
      req(out())
      dat <- out()
      sheet_append(sheet_id, dat, sheet = "kontraktor")
      session$userData$kontraktor_trigger(session$userData$kontraktor_trigger()+1)
      removeModal()
    })
  })
}