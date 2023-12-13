box::use(
  shiny[...],
  shinyFeedback[...],
  googlesheets4[range_write, sheet_append],
  janitor[clean_names],
  dplyr[`%>%`, count, filter, tibble],
  shinyjs[useShinyjs, disabled],
  reactable[reactable],
  glue[glue]
)

box::use(
  app/logic/tracker[rename_sheet_cols]
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

    kontr_info  <- reactive({
      data() %>%
        count(nama_kontraktor, name = "Jumlah Kavling")
    })

    output$kontr_input_ui  <- renderUI({
      req(data_filtered())
      textInput(
        ns("kontraktor"),
        label = "Kontraktor",
        value = data_filtered()$nama_kontraktor
      ) %>%
        disabled()
    })

    observeEvent(input$edit_kontraktor, {
      req(kontr_info())
      req(data_filtered())

      showModal(
        modalDialog(
          tagList(
            selectInput(
              ns("kontr_edit"),
              label = "Pilih Kontraktor:",
              choices = c("KONTRAKTOR A", "KONTRAKTOR B"),
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

      if (blok_id() %in% data()$blok_id) {
        existing  <- data()
        index  <- which(existing$blok_id == blok_id())
        range <- glue("A{index+1}")
        list(
          range = range,
          data = data.frame(nama_kontraktor = input$kontr_edit)
        )
      } else {
        tibble(
          nama_kontraktor = input$kontr_edit,
          blok = strsplit(blok_id(), "[/]")[[1]][1],
          no_kavling = as.integer(strsplit(blok_id(), "[/]")[[1]][2]),
        ) %>%
        rename_sheet_cols(cols_rules, revert=TRUE, rearrange=TRUE)
      }
    })

    observeEvent(out(), {
      req(out())
      dat <- out()
      sheet <- "kontraktor"

      if (blok_id() %in% data()$blok_id) {
        data <- out()$data
        range <- out()$range

        range_write(
          sheet_id,
          data,
          sheet,
          range,
          col_names = FALSE
        )
      } else {
        sheet_append(sheet_id, dat, sheet = sheet)
      }
      session$userData$kontraktor_trigger(session$userData$kontraktor_trigger()+1)
      removeModal()
    })

    return(
      reactive({
        input$kontraktor
      })
    )
  })
}
