box::use(
  shiny[
    div,
    span,
    req,
    reactive,
    NS,
    h3,
    tags,
    tagList,
    moduleServer,
    selectInput,
    uiOutput,
    renderUI
  ],
  reactable[...],
  glue[glue],
  dplyr[...],
  googlesheets4[...],
  janitor[clean_names]
)

box::use(
    app/logic/utils_tracker[...],
    app/logic/tracker_summary[...],
    app/view/input_bukti,
    app/view/input_kontraktor,
    app/view/table_bukti
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-tracker",
    div(
      class = "tracker-summary",
      reactableOutput(ns("summary")),
    ),
    div(
      class = "tracker-kavling",
      uiOutput(ns("blok_id_ui")),
      input_bukti$ui(ns("pencairan")),
      table_bukti$ui(ns("pencairan")),
      input_kontraktor$ui(ns("input_kontraktor")),
      input_bukti$ui(ns("konstruksi")),
      table_bukti$ui(ns("konstruksi"))
    )
  )
}

#' @export
server <- function(id, sheet_id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_pencairan_raw  <- read_tracker(sheet_id, "pencairan", clean_names = TRUE)
    data_konstruksi_raw  <- read_tracker(sheet_id, "konstruksi", clean_names = TRUE)
    data_kontraktor_raw  <- read_tracker(sheet_id, "kontraktor", clean_names = TRUE)

    data_summary  <- reactive({
      req(data())
      summary  <- get_summary(
        data_pencairan_raw,
        data_konstruksi_raw,
        data_kontraktor_raw
      )
      data() %>%
        select(
          Nama,
          `Blok/Kavling` = blok_id,
          `Sistem Pembayaran`
        ) %>%
        left_join(summary)
    })

    output$summary  <- renderReactable({
      reactable(
        data_summary(),
        searchable = TRUE
      )
    })

    output$blok_id_ui  <- renderUI({
        selectInput(
        ns("blok_id"),
        "Pilih Blok/Kavling:",
        choices = sort(data()$blok_id)
      )
    })

    data_pencairan  <- reactive({
        req(input$blok_id)
        data_pencairan_raw %>%
          filter(blok_id == input$blok_id)
    })

    data_konstruksi  <- reactive({
        req(input$blok_id)
        data_konstruksi_raw  %>%
          filter(blok_id == input$blok_id)
    })

    data_summary_filtered  <- reactive({
      req(input$blok_id)
      data_summary() %>%
        filter(`Blok/Kavling` == input$blok_id)
    })

    input_bukti$server("pencairan", "pencairan")
    table_bukti$server("pencairan", data_pencairan)
    input_kontraktor$server("input_kontraktor", data_summary, data_summary_filtered)
    input_bukti$server("konstruksi", "transfer")
    table_bukti$server("konstruksi", data_konstruksi)
  })
}