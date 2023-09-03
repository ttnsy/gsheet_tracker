box::use(
  shiny[...],
  reactable[...],
  glue[glue],
  dplyr[...],
  googlesheets4[...],
  janitor[clean_names]
)

box::use(
  app/logic/utils_tracker[...],
  app/logic/tracker_summary[...],
  app/view/pencairan,
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
      pencairan$ui(ns("pencairan")),
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
    data_cols  <- config::get(file = "data_cols.yml")
    cols_pencairan  <- data_cols[["pencairan"]]
    cols_konstruksi  <- data_cols[["konstruksi"]]
    cols_kontraktor  <- data_cols[["kontraktor"]]

    data_pencairan_raw  <- reactive({
      session$userData$pencairan_trigger()
      read_tracker(
        sheet_id,
        "pencairan",
        cols_rules = cols_pencairan
      )
    })

    data_konstruksi_raw  <- read_tracker(
      sheet_id,
      "konstruksi",
      cols_rules = cols_konstruksi
    )

    data_kontraktor_raw  <- read_tracker(
      sheet_id,
      "kontraktor",
      cols_rules = cols_kontraktor
    )

    data_main  <- reactive({
      data() %>%
        rename_sheet_cols(data_cols[["spr"]]) %>%
        select(
          nama,
          blok_id,
          sistem_pembayaran
        )
    })

    data_summary  <- reactive({
      req(data())
      data_pencairan  <- data_pencairan_raw()
      summary  <- get_summary(
        data_pencairan,
        data_konstruksi_raw,
        data_kontraktor_raw
      )
      data_main() %>%
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

    data_konstruksi  <- reactive({
        req(input$blok_id)
        data_konstruksi_raw  %>%
          filter(blok_id == input$blok_id)
    })

    data_main_filtered  <- reactive({
      req(input$blok_id)
      data_main() %>%
        filter(blok_id == input$blok_id)
    })

    data_summary_filtered  <- reactive({
      req(input$blok_id)
      data_summary() %>%
        filter(blok_id == input$blok_id)
    })

    input_kontraktor$server("input_kontraktor", data_summary, data_summary_filtered)

    pencairan$server(
      "pencairan",
      sheet_id,
      data_main = data_main_filtered,
      data_pencairan_raw = data_pencairan_raw,
      cols_rules = cols_pencairan
    )

    input_bukti$server("konstruksi", "transfer")
    table_bukti$server("konstruksi", data_konstruksi)
  })
}