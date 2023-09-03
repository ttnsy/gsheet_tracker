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
  app/view/input_kontraktor,
  app/view/konstruksi,
  app/view/table_summary
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-tracker",
    div(
      class = "tracker-summary",
      table_summary$ui(ns("table_summary")),
    ),
    div(
      class = "tracker-kavling",
      uiOutput(ns("blok_id_ui")),
      pencairan$ui(ns("pencairan")),
      input_kontraktor$ui(ns("input_kontraktor")),
      konstruksi$ui(ns("konstruksi"))
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
    cols_summary  <- data_cols[["summary"]]

    data_main  <- reactive({
      data() %>%
        rename_sheet_cols(data_cols[["spr"]]) %>%
        select(
          nama,
          blok_id,
          sistem_pembayaran
        )
    })

    data_pencairan_raw  <- reactive({
      session$userData$pencairan_trigger()
      read_tracker(
        sheet_id,
        "pencairan",
        cols_rules = cols_pencairan
      )
    })

    data_konstruksi_raw  <- reactive({
      session$userData$konstruksi_trigger()
      read_tracker(
        sheet_id,
        "konstruksi",
        cols_rules = cols_konstruksi
      )
    })

    data_kontraktor_raw  <- read_tracker(
      sheet_id,
      "kontraktor",
      cols_rules = cols_kontraktor
    )

    output$blok_id_ui  <- renderUI({
        selectInput(
        ns("blok_id"),
        "Pilih Blok/Kavling:",
        choices = sort(data()$blok_id)
      )
    })

    data_main_filtered  <- reactive({
      req(input$blok_id)
      data_main() %>%
        filter(blok_id == input$blok_id)
    })

    data_summary <- table_summary$server(
      "table_summary",
      data_main,
      data_pencairan_raw,
      data_konstruksi_raw,
      data_kontraktor_raw,
      cols_rules = cols_summary
    )

    input_kontraktor$server(
      "input_kontraktor",
      blok_id = reactive({
        input$blok_id
      }),
      data = data_summary
    )

    pencairan$server(
      "pencairan",
      sheet_id,
      data_main = data_main_filtered,
      data_pencairan_raw,
      cols_rules = cols_pencairan
    )

    konstruksi$server(
      "konstruksi",
      sheet_id,
      data_main = data_main_filtered,
      data_konstruksi_raw,
      cols_rules = cols_konstruksi
    )
  })
}