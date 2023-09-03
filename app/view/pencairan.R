box::use(
  shiny[...],
  reactable[...],
  dplyr[...]
)

box::use(
  app/logic/utils_tracker[...],
  app/view/input_bukti,
  app/view/table_bukti
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_bukti$ui(ns("input")),
    table_bukti$ui(ns("table"))
  )
}

#' @export
server <- function(id, sheet_id, data_main, data_pencairan_raw, cols_rules) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sheet <- "pencairan"
    trigger <- session$userData$pencairan_trigger

    data_pencairan  <- reactive({
      req(data_main())
      req(data_pencairan_raw())

      blok_id_selected  <- unique(data_main()$blok_id)
      data_pencairan_raw() %>%
        filter(blok_id == blok_id_selected)
    })

    input_bukti$server("input", "pencairan", data_main, sheet_id, sheet, cols_rules, trigger)
    table_bukti$server("table", data_pencairan, cols_rules)
  })
}
