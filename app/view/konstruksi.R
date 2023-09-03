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
server <- function(id, sheet_id, data_main, data_konstruksi_raw, cols_rules) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sheet <- "konstruksi"
    trigger <- session$userData$konstruksi_trigger

    data_konstruksi  <- reactive({
      req(data_main())
      req(data_konstruksi_raw())

      blok_id_selected  <- unique(data_main()$blok_id)
      data_konstruksi_raw() %>%
        filter(blok_id == blok_id_selected)
    })

    input_bukti$server("input", "konstruksi", data_main, sheet_id, sheet, cols_rules, trigger)
    table_bukti$server("table", data_konstruksi, cols_rules)
  })
}
