box::use(
  shiny[...],
  reactable[...],
  dplyr[...]
)

box::use(
  app/logic/utils_tracker[...],
  app/view/bukti_input,
  app/view/bukti_table
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-bukti",
    bukti_input$ui(ns("input")),
    bukti_table$ui(ns("table"))
  )
}

#' @export
server <- function(id, sheet_id, sheet, trigger, data_main, data, cols_rules) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_filtered  <- reactive({
      req(data_main())
      req(data())

      blok_id_selected <- unique(data_main()$blok_id)
      data() %>%
        filter(blok_id == blok_id_selected)
    })

    bukti_input$server("input", "transfer", data_main, sheet_id, sheet, cols_rules, trigger)
    bukti_table$server("table", data_filtered, cols_rules)
  })
}