box::use(
  reactable[...],
  dplyr[`%>%`, select, everything],
  shiny[...]
)

box::use(
  app/logic/utils_tracker[clean_tracker_cols],
  app/view/input_bukti
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    input_bukti$ui(ns("konstruksi")),
    reactableOutput(ns("tbl"))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    input_bukti$server("konstruksi", "transfer")

    output$tbl <- renderReactable({
      req(data())
      data_tbl <- clean_tracker_cols(data()) %>%
        select(c("Nama", "Sistem Pembayaran", "Blok/Kavling", everything()))
      reactable(data_tbl)
    })
  })
}