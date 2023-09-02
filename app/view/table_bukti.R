box::use(
  dplyr[`%>%`, select],
  reactable[reactable, renderReactable, reactableOutput],
  shiny[...]
)

box::use(
  app/logic/utils_tracker[clean_tracker_cols]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  reactableOutput(ns("tbl"))
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$tbl <- renderReactable({
      req(data())
      data <- clean_tracker_cols(data()) %>%
        select(c("Nama", "Sistem Pembayaran", "Blok/Kavling", everything()))

      reactable(data)
    })
  })
}