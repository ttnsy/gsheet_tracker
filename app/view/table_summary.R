box::use(
  shiny[...],
  reactable[...],
  dplyr[...]
)

box::use(
  app/logic/tracker_summary[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("tbl"))
}

#' @export
server <- function(id, data_main, data_pencairan, data_konstruksi, data_kontraktor) {
  moduleServer(id, function(input, output, session) {
    data_summary  <- reactive({
      req(data_main())
      pencairan  <- data_pencairan()
      konstruksi <- data_konstruksi()

      summary  <- get_summary(
        pencairan,
        konstruksi,
        data_kontraktor
      )
      data_main() %>%
        left_join(summary)
    })

    output$tbl  <- renderReactable({
      reactable(
        data_summary(),
        searchable = TRUE
      )
    })

    return(data_summary)
  })
}