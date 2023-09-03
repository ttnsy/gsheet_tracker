box::use(
  shiny[...],
  reactable[...],
  dplyr[...]
)

box::use(
  app/logic/tracker_summary[...],
  app/logic/utils_tracker[rename_sheet_cols]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactableOutput(ns("tbl"))
}

#' @export
server <- function(id, data_main, data_pencairan, data_konstruksi, data_kontraktor, cols_rules) {
  moduleServer(id, function(input, output, session) {
    data_summary  <- reactive({
      req(data_main())
      pencairan  <- data_pencairan()
      konstruksi <- data_konstruksi()
      kontraktor  <- data_kontraktor()

      summary  <- get_summary(
        pencairan,
        konstruksi,
        kontraktor
      )
      data_main() %>%
        left_join(summary)
    })

    data_summary_cleaned <- reactive({
      dat <- data_summary() %>%
        select(-c("blok", "no_kavling"))
      rename_sheet_cols(dat, cols_rules, revert = TRUE)
    })

    output$tbl  <- renderReactable({
      reactable(
        data_summary_cleaned(),
        searchable = TRUE
      )
    })

    return(data_summary)
  })
}