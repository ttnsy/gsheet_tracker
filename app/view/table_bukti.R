box::use(
  dplyr[`%>%`, select],
  reactable[reactable, renderReactable, reactableOutput],
  shiny[...]
)

box::use(
  app/logic/utils_tracker[rename_sheet_cols]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  reactableOutput(ns("tbl"))
}

#' @export
server <- function(id, data, cols_rules) {
  moduleServer(id, function(input, output, session) {

    output$tbl <- renderReactable({
      req(data())
      dat <- data() %>%
        select(-blok_id)
      data <- rename_sheet_cols(dat, cols_rules, revert=TRUE, rearrange=TRUE)

      reactable(data)
    })
  })
}