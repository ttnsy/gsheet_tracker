box::use(
  dplyr[`%>%`, select, mutate],
  reactable[colDef, colFormat, reactable, renderReactable, reactableOutput],
  shiny[...],
  rlang[...]
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
        select(date, link) %>%
        mutate(
          link = glue::glue('<a href="{link}" target="_blank">Gdrive Link</a>')
        )
      link  <- dat$link

      dat <- dat %>%
        select(-link) %>%
        cbind(link)

      reactable(
        dat,
        columns = list(
          date = colDef(
            name = "",
            rowHeader = TRUE,
            format = colFormat(datetime = TRUE),
            style = list(fontWeight = 600)
          ),
          link = colDef(
            name = names(cols_rules[cols_rules == "link"]),
            html = TRUE,
            align = "center"
          )
        )
      )
    })
  })
}