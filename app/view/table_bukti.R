box::use(
  dplyr[`%>%`, select, mutate],
  reactable[colDef, reactable, renderReactable, reactableOutput],
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
        select(-c("blok", "no_kavling")) %>%
        mutate(
          link = glue::glue('<a href="{link}" target="_blank">Gdrive Link</a>')
        )
      link  <- dat$link

      dat <- dat %>%
        select(-link) %>%
        rename_sheet_cols(cols_rules, revert=TRUE, rearrange=TRUE) %>%
        cbind(link)

      reactable(
        dat,
        columns = list(
          link = colDef(
            name = names(cols_rules[cols_rules == "link"]),
            html = TRUE
          )
        )
      )
    })
  })
}