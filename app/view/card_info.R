box::use(
  shiny[...]
)

box::use(
  app/logic/card[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class="card",
    div(
      class = "card__content",
      uiOutput(ns("info_fields"))
    )
  )
}

#' @export
server <- function(id, data_main_filtered) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    output$info_fields <- renderUI({
      req(data_main_filtered())
      data <- data_main_filtered()
      labels <- colnames(data)

      texts <- tagList()
      for (i in 1:ncol(data)) {
        text <- generate_info_field(
          label = labels[i],
          value = data[i]
        )
        texts <- tagAppendChild(texts, text)
      }

      texts
    })
  })
}