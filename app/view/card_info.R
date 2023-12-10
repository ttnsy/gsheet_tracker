box::use(
  shiny[
    div,
    moduleServer,
    NS,
    reactive,
    req,
    renderUI,
    uiOutput,
    tagList,
    tagAppendChild
  ]
)

box::use(
  app/logic/card_info[format_info_fields_dat],
  app/logic/utils_card[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class="card",
    uiOutput(ns("info_fields"))
  )
}

#' @export
server <- function(id, data_main_filtered, cols_rules) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    info_fields_data <- reactive({
      req(data_main_filtered())
      data <- data_main_filtered()

      format_info_fields_dat(data, cols_rules)
    })

    output$info_fields <- renderUI({
      req(info_fields_data())
      data <- info_fields_data()
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