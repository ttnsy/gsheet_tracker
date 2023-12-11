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
  app/logic/info[format_info_data, generate_info]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class="card",
    uiOutput(ns("info"))
  )
}

#' @export
server <- function(id, data_main_filtered, cols_rules) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    info_data <- reactive({
      req(data_main_filtered())
      data <- data_main_filtered()

      format_info_data(data, cols_rules)
    })

    output$info <- renderUI({
      req(info_data())
      data <- info_data()
      labels <- colnames(data)

      texts <- tagList()
      for (i in seq_len(ncol(data))) {
        text <- generate_info(
          label = labels[i],
          value = data[i]
        )
        texts <- tagAppendChild(texts, text)
      }

      texts
    })
  })
}