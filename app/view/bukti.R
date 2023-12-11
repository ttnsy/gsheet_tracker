box::use(
  shiny[...],
  dplyr[...],
  glue[glue]
)

box::use(
  app/logic/tracker[...],
  app/logic/bukti[...],
  app/view/bukti_input
)

#' @export
ui <- function(id, title) {
  ns <- NS(id)
  div(
    class = "bukti",
    div(
      class = "bukti__header",
      uiOutput(ns("bukti_header")),
    ),
    div(
      class = "bukti__items",
      uiOutput(ns("bukti_items"))
    ),
    bukti_input$ui(ns("input"))
  )
}

#' @export
server <- function(id, title, sheet_id, sheet, trigger, data_main, data, cols_rules) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_filtered  <- reactive({
      req(data_main())
      req(data())

      blok_id_selected <- unique(data_main()$blok_id)
      data() %>%
        filter(blok_id == blok_id_selected)
    })

    bukti_items_data <- reactive({
      data_filtered()  %>%
        select(date, link) %>%
        mutate(
          amount = glue("RP. 10,000,000,-")
        )
    })

    output$bukti_header <- renderUI({
      dat <- data_filtered()
      subtitle <- done_to_target_perc(dat)
      tagList(
        h2(class = "bukti__title", title),
        h3(class = "bukti__subtitle", subtitle)
      )
    })
    output$bukti_items <- renderUI({
      req(bukti_items_data())

      dat <- bukti_items_data()

      items <- tagList()
      for (i in seq_len(nrow(dat))) {
        item <- bukti_button_item(
          url = dat$link[i],
          value1 = dat$amount[i],
          value2 = dat$date[i]
        )
        items <- tagAppendChild(items, item)
      }
      items
    })

    bukti_input$server("input", "transfer", data_main, sheet_id, sheet, cols_rules, trigger)
  })
}
