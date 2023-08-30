box::use(
  shiny[
    div,
    span,
    req,
    reactive,
    NS,
    h3,
    tags,
    tagList,
    moduleServer,
    selectInput,
    uiOutput,
    renderUI
  ],
  reactable[...],
  glue[glue],
  dplyr[...],
  googlesheets4[...],
  janitor[clean_names]
)

box::use(
    app/logic/utils_tracker[...],
    app/logic/tracker_summary[...],
    app/view/tracker_pencairan,
    app/view/tracker_konstruksi
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-tracker",
    div(
      class = "tracker-summary",
      reactableOutput(ns("summary")),
    ),
    div(
      class = "tracker-kavling",
      uiOutput(ns("kavling_ui")),
      tracker_pencairan$ui(ns("pencairan")),
      tracker_konstruksi$ui(ns("konstruksi"))
    )
  )
}

#' @export
server <- function(id, sheet_id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pencairan  <- read_tracker(sheet_id, "pencairan", clean_names = TRUE)
    konstruksi  <- read_tracker(sheet_id, "konstruksi", clean_names = TRUE)
    kontraktor  <- read_tracker(sheet_id, "kontraktor", clean_names = TRUE)

    data_summary  <- get_summary(pencairan, konstruksi, kontraktor)

    output$summary  <- renderReactable({
      reactable(
        data_summary,
        searchable = TRUE
      )
    })

    output$kavling_ui  <- renderUI({
        selectInput(
        ns("kavling"),
        "Pilih Blok/Kavling:",
        choices = sort(data$blok_id)
      )
    })

    data_pencairan  <- reactive({
        req(input$kavling)
        pencairan %>%
          filter(blok_id == input$kavling)
    })

    data_konstruksi  <- reactive({
        req(input$kavling)
        konstruksi  %>%
          filter(blok_id == input$kavling)
    })

    kontraktor  <- reactive({
      req(input$kavling)
      data_summary %>%
        filter(`Blok/Kavling` == input$kavling)
    })

    tracker_pencairan$server("pencairan", data_pencairan)
    tracker_konstruksi$server("konstruksi", data_konstruksi, kontraktor)
  })
}