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
    app/view/pencairan,
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
      pencairan$ui(ns("pencairan")),
      tracker_konstruksi$ui(ns("konstruksi"))
    )
  )
}

#' @export
server <- function(id, sheet_id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_pencairan_raw  <- read_tracker(sheet_id, "pencairan", clean_names = TRUE)
    konstruksi  <- read_tracker(sheet_id, "konstruksi", clean_names = TRUE)
    kontraktor  <- read_tracker(sheet_id, "kontraktor", clean_names = TRUE)

    data_summary  <- get_summary(data_pencairan_raw, konstruksi, kontraktor)

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
        data_pencairan_raw %>%
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

    pencairan$server("pencairan", data = data_pencairan)
    tracker_konstruksi$server("konstruksi", data_konstruksi, kontraktor)
  })
}