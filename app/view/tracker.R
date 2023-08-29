box::use(
  shiny[req, reactive, NS, h3, tags, tagList, moduleServer, selectInput, uiOutput, renderUI],
  reactable[...],
  glue[glue],
  dplyr[...],
  googlesheets4[...],
  janitor[clean_names]
)

box::use(
    app/logic/tracker_summary[...],
    app/view/tracker_pencairan
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Summary"),
    reactableOutput(ns("summary")),
    h3("Tracker"),
    tags$hr(),
    uiOutput(ns("input_kavling")),
    tags$h4("Pencairan"),
    tracker_pencairan$ui(ns("pencairan"))
  )
}

#' @export
server <- function(id, sheet_id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pencairan  <- read_sheet(sheet_id, "pencairan")
    konstruksi  <- read_sheet(sheet_id, "konstruksi")
    kontraktor  <- read_sheet(sheet_id, "kontraktor") 

    output$summary  <- renderReactable({
      summary_data  <- get_summary(pencairan, konstruksi, kontraktor)
      reactable(summary_data)
    })

    output$input_kavling  <- renderUI({
        selectInput(
        ns("kavling"),
        "Pilih Blok/Kavling:",
        choices = sort(data$blok_id)
      )
    })

    data_pencairan  <- reactive({
        req(input$kavling)
        blok  <- strsplit(input$kavling, "[/]")[[1]][1]
        kavling  <- strsplit(input$kavling, "[/]")[[1]][2]

        clean_names(pencairan) %>%
            filter(blok == blok & nomor_kavling == kavling)
    })

    tracker_pencairan$server("pencairan", data_pencairan)
  })
}