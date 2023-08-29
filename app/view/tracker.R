box::use(
  shiny[div,req, reactive, NS, h3, tags, tagList, moduleServer, selectInput, uiOutput, renderUI],
  reactable[...],
  glue[glue],
  dplyr[...],
  googlesheets4[...],
  janitor[clean_names]
)

box::use(
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
        uiOutput(ns("input_kavling")),
        tracker_pencairan$ui(ns("pencairan")),
        tracker_konstruksi$ui(ns("konstruksi"))
    )
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
      reactable(
        summary_data,
        searchable = TRUE
      )
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

    data_konstruksi  <- reactive({
        req(input$kavling)
        blok  <- strsplit(input$kavling, "[/]")[[1]][1]
        kavling  <- strsplit(input$kavling, "[/]")[[1]][2]

        clean_names(konstruksi)  %>%
            filter(blok == blok & nomor_kavling == kavling)
    })

    tracker_pencairan$server("pencairan", data_pencairan)
    tracker_konstruksi$server("konstruksi", data_konstruksi)
  })
}