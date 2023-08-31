box::use(
  shiny[navbarPage, tabPanel, moduleServer, NS, renderText, tags, reactive, req, reactiveVal],
  googlesheets4[...],
  glue[glue],
  dplyr[...]
)

box::use(
  app/logic/utils_tracker[read_tracker],
  app/view/spr,
  app/view/tracker
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  navbarPage(
    title = "Tracker Sample",
    tabPanel(
      "Tabel SPR",
      spr$ui(ns("spr"))
    ),
    tabPanel(
      "Tracker",
      tracker$ui(ns("tracker"))
    ),
    tabPanel(
      "Kontraktor"
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    gs4_auth(cache = ".secrets", email = "tanesya.t@gmail.com")
    url  <- "https://docs.google.com/spreadsheets/d/1iCKfGD1QAmdBChqlfp5-WnN8rms4hmAPmdjUXLe859w/edit?usp=sharing" # nolint: line_length_linter.
    sheet_id  <- as_sheets_id(url)

    #' trigger to reload spr data from gsheet
    session$userData$spr_trigger  <- reactiveVal(0)

    spr_data <- reactive({
      session$userData$spr_trigger()
      out <- NULL

      tryCatch({
        out <- read_tracker(sheet_id, sheet_name = "spr")
      }, error = function(e) {
        print(e)
        showToast("error", glue("error reading SPR sheet: {e}"))
      })
      out
    })

    spr_data_process  <- reactive({
      req(spr_data())
      spr_data() %>%
        filter(Status == "Process")
    })

    spr$server("spr", sheet_id, spr_data)
    tracker$server("tracker", sheet_id, data = spr_data_process)
  })
}
