box::use(
  shiny[navbarPage, tabPanel, moduleServer, NS, renderText, tags, reactive, req, reactiveVal],
  googlesheets4[...],
  googledrive[drive_auth],
  glue[glue],
  dplyr[...],
  config[get]
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
    google_mail <- Sys.getenv("GOOGLE_MAIL")
    google_sheet_url  <- Sys.getenv("GOOGLE_SHEET_URL")

    gs4_auth(cache = ".secrets", email = google_mail)
    drive_auth(cache = ".secrets", email = google_mail)
    sheet_id  <- as_sheets_id(google_sheet_url)

    #' trigger to reload data from gsheet
    session$userData$spr_trigger  <- reactiveVal(0)
    session$userData$pencairan_trigger <- reactiveVal(0)
    session$userData$konstruksi_trigger <- reactiveVal(0)
    session$userData$kontraktor_trigger  <- reactiveVal(0)

    spr_data <- reactive({
      session$userData$spr_trigger()
      read_tracker(sheet_id, sheet_name = "spr")
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
