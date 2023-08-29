box::use(
  shiny[navbarPage, tabPanel, moduleServer, NS, renderText, tags, textOutput],
  googlesheets4[...],
  glue[glue],
  dplyr[...]
)

box::use(
  app/logic/utils_tracker[read_tracker],
  app/view/tbl_spr,
  app/view/tracker
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  navbarPage(
    title = "Tracker Sample",
    tabPanel(
      "Tabel SPR",
      tbl_spr$ui(ns("tbl_spr"))
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

    spr <- read_tracker(sheet_id, sheet_name = "spr")

    tbl_spr$server("tbl_spr", data = spr)
    tracker$server("tracker", sheet_id = sheet_id, data = filter(spr, Status == "Process"))
  })
}
