box::use(
  shiny[navbarPage, tabPanel, moduleServer, NS, renderText, tags, textOutput],
  googlesheets4[...],
  glue[glue],
  dplyr[...]
)

box::use(
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
    gs4_deauth()
    sheet_id  <- as_sheets_id("https://docs.google.com/spreadsheets/d/1iCKfGD1QAmdBChqlfp5-WnN8rms4hmAPmdjUXLe859w/edit?usp=sharing")
    spr  <- read_sheet(sheet_id, "spr")
    spr  <-  spr %>%
     mutate(blok_id = glue("{Blok}/{`Nomor Kavling`}"))

    tbl_spr$server("tbl_spr", data = spr)
    tracker$server("tracker", sheet_id = sheet_id, data = filter(spr, Status == "Process"))
  })
}
