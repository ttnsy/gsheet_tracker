box::use(
  shiny[navbarPage, tabPanel, moduleServer, NS, renderText, tags, textOutput],
  googlesheets4[gs4_deauth, read_sheet]
)

box::use(
  app/view/tbl_spr
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
      "Tracker"
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
    spr  <- read_sheet("https://docs.google.com/spreadsheets/d/1iCKfGD1QAmdBChqlfp5-WnN8rms4hmAPmdjUXLe859w/edit?usp=sharing")

    tbl_spr$server("tbl_spr", data = spr)

  })
}
