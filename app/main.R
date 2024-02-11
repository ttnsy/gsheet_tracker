box::use(
  shiny[fluidPage, moduleServer, NS, renderText, tags, reactive, req, reactiveVal, tagList],
  shiny.router[router_ui, router_server, route, route_link],
  googlesheets4[...],
  googledrive[drive_auth],
  glue[glue],
  dplyr[...],
  config[get],
  reactable[reactableTheme]
)

box::use(
  app/config[...],
  app/logic/main[read_spr_data],
  app/logic/auth[auth_google],
  app/view/spr,
  app/view/tracker
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$nav(
    class = "navbar",
    tags$ul(
      tags$li(
        tags$a(class = "nav-link", "Tabel SPR", href = route_link("/"))
      ),
      tags$li(
        tags$a(class = "nav-link", "Tracker", href = route_link("tracker"))
      )
    )
  ),
  fluidPage(
    router_ui(
      route("/", spr$ui(ns("spr"))),
      route("tracker", tracker$ui(ns("tracker")))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")

    auth_google()
    sheet_id  <- as_sheets_id(google_sheet_url)

    #' trigger to reload data from gsheet
    session$userData$spr_trigger  <- reactiveVal(0)
    session$userData$pencairan_trigger <- reactiveVal(0)
    session$userData$konstruksi_trigger <- reactiveVal(0)
    session$userData$kontr_progress_trigger  <- reactiveVal(0)
    session$userData$kontraktor_trigger  <- reactiveVal(0)

    spr_data <- reactive({
      session$userData$spr_trigger()
      read_spr_data(
        sheet_id,
        sheet_name = sheet_name_spr,
        cols_rules = cols_spr
      )
    })

    spr_data_process  <- reactive({
      req(spr_data())
      spr_data() %>%
        filter(status == "Process")
    })

    spr$server("spr", sheet_id, spr_data)
    tracker$server("tracker", sheet_id, data = spr_data_process)
  })
}
