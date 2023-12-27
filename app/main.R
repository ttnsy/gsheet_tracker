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
  app/logic/tracker[read_tracker],
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

    gs4_auth(path = google_service_acc_path)
    drive_auth(path = google_service_acc_path)
    sheet_id  <- as_sheets_id(google_sheet_url)

    #' trigger to reload data from gsheet
    session$userData$spr_trigger  <- reactiveVal(0)
    session$userData$pencairan_trigger <- reactiveVal(0)
    session$userData$konstruksi_trigger <- reactiveVal(0)
    session$userData$kontr_progress_trigger  <- reactiveVal(0)
    session$userData$kontraktor_trigger  <- reactiveVal(0)

    spr_data <- reactive({
      session$userData$spr_trigger()

      read_tracker(
        sheet_id,
        sheet_name = "spr",
        cols_rules = data_cols[["spr"]]
      ) %>%
        group_by(blok_id) %>%
        slice(which.max(timestamp)) %>%
        ungroup()
    })

    spr_data_process  <- reactive({
      req(spr_data())
      spr_data() %>%
        filter(status == "Process")
    })

    spr$server("spr", sheet_id, spr_data, cols_rules = data_cols[["spr"]])
    tracker$server("tracker", sheet_id, data = spr_data_process, data_cols)
  })
}
