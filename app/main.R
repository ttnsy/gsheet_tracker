box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, reactive, req, reactiveVal],
  shiny.router[router_ui, router_server, route, route_link],
  googlesheets4[...],
  googledrive[drive_auth],
  glue[glue],
  dplyr[...],
  config[get],
  reactable[reactableTheme]
)

box::use(
  app/logic/utils_tracker[read_tracker],
  app/view/spr,
  app/view/tracker
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    title = "Tracker Sample",
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
    router_ui(
      route("/", spr$ui(ns("spr"))),
      route("tracker", tracker$ui(ns("tracker")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("/")

    google_mail <- Sys.getenv("GOOGLE_MAIL")
    google_sheet_url  <- Sys.getenv("GOOGLE_SHEET_URL")

    gs4_auth(cache = ".secrets", email = google_mail)
    drive_auth(cache = ".secrets", email = google_mail)
    sheet_id  <- as_sheets_id(google_sheet_url)

    #' trigger to reload data from gsheet
    session$userData$spr_trigger  <- reactiveVal(0)
    session$userData$pencairan_trigger <- reactiveVal(0)
    session$userData$konstruksi_trigger <- reactiveVal(0)
    session$userData$kontr_progress_trigger  <- reactiveVal(0)
    session$userData$kontraktor_trigger  <- reactiveVal(0)

    #' columns
    data_cols  <- config::get(file = "data_cols.yml")

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
