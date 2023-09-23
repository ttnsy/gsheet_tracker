box::use(
  shiny[...],
  reactable[...],
  glue[glue],
  dplyr[...],
  googlesheets4[...],
  janitor[clean_names]
)

box::use(
  app/logic/utils_tracker[...],
  app/view/bukti,
  app/view/input_kontraktor
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "container-tracker",
      div(
        class = "sidebar-tracker",
        uiOutput(ns("blok_id_ui")),
        uiOutput(ns("info"))
      ),
      div(
        class = "tracker-contents",
        fluidRow(
          bukti$ui(ns("pencairan"), title = "Pencairan Bank")
        ),
        fluidRow(
          input_kontraktor$ui(ns("input_kontraktor")),
          uiOutput(ns("bukti_ui_konstruksi"))
        )
      )
    )
  )
}

#' @export
server <- function(id, sheet_id, data, data_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$info  <- renderUI({
      req(data_main_filtered())

      dat <- data_main_filtered()
      tagList(
        icon("home-user", "fa-3x"),
        div(
          class = "info-contents",
          p(class = "title", glue("{dat$nama}")),
          p(class = "subtitle", glue("{dat$blok_id}")),
          p(class = "text", "Sistem Pembayaran:", glue("{dat$sistem_pembayaran}"))
        )
      )
    })

    cols_pencairan  <- data_cols[["pencairan"]]
    cols_konstruksi  <- data_cols[["konstruksi"]]
    cols_kontraktor  <- data_cols[["kontraktor"]]

    data_main  <- reactive({
      data() %>%
        select(
          nama,
          blok_id,
          sistem_pembayaran
        )
    })

    data_pencairan_raw  <- reactive({
      session$userData$pencairan_trigger()
      read_tracker(
        sheet_id,
        "pencairan",
        cols_rules = cols_pencairan
      )
    })

    data_konstruksi_raw  <- reactive({
      session$userData$konstruksi_trigger()
      read_tracker(
        sheet_id,
        "konstruksi",
        cols_rules = cols_konstruksi
      )
    })

    data_kontraktor_raw  <- reactive({
      session$userData$kontraktor_trigger()
      read_tracker(
        sheet_id,
        "kontraktor",
        cols_rules = cols_kontraktor
      )
    })

    output$blok_id_ui  <- renderUI({
      selectInput(
        ns("blok_id"),
        "Pilih Blok/Kavling:",
        choices = sort(data()$blok_id)
      )
    })

    data_main_filtered  <- reactive({
      req(input$blok_id)
      data_main() %>%
        filter(blok_id == input$blok_id)
    })

    bukti$server(
      "pencairan",
      sheet_id,
      sheet = "pencairan",
      trigger = session$userData$pencairan_trigger,
      data_main = data_main_filtered,
      data = data_pencairan_raw,
      cols_rules = cols_pencairan
    )

    input_kontraktor_val <- input_kontraktor$server(
      "input_kontraktor",
      blok_id = reactive({
        input$blok_id
      }),
      cols_rules = cols_kontraktor,
      data = data_kontraktor_raw,
      sheet_id
    )

    observeEvent(input_kontraktor_val(), {
      output$bukti_ui_konstruksi  <- renderUI({
        if (input_kontraktor_val() == "") {
            div(
              class = "bukti_ui_message",
              icon("table", "fa-5x"),
              h5(
                "Input nama kontraktor untuk memulai."
              )
            )
        } else {
          bukti$ui(ns("konstruksi"), title = "Pembayaran Kontraktor")
        }
      })
    })

    bukti$server(
      "konstruksi",
      sheet_id,
      sheet = "konstruksi",
      trigger = session$userData$konstruksi_trigger,
      data_main = data_main_filtered,
      data = data_konstruksi_raw,
      cols_rules = cols_konstruksi
    )
  })
}