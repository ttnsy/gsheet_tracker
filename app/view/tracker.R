box::use(
  shiny[...],
  reactable[...],
  glue[glue],
  dplyr[...],
  googlesheets4[...],
  janitor[clean_names]
)

box::use(
  app/config[...],
  app/logic/tracker[...],
  app/view/bukti,
  app/view/info,
  app/view/input_kontraktor
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("blok_id_ui")),
    div(
      class = "container-tracker",
      info$ui(ns("info")),
      div(
        class = "card",
        bukti$ui(ns("pencairan"))
      ),
      div(
        class = "card",
        div(
          class = "card__header",
          h3(class = "card__title", "Pembayaran Kontraktor"),
          input_kontraktor$ui(ns("input_kontraktor")),
        ),
        uiOutput(ns("bukti_ui_konstruksi"))
      )
    )
  )
}

#' @export
server <- function(id, sheet_id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_main  <- reactive({
      data() %>%
        select(
          nama,
          blok_id,
          sistem_pembayaran,
          tipe_dan_lt,
          harga_tanah_bangunan,
          disc
        )
    })

    data_pencairan_raw  <- reactive({
      session$userData$pencairan_trigger()
      read_tracker(
        sheet_id,
        sheet_name_pencairan,
        cols_rules = cols_pencairan
      )
    })

    data_konstruksi_raw  <- reactive({
      session$userData$konstruksi_trigger()
      read_tracker(
        sheet_id,
        sheet_name_konstruksi,
        cols_rules = cols_konstruksi
      )
    })

    data_kontr_progress_raw  <- reactive({
      session$userData$kontr_progress_trigger()
      read_tracker(
        sheet_id,
        sheet_name_kontr_progress,
        cols_rules = cols_kontr_progress
      )
    })

    data_kontraktor_raw  <- reactive({
      session$userData$kontraktor_trigger()
      read_tracker(
        sheet_id,
        sheet_name_kontraktor,
        cols_rules = cols_kontraktor
      )
    })

    output$blok_id_ui  <- renderUI({
      selectInput(
        ns("blok_id"),
        "Blok/ No. Kavling:",
        choices = sort(data()$blok_id)
      )
    })

    data_main_filtered  <- reactive({
      req(input$blok_id)
      data_main() %>%
        filter(blok_id == input$blok_id)
    })

    info$server("info", data_main_filtered, cols_rules = cols_spr)

    bukti$server(
      "pencairan",
      title = "Bukti Pencairan Bank",
      sheet_id,
      sheet = sheet_name_pencairan,
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
              class = "card__placeholder",
              icon("user-pen", "fa-3x"),
              p(
                "Input nama kontraktor untuk memulai."
              )
            )
        } else {
          div(
            class = "card--col2",
            bukti$ui(ns("kontr_progress"), title = "Progress Kontraktor"),
            bukti$ui(ns("konstruksi"), title = "Pembayaran Kontraktor")
          )
        }
      })
    })

    bukti$server(
      "kontr_progress",
      title = "Bukti Progress",
      sheet_id,
      sheet = sheet_name_kontr_progress,
      trigger = session$userData$kontr_progress_trigger,
      data_main = data_main_filtered,
      data = data_kontr_progress_raw,
      cols_rules = cols_kontr_progress
    )

    bukti$server(
      "konstruksi",
      title = "Bukti Transfer",
      sheet_id,
      sheet = sheet_name_konstruksi,
      trigger = session$userData$konstruksi_trigger,
      data_main = data_main_filtered,
      data = data_konstruksi_raw,
      cols_rules = cols_konstruksi
    )
  })
}
