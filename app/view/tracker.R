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
  app/view/card_info,
  app/view/input_kontraktor
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("blok_id_ui")),
    div(
      class = "container-tracker",
      card_info$ui(ns("card_info"))
    )
    # div(
    #   class = "tracker-contents",
    #   fluidRow(
    #     bukti$ui(ns("pencairan"), title = "Pencairan Bank")
    #   ),
    #   fluidRow(
    #     input_kontraktor$ui(ns("input_kontraktor")),
    #     uiOutput(ns("bukti_ui_konstruksi"))
    #   )
    # )
  )
}

#' @export
server <- function(id, sheet_id, data, data_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cols_spr <- data_cols[["spr"]]
    cols_pencairan  <- data_cols[["pencairan"]]
    cols_konstruksi  <- data_cols[["konstruksi"]]
    cols_kontraktor  <- data_cols[["kontraktor"]]
    cols_kontraktor_progress  <- data_cols[["kontraktor_progress"]]

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

    data_kontr_progress_raw  <- reactive({
      session$userData$kontr_progress_trigger()
      read_tracker(
        sheet_id,
        "kontraktor_progress",
        cols_rules = cols_kontraktor_progress
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

    card_info$server("card_info", data_main_filtered, cols_rules = cols_spr)

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
          div(
            class = "container-kontr",
            bukti$ui(ns("kontr_progress"), title = "Progress Kontraktor"),
            bukti$ui(ns("konstruksi"), title = "Pembayaran Kontraktor")
          )
        }
      })
    })

    bukti$server(
      "kontr_progress",
      sheet_id,
      sheet = "kontraktor_progress",
      trigger = session$userData$kontr_progress_trigger,
      data_main = data_main_filtered,
      data = data_kontr_progress_raw,
      cols_rules = cols_kontraktor_progress
    )

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