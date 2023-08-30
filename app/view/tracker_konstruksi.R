box::use(
  reactable[...],
  dplyr[`%>%`, select, count],
  shiny[...],
  shinyjs[useShinyjs, enable, disable, disabled]
)

box::use(
  app/logic/utils_tracker[clean_tracker_cols]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    div(
      class="container-input-kontraktor",
      uiOutput(ns("kontr_input_ui")),
      actionButton(
        ns("edit_kontraktor"),
        label = "",
        title = "Click to edit",
        icon = icon("user-pen")
      )
    ),
    actionButton(
        ns("add"),
        "Tambah bukti transfer",
        class = "btn-add",
        icon = icon("plus"),
        width = "40%"
    ),
    reactableOutput(ns("tbl"))
  )
}

#' @export
server <- function(id, data, summary) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    output$kontr_input_ui  <- renderUI({
      req(summary())
      textInput(
        ns("kontraktor"),
        label = "Nama Kontraktor:",
        value = summary()$`Nama Kontraktor`
      ) %>%
        disabled()
    })

    kontr_info  <- reactive({
      summary() %>%
        janitor::clean_names() %>%
        count(nama_kontraktor, name = "Jumlah Kavling")
    })

    observeEvent(input$edit_kontraktor, {
      req(kontr_info())
      req(summary())

      showModal(
        modalDialog(
          tagList(
            selectInput(
              ns("kontr_edit"),
              label = "Nama kontraktor:",
              choices = c("Asep", "Buyung"),
              selected = summary()$`Nama Kontraktor`
            ),
            reactable(kontr_info())
          )
        )
      )
    })

    output$tbl <- renderReactable({
      req(data())
      data_tbl <- clean_tracker_cols(data()) %>%
        select(c("Nama", "Sistem Pembayaran", "Blok/Kavling", everything()))
      reactable(data_tbl)
    })

    observeEvent(input$add, {
      showModal(
        modalDialog(
          div(
            class = "modal-konstruksi",
            dateInput(
              ns("date"),
              label = "Tanggal Transfer:"
            ),
            span("Bukti Transfer:", class = "label-modal"),
            tabsetPanel(
              tabPanel(
                "Google Drive URL",
                textInput(
                  ns("url"),
                  label = ""
                )
              ),
              tabPanel(
                "Upload Image",
                fileInput(
                  ns("upload"),
                  label = ""
                )
              )
            )
          ),
          footer = list(
            modalButton("Cancel"),
            actionButton(
              ns("submit"),
              "Submit",
              class = "btn btn-primary",
              style = "color: white"
            )
          ) 
        )
      )
    })
  })
}