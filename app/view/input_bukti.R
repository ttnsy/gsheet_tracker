box::use(
  shiny[...],
  glue[glue],
  stringr[str_to_title]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bttn_add_ui"))
}

#' @export
server <- function(id, label) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    label_bttn  <- glue("Tambah bukti {label}")
    label_date  <- glue("Tanggal {label}:")
    label_bukti  <- glue("Bukti {label}:")

    output$bttn_add_ui  <- renderUI({
      actionButton(
        ns("bttn_add"),
        label_bttn,
        class = "btn-add",
        icon = icon("plus"),
        width = "40%"
      )
    })

    observeEvent(input$bttn_add, {
      showModal(
        modalDialog(
          div(
            class = "modal-input",
            dateInput(
              ns("date"),
              label = label_date
            ),
            span(label_bukti, class = "label-modal"),
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