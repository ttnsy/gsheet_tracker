box::use(
  reactable[...],
  dplyr[`%>%`, select],
  shiny[...],
)

box::use(
  app/logic/utils_tracker[clean_tracker_cols]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(
      ns("add"),
      "Tambah bukti pencairan",
      class = "btn-add",
      icon = icon("plus"),
      width = "40%"
    ),
    reactableOutput(ns("tbl"))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    output$tbl <- renderReactable({
      req(data())
      data <- clean_tracker_cols(data()) %>%
        select(c("Nama", "Sistem Pembayaran", "Blok/Kavling", everything()))

      reactable(data)
    })

    observeEvent(input$add, {
      showModal(
        modalDialog(
          div(
            class = "modal-pencairan",
            dateInput(
              ns("date"),
              label = "Tanggal Pencairan:"
            ),
            span("Bukti Pencairan:", class = "label-modal"),
            tabsetPanel(
              tabPanel(
                "Google Drive URL",
                textInput(
                  ns("link_pencairan"),
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