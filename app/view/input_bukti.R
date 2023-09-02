box::use(
  shiny[...],
  dplyr[`%>%`, select, mutate],
  glue[glue],
  stringr[str_to_title],
  googlesheets4[sheet_append],
  googledrive[...],
  janitor[clean_names]
)

box::use(
  app/logic/utils_tracker[generate_data_bukti]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bttn_add_ui"))
}

#' @export
server <- function(id, label, data_main_filtered, sheet_id, sheet) {
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
                  label = "",
                  accept = "image/*"
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

    out <- eventReactive(input$submit, {
      if (input$url != "") {
        link  <- input$url
      }
      if (!is.null(input$upload)) {
        res  <- drive_upload(
          media = input$upload$datapath,
          name = input$upload$name
        )
        link  <- res$id
      }
      return(
        list(
          date = input$date,
          link = link
        )
      )
    })

    observeEvent(out(), {
      req(out())
      req(data_main_filtered())

      date <- out()$date
      link <- out()$link
      data_main  <- data_main_filtered() %>% clean_names()
      cols_rules  <- config::get(file = "data_cols.yml")[[sheet]]

      dat <- generate_data_bukti(
        cols_rules,
        data_main,
        date,
        link
      )

      sheet_append(sheet_id, dat, sheet = sheet)
      removeModal()
    })
  })
}