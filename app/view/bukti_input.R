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
  app/logic/utils_tracker[generate_data_bukti, rename_sheet_cols]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("bttn_add_ui"))
}

#' @export
server <- function(id, label, data_main_filtered, sheet_id, sheet, cols_rules, trigger) {
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
            radioButtons(
              ns("upload_opt"),
              label = label_bukti,
              inline = TRUE,
              c(
                "Google Drive URL" = "url",
                "Upload from computer" = "upload"
              )
            ),
            uiOutput(ns("upload_input_ui"))
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

    output$upload_input_ui  <- renderUI({
      req(input$upload_opt)
      if (input$upload_opt == "url") {
        textInput(
          ns("url"),
          label = NULL,
          placeholder = "Paste URL here"
        )
      } else {
        fileInput(
          ns("upload"),
          label = NULL,
          accept = "image/*"
        )
      }
    })

    out <- eventReactive(input$submit, {
      if (input$url != "") {
        link  <- input$url
      }
      if (!is.null(input$upload)) {
        res  <- drive_upload(
          path = "pk-tracker_sample",
          media = input$upload$datapath,
          name = input$upload$name
        )
        link  <- res$id
        link <- glue("https://drive.google.com/file/d/{res$id}")
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

      dat <- generate_data_bukti(
        data_main,
        date,
        link
      )

      dat <- rename_sheet_cols(dat, cols_rules, revert=TRUE, rearrange=TRUE)
      sheet_append(sheet_id, dat, sheet = sheet)
      trigger(trigger() + 1)
      removeModal()
    })
  })
}