box::use(
  shiny[...],
  shinyvalidate[...],
  dplyr[`%>%`, select, mutate],
  glue[glue],
  stringr[str_to_title],
  googlesheets4[sheet_append],
  googledrive[...],
  janitor[clean_names]
)

box::use(
  app/logic/tracker[generate_data_bukti, rename_sheet_cols]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("btn_add_ui"))
}

#' @export
server <- function(id, label, data_main_filtered, sheet_id, sheet, cols_rules, trigger) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    iv  <- InputValidator$new()
    label_btn  <- glue("Tambah bukti {label}")
    label_date  <- glue("Tanggal {label}:")
    label_bukti  <- glue("Bukti {label}:")

    output$btn_add_ui  <- renderUI({
      actionButton(
        ns("btn_add"),
        "Tambah bukti",
        title = label_btn,
        icon = icon("square-plus", "fa-reguler fa-2xl")
      )
    })

    observeEvent(input$btn_add, {
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

    iv$add_rule("url", function(url) {
      pattern <- "^(https?://)?(www\\.)?drive\\.google\\.com/.*$"
      is_gdrive_url  <- grepl(pattern, url, ignore.case = TRUE)

      if (!is_gdrive_url) {
        "Not a google drive URL."
      }
    })

    out <- eventReactive(input$submit, {
      if (input$upload_opt == "url") {
        if (iv$is_valid()) {
          link  <- input$url
        } else {
          iv$enable()
        }
      } else {
        res  <- drive_upload(
          path = "pk-tracker_sample",
          media = input$upload$datapath,
          name = input$upload$name
        )
        link  <- res$id
        link <- glue("https://drive.google.com/file/d/{res$id}")
      }
      req(exists("link"))
      list(
        date = input$date,
        link = link
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
