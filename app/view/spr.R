box::use(
  shiny[...],
  shinyFeedback[showToast],
  googlesheets4[sheet_append],
  reactable[...],
  crosstalk[...],
  dplyr[...],
  glue[glue],
  htmlwidgets[JS]
)

box::use(
  app/config[cols_spr, sheet_name_spr],
  app/logic/tracker[rename_sheet_cols],
  app/logic/spr[
    clean_spr_data,
    generate_info_spr,
    reactable_spr,
    spr_to_append
  ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "container-spr",
    uiOutput(ns("spr_filter")),
    div(
      class = "container-tbl card",
      reactableOutput(ns("spr"))
    )
  )
}

#' @export
server <- function(id, sheet_id, spr_data) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    spr_clean <- eventReactive(spr_data(), {
      data <- spr_data()
      clean_spr_data(data)
    })

    spr_shared <- SharedData$new(spr_clean)

    output$spr_filter <- renderUI({
      filter_checkbox(
        "status",
        "Status",
        inline = TRUE,
        spr_shared,
        ~ Status
      )
    })

    output$spr <- renderReactable({
      reactable_spr(spr_shared)
    })

    col_state <- reactive({
      getReactableState("spr", "selected")
    })

    to_edit <- reactive({
      req(col_state())
      data <- spr_data()

      data[col_state(), ]
    })

    observeEvent(col_state(), {
      dat <- to_edit()
      info <- generate_info_spr(dat)

      showModal(
        modalDialog(
          class = "modal-edit",
          tagList(
            info,
            selectInput(
              ns("status"),
              "Status",
              choices = c("Cancel", "Process", "Reject"),
              selected = dat$status
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

    observeEvent(input$submit, {
      req(input$status)
      dat <- to_edit()
      dat <- spr_to_append(dat)

      sheet_append(sheet_id, dat, sheet = sheet_name_spr)
      removeModal()
      session$userData$spr_trigger(session$userData$spr_trigger() + 1)
      showToast("success", "Success updating status.")
    })
  })
}
