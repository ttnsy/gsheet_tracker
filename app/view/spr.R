options(scipen = 99)

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
  app/logic/info[generate_info],
  app/logic/tracker[rename_sheet_cols]
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
server <- function(id, sheet_id, spr_data, cols_rules) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    spr_clean <- reactive({
      req(spr_data())

      spr_data() %>%
        select(-c("blok_id"))  %>%
        mutate(
          bukti_booking_fee = as.character(
          a("Gdrive Link", href = bukti_booking_fee, target = "_blank")
        )
      ) %>%
      select(timestamp, status, everything()) %>%
      rename_sheet_cols(cols_rules, revert=TRUE)
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
      reactable(
        spr_shared,
        class = "tbl-spr",
        searchable = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        defaultPageSize = 15,
        minRows = 1,
        selection = "single",
        onClick = "select",
        rowStyle = JS("function(rowInfo) {
          if (rowInfo && rowInfo.selected) {
          return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #ffa62d' }
          }
        }"),
        defaultColDef = colDef(
          cell = function(value) {
            if (!is.numeric(value)) {
              return(value)
            }
            format(value, big.mark = ",")
          },
          headerClass = "reactable__header",
          align = "left",
          vAlign = "center",
          style = list(
            height = "40px",
            fontSize = "12.5px"
          )
        ),
        columns = list(
          Timestamp = colDef(
            sticky = "left"
          ),
          Status = colDef(
            sticky = "left",
            style = function(value) {
              if (value == "Process") {
                color <- "#008000"
              } else if (value == "Reject") {
                color  <- "#e00000"
              } else {
                color  <- "#777"
              }
              list(
                color = color,
                fontWeight = "bold"
              )
            },
            align = "center"
          ),
          `Bukti Booking Fee` = colDef(
            html = TRUE
          )
        )
      )
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

      info <- tagList(
        generate_info(
          "Nama",
          dat$nama
        ),
        generate_info(
          "Blok/No. Kavling",
          glue("{dat$blok}/{dat$no_kavling}")
        )
      )

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

      dat <- to_edit() %>%
        select(-blok_id) %>%
        mutate(
          timestamp = Sys.time(),
          status = input$status
        ) %>%
      rename_sheet_cols(cols_rules, revert=TRUE)

      sheet_append(sheet_id, dat, sheet = "spr")
      removeModal()
      session$userData$spr_trigger(session$userData$spr_trigger() + 1)
      showToast("success", "Success updating status.")
    })
  })
}
