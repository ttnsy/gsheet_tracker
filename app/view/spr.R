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
  app/logic/tracker[rename_sheet_cols]
)

js <- "
$(document).on('shiny:value', function(e) {
  if(e.name === 'spr'){
    setTimeout(function(){Shiny.bindAll(document.getElementById('spr'))}, 0);
  }
});
"

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-spr",
    uiOutput(ns("spr_filter")),
    reactableOutput(ns("spr"))
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
        spr_shared,
        ~ Status
      )
    })

    output$spr <- renderReactable({
      sticky_style  <- list(backgroundColor = "#f7f7f7")

      reactable(
        spr_shared,
        class = "tbl-spr",
        searchable = TRUE,
        bordered = TRUE,
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
        columns = list(
          Timestamp = colDef(
            sticky = "left",
            style = sticky_style,
            headerStyle = sticky_style
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
                backgroundColor = "#f7f7f7",
                color = color,
                fontWeight = "bold"
              )
            },
            headerStyle = sticky_style,
            align = "center"
          ),
          `Bukti Booking Fee` = colDef(
            html = TRUE
          )
        )
      )
    })

    col_state <- reactive({getReactableState("spr", "selected")})

    to_edit <- reactive({
      req(col_state())
      data <- spr_data()

      data[col_state(), ]
    })

    observeEvent(col_state(), {
      dat <- to_edit()

      showModal(
        modalDialog(
          class = "modal-edit",
          title = glue("{dat$nama} ({dat$blok}/{dat$no_kavling})"),
          selectInput(
            ns("status"),
            "Status",
            choices = c("Cancel", "Process", "Reject"),
            selected = dat$status
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
