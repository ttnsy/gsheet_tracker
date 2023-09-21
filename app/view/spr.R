box::use(
  shiny[...],
  shinyFeedback[showToast],
  googlesheets4[range_write],
  reactable[...],
  crosstalk[...],
  dplyr[...],
  glue[glue],
  htmlwidgets[JS]
)

box::use(
  app/logic/utils_tracker[read_tracker]
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
server <- function(id, sheet_id, spr_data) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    spr_clean <- reactive({
      req(spr_data())
      spr_data() %>%
        select(-c("blok_id"))  %>%
        mutate(
          `Bukti Booking Fee` = as.character(
          a("Gdrive Link", href = `Bukti Booking Fee`, target = "_blank")
        )
      ) %>%
      select(Timestamp, Status, everything())
    })

    spr_shared <- SharedData$new(spr_clean)

    output$spr_filter <- renderUI({
      filter_checkbox(
        "Status",
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

    col_state <- reactive({
      getReactableState("spr", "selected")
    })

    observeEvent(col_state(), {
      data <- spr_clean()
      to_edit <- data[col_state(), ]

      showModal(
        modalDialog(
          class = "modal-edit",
          title = glue("{to_edit$Nama} ({to_edit$Blok}/{to_edit$`Nomor Kavling`})"),
          selectInput(
            ns("status"),
            "Status",
            choices = c("Cancel", "Process", "Reject"),
            selected = to_edit$Status
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
      range  <- glue("X{col_state()+1}")
      range_write(
        sheet_id,
        data = data.frame(Status = input$status),
        sheet = "spr",
        range = range,
        col_names = FALSE
      )
      removeModal()
      session$userData$spr_trigger(session$userData$spr_trigger() + 1)
      showToast("success", "Success updating status.")
    })
  })
}
