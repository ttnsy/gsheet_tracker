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
server <- function(id, sheet_id, data) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns

    #' trigger to reload spr data from gsheet
    session$userData$spr_trigger  <- reactiveVal(0)

    spr <- reactive({
      session$userData$spr_trigger()
      out <- NULL

      tryCatch({
        out <- read_tracker(sheet_id, sheet_name = "spr")
      }, error = function(e) {
        print(e)
        showToast("error", glue("error reading SPR sheet: {e}"))
      })
      out
    })

    spr_clean <- reactive({
      req(spr())
      spr() %>%
        select(-c("blok_id"))  %>%
        mutate(
          `Bukti Booking Fee` = as.character(
          a("Gdrive Link", href = `Bukti Booking Fee`, target = "_blank")
        )
      )
    })

    data_spr <- SharedData$new(spr_clean)

    output$spr_filter <- renderUI({
      filter_checkbox(
        "Status",
        "Status",
        inline = TRUE,
        data_spr,
        ~ Status
      )
    })

    output$spr <- renderReactable({
      reactable(
        data_spr,
        wrap = TRUE,
        searchable = TRUE,
        selection = "single",
        onClick = "select",
        rowStyle = JS("function(rowInfo) {
            if (rowInfo && rowInfo.selected) {
            return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #ffa62d' }
            }
        }"),
        columns = list(
          Status = colDef(
            style = function(value) {
                if (value == "Process") {
                    color <- "#008000"
                } else if (value == "Reject") {
                    color  <- "#e00000"
                } else {
                    color  <- "#777"
                }
                list(color = color, fontWeight = "bold")
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

    observeEvent(col_state(), {
      data <- spr_clean()
      to_edit <- data[col_state(), ]

      showModal(
        modalDialog(
          div(
            class = "modal-edit",
            selectInput(
                ns("status"),
                "Status",
                choices = c("Cancel", "Process", "Reject"),
                selected = to_edit$Status
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