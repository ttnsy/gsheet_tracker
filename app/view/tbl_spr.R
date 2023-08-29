box::use(
  shiny[...],
  reactable[...],
  crosstalk[...],
  dplyr[...],
  glue[glue],
  htmlwidgets[JS]
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
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    col_state <- reactive({
        getReactableState("spr", "selected")
    })

    observeEvent(col_state(), {
        to_edit  <- data[col_state(), ]

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
                    modalButton('Cancel'),
                    actionButton(
                        ns('submit'),
                        'Submit',
                        class = "btn btn-primary",
                        style = "color: white"
                    )
                )
            )
        )
    })

    edit_dat <- reactive({
        req(input$status)
        hold  <- data[col_state(), ]
        if(input$status != hold$Status){
            out <- hold %>%
            mutate(Status = input$Status)
        } else {
            out  <- hold
        }
        return(
            list(
                data_old  = hold,
                data_new = out
            )
        )
    })

    data <- data %>%
        select(-c("blok_id"))  %>%
        mutate(
            `Bukti Booking Fee` = as.character(
                a("Gdrive Link", href = `Bukti Booking Fee`, target = "_blank")
            )
        )

    data_spr <- SharedData$new(data)

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
  })
}