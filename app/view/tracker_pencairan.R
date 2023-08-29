box::use(
  reactable[...],
  shiny[req, actionButton, icon, verticalLayout, h3, moduleServer, NS, tagList],
)

box::use(
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  verticalLayout(
    actionButton(
        ns("add_pencairan"),
        "Add",
        class = "btn-success",
        style = "color: #fff",
        icon = icon('plus'),
        width = "40%"
    ),
    reactableOutput(ns("tbl_pencairan"))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$tbl_pencairan <- renderReactable({
        req(data())
        reactable(data())
    })
  })
}