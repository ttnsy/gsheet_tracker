box::use(
  shiny[NS, h3, tagList, moduleServer, selectInput],
  reactable[colDef, reactable, reactableOutput, renderReactable],
  dplyr[...]
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
  reactableOutput(ns("spr"))
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$spr <- renderReactable({
      for (i in 1:length(data$Status)) {
        data$Status[i] <- c(
          as.character(
            selectInput(
              inputId = "status",
              label = NULL,
              choices = c("Cancel", "Reject", "Approved"),
              selected = data$Status[i]
            )
          )
        )
      }

      reactable(
        data,
        columns = list(
          Status = colDef(
            html = TRUE,
            align = "center"
          )
        )
      )
    })
  })
}