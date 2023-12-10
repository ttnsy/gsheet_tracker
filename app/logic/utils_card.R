box::use(
  shiny[div, tags]
)

#' @export
generate_info_field <- function(label, value) {
  div(
    class = "info-field",
    tags$label(label),
    tags$h3(value)
  )
}