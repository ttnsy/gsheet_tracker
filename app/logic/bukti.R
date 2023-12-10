box::use(
  glue[glue],
  shiny[h3, isTruthy, tagList, tags, icon, div]
)

#' @export
get_bukti_text <- function(value1, value2) {
  value1 <- if (isTruthy(value1)) {
    h3(class = "bukti__text--dark", value1)
  }

  value2 <- if (isTruthy(value2)) {
    h3(class = "bukti__text--light", value2)
  }

  tagList(value1, value2)
}

#' @export
bukti_button_item <- function(url, value1=NULL, value2=NULL) {
  onclick <- glue("window.open('{url}','_blank');")
  text <- get_bukti_text(value1, value2)

  values <-  c(value1, value2)
  tags$button(
    class = "bukti__button",
    onclick=onclick,
    icon("file-image", class = "fa-solid fa-2xl"),
    div(
      class = "bukti__label",
      text
    )
  )
}

#' @export
done_to_target_perc <- function(data, show_ratio=TRUE) {
  n <- nrow(data)
  total <- 4

  out <- glue("{n/total * 100}%")
  if (isTRUE(show_ratio)) {
    out <- glue("{out} {n}/{total}")
  }
}