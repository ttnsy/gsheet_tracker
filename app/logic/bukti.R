box::use(
  glue[glue],
  shiny[h3, isTruthy, tagList, tags, icon, div]
)

box::use(
  app/logic/utils[is_date, format_dttm, format_rupiah]
)

#' @export
format_bukti_value <- function(value) {
  if (is.numeric(value)) {
    format_rupiah(value)
  } else if (is_date(value)) {
    format_dttm(value)
  } else {
    value
  }
}

#' @export
get_bukti_text <- function(value1=NULL, value2=NULL) {
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
  values <- list(value1, value2)
  for (i in seq_len(length(values))) {
    var <- glue("value{i}")
    val <- format_bukti_value(values[[i]])
    assign(var, val)
  }
  text <- get_bukti_text(value1, value2)

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
done_to_target_perc <- function(data, target=4, show_ratio=TRUE) {
  n <- nrow(data)

  out <- glue("{n/target * 100}%")
  if (isTRUE(show_ratio)) {
    out <- glue("{out} {n}/{target}")
  }
}
