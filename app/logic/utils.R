box::use(
  glue[glue]
)

#' @export
is_date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

#' @export
format_dttm <- function(x) {
  format(x, "%b %d, %Y %H:%M:%S")
}

#' @export
format_rupiah <- function(x) {
  x <- round(x, digits = 2)
  x <- format(x, nsmall = 2, big.mark = ".", decimal.mark=",")
  glue("Rp. {x}")
}
