box::use(
  dplyr[`%>%`, select, everything, mutate, rename],
  glue[glue],
  shiny[div, tags]
)

box::use(
  app/logic/tracker[rename_sheet_cols],
  app/logic/utils[format_rupiah]
)

#' @export
generate_info <- function(label, value) {
  div(
    class = "info",
    tags$label(label),
    tags$h4(value)
  )
}

#' @export
format_info_data <- function(data, cols_rules) {
  data %>%
    select(blok_id, everything())  %>%
    mutate(
      harga_tanah_bangunan = format_rupiah(harga_tanah_bangunan)
    ) %>%
    rename_sheet_cols(cols_rules, revert=TRUE) %>%
    rename(
      `Blok/No. Kavling` = blok_id,
      `Tipe Rumah` = `Tipe Rumah yang Diminati dan Luas Tanah`
    )
}
