box::use(
  dplyr[`%>%`, group_by_at, slice, ungroup]
)

box::use(
  app/logic/tracker[read_tracker]
)

#' @export
slice_max_timestamp <- function(data, by) {
  data %>%
    group_by_at(by) %>%
    slice(which.max(timestamp)) %>%
    ungroup()
}

#' @export
read_spr_data <- function(sheet_id, sheet_name, cols_rules) {
  read_tracker(
    sheet_id,
    sheet_name = sheet_name,
    cols_rules = cols_rules
  ) %>%
    slice_max_timestamp(by = "blok_id")
}
