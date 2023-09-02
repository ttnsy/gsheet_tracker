box::use(
  dplyr[`%>%`, all_of, select, mutate, rename, rename_at, rename_with, rename_all],
  glue[glue],
  googlesheets4[read_sheet],
  janitor[clean_names],
  stringr[str_to_title],
  stats[setNames]
)

#' @export
rename_sheet_cols  <- function(dat, cols_list, revert=FALSE) {
  cols_to  <- unlist(cols_list)
  if (isFALSE(revert)) {
    cols_to <- setNames(names(cols_to), cols_to)
  }
  dat %>%
    rename(any_of(cols_to))
}

#' @export
read_tracker  <- function(sheet_id, sheet_name, col_names = NULL) {
  dat  <- read_sheet(sheet_id, sheet_name)
  dat  <-  dat %>%
    mutate(blok_id = glue("{Blok}/{`Nomor Kavling`}")) 

  if(!is.null(col_names)){
    dat <- rename_sheet_cols(dat, col_names)
  }
  return(dat)
}

#' @export
clean_tracker_cols  <- function(data) {
  data %>%
    select(-c("blok", "no_kavling")) %>%
    rename("Blok/Kavling" = "blok_id")  %>%
    rename_with(~str_to_title(gsub("_", " ", .)))
}

#' @export
generate_data_bukti  <- function(cols_rules, data_main, date, link){
  stopifnot("blok_id" %in% colnames(data_main))
  stopifnot(inherits(date, "Date"))

  cols <- unlist(cols_rules, use.names=FALSE)
  cols_target  <- names(cols_rules)

  out <- data_main %>%
    mutate(
      blok = strsplit(blok_id, "[/]")[[1]][1],
      no_kavling = strsplit(blok_id, "[/]")[[1]][2],
      date = date,
      link = link
    ) %>%
    select(all_of(cols)) %>%
    rename_all(~ as.character(cols_target))

  out
}
