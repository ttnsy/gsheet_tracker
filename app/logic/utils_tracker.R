box::use(
    dplyr[`%>%`, all_of, select, mutate, rename, rename_with, rename_all],
    glue[glue],
    googlesheets4[read_sheet],
    janitor[clean_names],
    stringr[str_to_title]
)

#' @export
read_tracker  <- function(sheet_id, sheet_name, clean_names = FALSE) {
    dat  <- read_sheet(sheet_id, sheet_name)
    dat  <-  dat %>%
     mutate(blok_id = glue("{Blok}/{`Nomor Kavling`}"))

    if (clean_names) {
        dat <- clean_names(dat)
    }

    return(dat)
}

#' @export
clean_tracker_cols  <- function(data) {
  data %>%
    select(-c("blok", "nomor_kavling")) %>%
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
      nomor_kavling = strsplit(blok_id, "[/]")[[1]][2],
      date = date,
      link = link
    ) %>%
    select(all_of(cols)) %>%
    rename_all(~ as.character(cols_target))

  out
}
