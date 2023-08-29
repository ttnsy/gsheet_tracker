box::use(
    dplyr[`%>%`, select, mutate, rename, rename_with],
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