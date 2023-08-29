box::use(
    dplyr[`%>%`, mutate],
    glue[glue],
    googlesheets4[read_sheet],
    janitor[clean_names]
)

#' @export
read_tracker  <- function(sheet_id, sheet_name, clean_names = FALSE){
    dat  <- read_sheet(sheet_id, sheet_name)
    dat  <-  dat %>%
     mutate(blok_id = glue("{Blok}/{`Nomor Kavling`}"))

    if(clean_names){
        dat <- clean_names(dat)
    }

    return(dat)
}