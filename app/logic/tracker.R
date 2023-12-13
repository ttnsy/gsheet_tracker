box::use(
  dplyr[
    `%>%`,
    any_of,
    group_by,
    mutate,
    left_join,
    n,
    rename,
    select,
    summarise,
    ungroup
  ],
  glue[glue],
  googlesheets4[read_sheet],
  janitor[clean_names],
  stringr[str_to_title],
  stats[setNames]
)

#' @export
rename_sheet_cols  <- function(dat, cols_rules, revert=FALSE, rearrange=FALSE) {
  cols_to  <- unlist(cols_rules)
  if (isFALSE(revert)) {
    cols_to <- setNames(names(cols_to), cols_to)
  }
  dat <- dat %>%
    rename(any_of(cols_to))
  if (rearrange) {
    dat <- dat %>%
    select(any_of(names(cols_to)))
  }
  dat
}

#' @export
read_tracker  <- function(sheet_id, sheet_name, cols_rules = NULL) {
  dat  <- read_sheet(sheet_id, sheet_name)
  dat  <-  dat %>%
    mutate(blok_id = glue("{Blok}/{`Nomor Kavling`}"))

  if (!is.null(cols_rules)) {
    dat <- rename_sheet_cols(dat, cols_rules)
  }
  return(dat)
}

#' @export
generate_data_bukti  <- function(data_main, date, link, amount) {
  stopifnot("blok_id" %in% colnames(data_main))
  stopifnot(inherits(date, "Date"))

  out <- data_main %>%
    mutate(
      blok = strsplit(blok_id, "[/]")[[1]][1],
      no_kavling = strsplit(blok_id, "[/]")[[1]][2],
      date = date,
      link = link
    ) %>%
    select(-blok_id)

  if (!is.na(amount)) {
    out <- out %>%
      mutate(
        amount = amount
      )
  }

  out
}

get_done_termin <- function(konstruksi) {
  konstruksi %>%
    group_by(nama, blok_id) %>%
    summarise(
      termin_done = n()
    ) %>%
    ungroup()
}

#' @export
get_summary  <- function(pencairan, konstruksi, kontraktor) {
  done_termin  <- get_done_termin(konstruksi)

  dat  <- pencairan %>%
    group_by(nama, sistem_pembayaran, blok_id)  %>%
    summarise(
      jml_pencairan = n()
    ) %>%
    ungroup()  %>%
    left_join(kontraktor) %>%
    mutate(
      termin_avail = ifelse(nama_kontraktor == "Asep", jml_pencairan * 3, jml_pencairan * 1),
      termin_avail = pmin(termin_avail, 9)
    ) %>%
    left_join(done_termin)
}
