box::use(
  dplyr[`%>%`, group_by, select, summarise, n, ungroup, left_join, mutate, rename, rename_with],
  stringr[str_to_title]
)

#' @export 
get_summary  <- function(pencairan, konstruksi, kontraktor) {
  done_termin  <- konstruksi %>%
    group_by(nama, blok_id) %>%
    summarise(
      termin_done = n()
    ) %>%
    ungroup()

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
