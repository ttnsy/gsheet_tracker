box::use(
    dplyr[...],
    janitor[clean_names]
)

#' @export 
get_summary  <- function(pencairan, konstruksi, kontraktor) {
    done_termin  <- konstruksi %>%
        clean_names() %>%
        group_by(nama, blok, nomor_kavling) %>%
        summarise(
            termin_done = n()
        ) %>%
        ungroup()

     pencairan %>%
        clean_names() %>%
        group_by(nama, sistem_pembayaran, blok, nomor_kavling)  %>%
        summarise(
            jml_pencairan = n()
        ) %>%
        ungroup()  %>%
        left_join(clean_names(kontraktor)) %>%
        mutate(
            termin_avail = ifelse(nama_kontraktor == "Asep", jml_pencairan * 3, jml_pencairan * 1),
            termin_avail = pmin(termin_avail, 5)
        ) %>%
        left_join(done_termin)
}