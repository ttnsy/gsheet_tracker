box::use(
  dplyr[`%>%`, select, everything, mutate, rename],
  glue[glue]
)

box::use(
  app/logic/utils_tracker[rename_sheet_cols]
)

#' @export
format_info_fields_dat <- function(data, cols_rules) {
  data %>%
    select(blok_id, everything())  %>%
    mutate(
      harga_tanah_bangunan = format(
        harga_tanah_bangunan,
        big.mark=","
      ),
      harga_tanah_bangunan = glue("RP. {harga_tanah_bangunan},-")
    ) %>%
    rename_sheet_cols(cols_rules, revert=TRUE) %>%
    rename(
      `Blok/No. Kavling` = blok_id,
      `Tipe Rumah` = `Tipe Rumah yang Diminati dan Luas Tanah`
    )
  }