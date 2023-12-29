box::use(
  glue[glue],
  dplyr[`%>%`, select, pull],
  htmltools[tagQuery],
  testthat[describe, it, expect_equal, expect_true],
  fixtuRes[random_data_frame, random_string]
)

box::use(
  app/logic/info[...],
  app/logic/utils[format_rupiah]
)

describe("generate_info()", {

  it("returns div with 'info' class", {
    out <- generate_info(
      label = random_string(),
      value = random_string()
    )
    expect_true(grepl("^<div .*</div>$", out))
    expect_true(tagQuery(out)$hasClass("info"))
  })
})

describe("format_info_data()", {
  df_config <- list(
    columns = list(
      nama = list(
        type = "string",
        length = 3
      ),
      blok_id = list(
        type = "integer",
        max = 10
      ),
      sistem_pembayaran = list(
        type = "string",
        length = 3
      ),
      tipe_dan_lt = list(
        type = "string",
        length = 3
      ),
      harga_tanah_bangunan = list(
        type = "numeric",
        min = 100000000,
        max = 200000000
      ),
      disc = list(
        type = "numeric",
        min = 1000000,
        max = 2000000
      )
    )
  )

  dat <- random_data_frame(df_config, 6)
  cols_rules <- config::get(file="../data_cols.yml")$spr
  out <- format_info_data(dat, cols_rules)

  it("rename columns based on cols_rules input except for tipe_dan_lt", {
    before <- colnames(dat)
    after <- colnames(out)

    cols_rules <- cols_rules[cols_rules %in% before]
    cols_rules <- cols_rules[cols_rules != "tipe_dan_lt"]

    expect_true(all((names(cols_rules) %in% after) == TRUE))
  })

  it("formats harga_tanah_bangunan formatted with format_rupiah function", {
    col <- names(cols_rules[cols_rules == "harga_tanah_bangunan"])
    actual <- out %>%
      select(all_of(col)) %>%
      pull()
    expected <- format_rupiah(dat$harga_tanah_bangunan)

    expect_equal(actual, expected)
  })
})
