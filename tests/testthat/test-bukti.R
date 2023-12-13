box::use(
  testthat[...],
  fixtuRes[random_numeric, random_datetime, random_string]
)

box::use(
  app/logic/bukti[...]
)

has_thousand_separator_with_decimals <- function(x) {
  #' Regular expression to match a consistent thousand separator (dot or comma)
  #' and an optional decimal part with exactly two digits
  #' Handles both English (1,000.00) and European (1.000,00) conventions
  pattern <- "^\\d{1,3}((,\\d{3})*(\\.\\d{2})?|(\\.\\d{3})*(,\\d{2})?)$"
  return(grepl(pattern, x))
}

describe("format_bukti_value()", {
  rand_num <- round(random_numeric(),2)
  rand_dttm <- random_datetime("2023-01-01", "2023-01-12")

  it("formats numbers with 'Rp.' prefix", {
    expect_equal(
      "Rp. ",
      substr(
        as.character(format_bukti_value(rand_num)), 1, 4
      )
    )
  })

  it("formats numbers with thousand separators and two decimal rounding", {
    expect_true(
      has_thousand_separator_with_decimals(
        gsub("Rp. ", "", format_bukti_value(rand_num))
      )
    )
  })

  it("formats datetime to '%b %d, %Y %H:%M:%S' format", {
    format <-  '%b %d, %Y %H:%M:%S'
    expect_true(
      !is.na(
        strptime(format_bukti_value(rand_dttm), format)
      )
    )
  })
})

