box::use(
  glue[glue],
  htmltools[tagQuery, tagGetAttribute],
  testthat[describe, it, expect_equal, expect_true],
  fixtuRes[random_numeric, random_datetime, random_string]
)

box::use(
  app/logic/bukti[...]
)

has_thousand_sep_with_two_dec <- function(x) {
  #' Regular expression to match a consistent thousand separator (dot or comma)
  #' and an optional decimal part with exactly two digits
  #' Handles both English (1,000.00) and European (1.000,00) conventions
  pattern <- "^\\d{1,3}((,\\d{3})*(\\.\\d{2})?|(\\.\\d{3})*(,\\d{2})?)$"
  return(grepl(pattern, x))
}

describe("format_bukti_value()", {
  rand_num <- round(random_numeric(), 2)
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
      has_thousand_sep_with_two_dec(
        gsub("Rp. ", "", format_bukti_value(rand_num))
      )
    )
  })

  it("formats datetime to '%b %d, %Y %H:%M:%S' format", {
    format <-  "%b %d, %Y %H:%M:%S"
    expect_true(
      !is.na(
        strptime(format_bukti_value(rand_dttm), format)
      )
    )
  })
})

describe("get_bukt()", {
  value1 <- random_string()
  value2 <- random_string()

  class1 <- "bukti__text--dark"
  class2 <- "bukti__text--light"

  tagq <- tagQuery(
    get_bukti_text(
      value1 = value1,
      value2 = value2
    )
  )

  it("assigns bukti__text--dark to value1 and bukti__text--light to value2", {
    expect_true(tagq$hasClass(class1)[1])
    expect_true(tagq$hasClass(class2)[2])
  })
})

describe("bukti_button_item", {
  url <- "https://www.google.com/"
  out <- bukti_button_item(
    url = url,
    value1 = random_string(),
    value2 = random_string()
  )
  tagq <- tagQuery(out)

  it("returns an HTML button", {
    expect_true(
      grepl("^<button .*</button>$", out)
    )
  })

  it("assigns bukti__button class to output", {
    expect_true(tagq$hasClass("bukti__button"))
  })

  it("adds window.open(url, '_blank'); method in onclick event to url input", {
    expect_true(tagq$hasAttrs("onclick"))
    expect_equal(
      glue("window.open('{url}','_blank');"),
      tagGetAttribute(out, "onclick")
    )
  })
})
