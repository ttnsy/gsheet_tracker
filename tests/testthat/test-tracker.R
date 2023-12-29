box::use(
  glue[glue],
  dplyr[`%>%`, select, pull],
  htmltools[tagQuery],
  testthat[describe, it, expect_equal, expect_true],
  fixtuRes[random_data_frame, random_string]
)

box::use(
  app/logic/tracker[...]
)

describe("rename_sheet_cols()", {
  cols_rules <- list(
    name1 = random_string(),
    name2 = random_string(),
    name3 = random_string()
  )

  dat <- random_data_frame(
    list(
      columns = list(
        name1 = list(type = "string"),
        name2 = list(type = "integer"),
        name3 = list(type = "string")
      )
    ),
    size = 6
  )

  it("rename columns based on cols_rules' value by default", {
    expect_equal(
      unname(unlist(cols_rules)),
      colnames(rename_sheet_cols(dat, cols_rules))
    )
  })

  it("rename columns based on cols_rules' key when revert=TRUE", {
    expect_equal(
      names(cols_rules),
      colnames(rename_sheet_cols(dat, cols_rules, revert = TRUE))
    )
  })
})
