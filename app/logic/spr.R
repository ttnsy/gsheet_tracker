box::use(
  dplyr[...],
  htmltools[a],
  htmlwidgets[JS],
  reactable[colDef, reactable],
  shiny[tagList]
)

box::use(
  app/config[cols_spr],
  app/logic/info[generate_info],
  app/logic/tracker[rename_sheet_cols]
)

#' @export
clean_spr_data <- function(data) {
  data %>%
    select(-c("blok_id"))  %>%
    mutate(
      bukti_booking_fee = as.character(
        a("Gdrive Link", href = bukti_booking_fee, target = "_blank")
      )
    ) %>%
    select(timestamp, status, everything()) %>%
    rename_sheet_cols(cols_spr, revert=TRUE)
}

#' @export
generate_info_spr <- function(data) {
  tagList(
    generate_info(
      "Nama",
      data$nama
    ),
    generate_info(
      "Blok/No. Kavling",
      glue("{data$blok}/{data$no_kavling}")
    )
  )
}

#' @export
spr_to_append <- function(data) {
  data %>%
    select(-blok_id) %>%
    mutate(
      timestamp = Sys.time(),
      status = input$status
    ) %>%
    rename_sheet_cols(cols_spr, revert=TRUE)
}

#' @export
reactable_spr <- function(data) {
  reactable(
    data,
    class = "tbl-spr",
    searchable = TRUE,
    wrap = FALSE,
    resizable = TRUE,
    defaultPageSize = 15,
    minRows = 1,
    selection = "single",
    onClick = "select",
    rowStyle = JS("function(rowInfo) {
      if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee', boxShadow: 'inset 2px 0 0 0 #ffa62d' }
      }
    }"),
    defaultColDef = colDef(
      cell = function(value) {
        if (!is.numeric(value)) {
          return(value)
        }
        format(value, big.mark = ",")
      },
      headerClass = "reactable__header",
      align = "left",
      vAlign = "center",
      style = list(
        height = "40px",
        fontSize = "12.5px"
      )
    ),
    columns = list(
      Timestamp = colDef(
        sticky = "left"
      ),
      Status = colDef(
        sticky = "left",
        style = function(value) {
          if (value == "Process") {
            color <- "#008000"
          } else if (value == "Reject") {
            color  <- "#e00000"
          } else {
            color  <- "#777"
          }
          list(
            color = color,
            fontWeight = "bold"
          )
        },
        align = "center"
      ),
      `Bukti Booking Fee` = colDef(
        html = TRUE
      )
    )
  )
}