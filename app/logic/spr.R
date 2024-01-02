box::use(
  htmlwidgets[JS],
  reactable[colDef, reactable]
)

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