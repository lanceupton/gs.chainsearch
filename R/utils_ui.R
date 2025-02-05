
#' @importFrom bs4Dash actionButton
btn_primary <- function(inputId, label = NULL, icon = NULL) {
  actionButton(
    inputId = inputId,
    label = label,
    icon = icon,
    status = "primary"
  )
}

#' @importFrom bs4Dash bs4Card
card_primary <- function(title, ..., footer = NULL, width = NULL, icon = NULL) {
  bs4Card(
    title = title,
    footer = footer,
    status = "primary",
    width = width,
    icon = icon,
    ...
  )
}

#' @importFrom DT datatable formatRound
dt_minimal <- function(data, colnames, selection = c("single", "multiple", "none"), ...) {
  selection <- match.arg(selection)
  if (identical(selection, "none")) {
    selection <- NULL
  } else {
    selection <- list(mode = selection, target = "row")
  }
  datatable(
    data = data,
    rownames = NULL,
    colnames = colnames,
    class = "display",
    options = list(dom = "tp"),
    escape = TRUE,
    style = "bootstrap4",
    width = NULL,
    height = NULL,
    elementId = NULL,
    selection = selection,
    ...
  ) |>
    formatRound(columns = which(sapply(data, is.numeric)), digits = 2)
}
