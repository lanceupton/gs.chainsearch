
# I/O ---------------------------------------------------------------------

#' @importFrom data.table fwrite
write_csv <- function(x, file) {
  fwrite(x, file)
}

#' @importFrom data.table fread
read_csv <- function(file, ...) {
  fread(file = file, stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE, data.table = FALSE, ...)
}

# SHINY -------------------------------------------------------------------

# Create an observer to toggle DOM based on truthiness of a reactive expression
#' @importFrom shinyjs toggleState
bind_state <- function(id, condition, invert = FALSE, asis = FALSE) {
  rx1 <- reactive(isTruthy(condition()))
  rx2 <- reactive(ifelse(invert, !rx1(), rx1()))
  observe({
    for (idx in id) {
      toggleState(id = idx, condition = rx2(), asis = asis)
    }
  })
}

#' @importFrom gargoyle init
trigger_init <- function(id) {
  init(id)
}

#' @importFrom gargoyle trigger
trigger_press <- function(id) {
  trigger(id)
}

#' @importFrom gargoyle watch
trigger_watch <- function(id) {
  watch(id)
}

#' @importFrom gargoyle on
#' @importFrom rlang enquo0
#' @importFrom shiny quoToFunction
on_trigger <- function(id, expr) {
  fun <- quoToFunction(enquo0(expr))
  on(id, fun())
}

# HELPERS -----------------------------------------------------------------

#' @importFrom shiny isTruthy
"%||%" <- function(x, y) {
  if (isTruthy(x)) x else if (isTruthy(y)) y else NULL
}
  