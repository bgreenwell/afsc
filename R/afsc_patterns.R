#' Unique Patterns
#'
#' Extract unique patterns from a character vector.
#'
#' @param x A character vector.
#' @param show_ws Logical indicating whether or not to display whitespace.
#' @param show_ws_as Character used to indicate whitespace. Defaults to "w".
#' @export
unique_patterns <- function(x, show_ws = TRUE, show_ws_as = "w",
                            as_table = FALSE) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  x <- gsub("[a-z]", "a", x)
  x <- gsub("[A-Z]", "A", x)
  x <- gsub("[0-9]", "9", x)
  if (show_ws) {
    x <- gsub("\\s", show_ws_as, x)
  }
  if(as_table) {
    dtab <- as.data.frame(sort(table(x)))
    names(dtab) <- "frequency"
    dtab
  } else {
    unique(x)
  }
}
