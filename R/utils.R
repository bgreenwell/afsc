#' Alphanumeric
#'
#' General test if characters in a string are alphanumeric.
#'
#' @param string An object that inherits from class \code{"character"}.
#' @return Returns \code{TRUE} if all characters in the string are alphanumeric
#'   and there is at least one character, FALSE otherwise.
#' @export
#' @examples
#' is_alnum(c("1", 3, "ab2", "1-s", " 3", "", " ", "a ", "12a", "1.2"))
is_alnum <- function(x) {
  grepl("^[A-Za-z0-9]+$", x)
  # grepl("[:alnum:]", x)
}

