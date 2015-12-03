#' Officer/Enlisted Air Force Specialty Codes (AFSCs)
#'
#' Determine which AFSCs are officer and which are enlisted.
#'
#' @param x A character vector containing AFSCs.
#' @param allow_X Logical indicating wether or not the \code{"X"} placeholder is
#'   allowed. Default is \code{FALSE}.
#' @return \code{afsc_type} returns the AFSC type (i.e., either \code{"officer"}
#'   or \code{"enlisted"}). The functions \code{is_enlisted} and
#'   \code{is_officer} are logical tests.
#' @rdname afsc_type
#' @export
#' @examples
#' afscs <- c("9T000", "92T0", "A9T0X0K")
#' afsc_type(afscs)
#' is_enlisted(afscs, allow_X = TRUE)
#' is_officer(afscs)
afsc_type <- function(x, allow_X = FALSE) {
  ifelse(is_enlisted(x, allow_X = allow_X), "enlisted",
         ifelse(is_officer(x, allow_X = allow_X), "officer", NA))
}

#' @rdname afsc_type
#' @export
is_enlisted <- function(x, allow_X = FALSE) {
  if (allow_X) {
    grepl("^[A-Z]?[1-9][A-Z][0-9][013579?X][0-9][A-Z]?$", x)
  } else {
    grepl("^[A-Z]?[1-9][A-Z][0-9][013579][0-9][A-Z]?$", x)
  }
}


#' @rdname afsc_type
#' @export
is_officer <- function(x, allow_X = FALSE) {
  if (allow_X) {
    grepl("^[A-Z]?[1-9][0-9][A-Z][0-9?X][A-Z]?$", x)
  } else {
    grepl("^[A-Z]?[1-9][0-9][A-Z][0-9][A-Z]?$", x)
  }
}

