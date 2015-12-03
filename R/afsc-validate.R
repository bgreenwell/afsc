#' Valid/Invalid Air Force Specialty Codes (AFSCs)
#'
#' Determine which AFSCs are valid or invalid.
#'
#' @rdname afsc_validate
#' @param x A character vector containing AFSCs.
#' @param allow_X Logical indicating wether or not the \code{"X"} placeholder is
#'   allowed. Default is \code{FALSE}.
#' @param unique_only Logical indicating wether or not only the unique AFSCs
#'   should be returned. Default is \code{FALSE}.
#' @return Returns a logical vector of the same length as its argument \code{x},
#'   containing \code{TRUE} for those elements that are considered as valid
#'   AFSCs, and \code{FALSE} otherwise.
#' @export
is_valid_afsc <- function(x, allow_X = FALSE) {
  !is.na(afsc_type(x, allow_X = allow_X))
}


#' @rdname afsc_validate
#' @export
get_invalid_afscs <- function(x, allow_X = FALSE, unique_only = FALSE) {
  invalid <- x[!is_valid_afsc(x, allow_X = allow_X)]
  if (unique_only) {
    unique(invalid)
  } else {
    invalid
  }
}


#' @rdname afsc_validate
#' @export
get_valid_afscs <- function(x, allow_X = FALSE, unique_only = FALSE) {
  valid <- x[is_valid_afsc(x, allow_X = allow_X)]
  if (unique_only) {
    unique(valid)
  } else {
    valid
  }
}
