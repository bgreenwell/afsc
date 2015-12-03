#' Career Group
#'
#' Extract the career group indicated by the AFSC.
#'
#' @param x A character vector containing AFSCs.
#' @param allow_X Logical indicating wether or not the \code{"X"} placeholder is
#'   allowed. Default is \code{FALSE}.
#' @export
get_career_group <- function(x, allow_X = FALSE) {
  types <- afsc_type(x, allow_X = allow_X)
  # For valid AFSCsm the first digit shouls be the career group
  ifelse(is.na(types), NA, regmatches(x, regexpr("\\d", x)))
}


#' Shredout
#'
#' Shred different levels.
#'
#' @param x A character vector containing AFSCs.
#' @param allow_X Logical indicating wether or not the \code{"X"} placeholder is
#'   allowed. Default is \code{FALSE}.
#' @export
#' @details For example, \code{level = 1} extracts the career group.
#' @export
shred_afsc <- function(x, level = 1L, allow_X = FALSE) {
  types <- afsc_type(x, allow_X = allow_X)
  ifelse(is.na(types), NA, substr(x, start = 1, stop = level))
}
