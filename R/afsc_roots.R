#' Air Force Specialty Code (AFSC) Prefixes/Suffixes
#'
#' Extract or strip off AFSC prefixes and suffixes (if any).
#'
#' @param x A character vector containing AFSCs.
#' @param allow_X Logical indicating whether or not the "X" placeholder is allowed. Default is \code{FALSE}.
#' @return \code{strip_roots} returns the original vector of AFSCs with all of
#'   the prefixes and suffixes (if any) removed. The functions \code{get_prefix} and
#'   \code{get_suffix} return, respectively, the prefix and suffix (if any) from
#'   each AFSC.
#' @rdname afsc_roots
#' @export
#' @examples
#' afscs <- c("9T000", "X1N371E", "123ABC")
#' strip_roots(afscs)
#' get_prefix(afscs)
#' get_suffix(afscs)
strip_roots <- function(x, allow_X = allow_X) {

  # Logical vectors indicating which elements contain a prefix/suffix (if any)
  invalid <- !is_valid_afsc(x, allow_X = allow_X)
  prefixes <- grepl("^[a-zA-Z]", x)
  suffixes <- grepl("[a-zA-Z]$", x)

  # Strip prefixes
  if (any(prefixes)) {
    x[prefixes] <- substr(x[prefixes], start = 2, stop = nchar(x[prefixes]))
  }

  # Strip suffixes
  if (any(suffixes)) {
    x[suffixes] <- substr(x[suffixes], start = 1, stop = nchar(x[suffixes]) - 1)
  }

  # Return value
  x[invalid] <- NA  # FIXME: Should strip_roots return NA or original element?
  x

}


#' @rdname afsc_roots
#' @export
get_prefix <- function(x) {
  invalid <- !is_valid_afsc(x, allow_X = allow_X)
  prefixes <- grepl("^[a-zA-Z]", x)
  x[prefixes] <- substr(x[prefixes], start = 1, stop = 1)
  x[!prefixes] <- "none"
  x[invalid] <- NA
  x
}


#' @rdname afsc_roots
#' @export
get_suffix <- function(x) {
  invalid <- !is_valid_afsc(x, allow_X = allow_X)
  suffixes <- grepl("[a-zA-Z]$", x)
  x[suffixes] <- substr(x[suffixes], start = nchar(x[suffixes]),
                        stop = nchar(x[suffixes]))
  x[!suffixes] <- "none"
  x[invalid] <- NA
  x
}
