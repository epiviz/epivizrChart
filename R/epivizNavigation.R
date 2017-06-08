#' Create an \code{\link[epivizrChart]{EpivizNavigation}} object.
#'
#' @param chr (character) chromosome to browse to on app startup.
#' @param start (integer) start location to browse to on app startup.
#' @param end (integer) end location to browse to on app startup.
#' @param data_mgr (EpivizChartDataMgr)
#' @return An object of class \code{\link[epivizrChart]{EpivizNavigation}}
#'
#' @examples
#' # see package vignette for example usage
#' epiviz <- epivizEnvironment(chr="chr11", start=99800000, end=103383180)
#'
#' @export
epivizNavigation <- function(chr="chr11", start=99800000, end=103383180) {
  epiviz_nav <- tag("epiviz-navigation", list(chr=chr, start=start, end=end))

  nav_obj <- EpivizNavigation$new(chr=chr, start=start, end=end, tag=epiviz_nav)

  return(nav_obj)
}
