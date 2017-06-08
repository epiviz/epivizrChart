#' Create an \code{\link[epivizrChart]{EpivizEnvironment}} object.
#'
#' @param chr (character) chromosome to browse to on app startup.
#' @param start (integer) start location to browse to on app startup.
#' @param end (integer) end location to browse to on app startup.
#' @param data_mgr (EpivizChartDataMgr)
#' @return An object of class \code{\link[epivizrChart]{EpivizEnvironment}}
#'
#' @examples
#' # see package vignette for example usage
#' epiviz <- epivizEnvironment(chr="chr11", start=99800000, end=103383180)
#'
#' @export
epivizEnvironment <- function(chr=NULL, start=NULL, end=NULL) {
  epiviz_env <- tag("epiviz-environment", list(chr=chr, start=start, end=end))

  env_obj <- EpivizEnvironment$new(chr=chr, start=start, end=end, tag=epiviz_env)

  return(env_obj)
}
