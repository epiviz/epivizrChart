#' Create \code{\link{EpivizPolymer}} object to knit epiviz charts.
#' @param chr (character) chromosome to browse to on app startup.
#' @param start (integer) start location to browse to on app startup.
#' @param end (integer) end location to browse to on app startup.
#' @return An object of class \code{\link{EpivizPolymer}}
#' 
#' @examples
#' # see package vignete for example usage
#' epiviz <- epivizEnvironment(chr="chr11", start=99800000, end=103383180)
#' 
#' @export
epivizEnvironment <- function(chr="chr11", start=99800000, end=103383180) {
  server <- epivizrServer::createServer(non_interactive=TRUE)
  data_mgr <- epivizrData::createMgr(server)
  epivizEnvir <- htmltools::tag("epiviz-environment" , list(chr=chr, start=start, end=end))
  epiviz <- EpivizPolymer$new(chr=chr, start=start, end=end,
    data_mgr=data_mgr, epivizEnvir=epivizEnvir)
  
  return(epiviz)
}