#' Generic methods to plot charts and add navigation regions
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
#' @examples
#' library(epivizrChart)
#' library(Homo.sapiens)
#' require(magrittr)
#' 
#' # example data set
#' data(sumexp)
#' # create an environment element
#' epivizEnv <- epivizEnv()
#' 
#' # chain and add navigation regions and plots.
#' epivizEnv %>% 
#' plot(sumexp, datasource_name="sumExp", columns=c("cancer", "normal")) %>% 
#' append_region(chr="chr11", start=118000000, end=121000000) %>% 
#' plot(sumexp, datasource_name="sumExp", columns=c("normal", "cancer"))
#' epivizEnv

setMethod("plot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, ...)
  x
})

#' Generic method to  add navigation regions
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}}
#' @param ... other parameters - genomic locations -  chr, start and end.
#' @export
#' 
setGeneric("append_region", signature = "x", 
          function(x, ...) standardGeneric("append_region")) 

#' Generic method to  add navigation regions
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}}
#' @param chr chromosome id. ex. "chr11"
#' @param start genomic region start
#' @param end genomic region end
#' @param return_parent To return the parent or the new navigation element. Defaults to FALSE
#' @export
#' 
setMethod("append_region", 
          signature = "EpivizEnvironment",
          function(x, chr, start, end, return_parent=FALSE) {
  child <- x$append_region(chr, start, end)
  if(return_parent) {
    return(x)
  }
  child
})
