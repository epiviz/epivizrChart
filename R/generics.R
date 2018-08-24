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

setGeneric("BlocksTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("BlocksTrack") })

#' Method to add Blocks Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("BlocksTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="BlocksTrack", ...)
  x
})

setGeneric("StackedBlocksTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("StackedBlocksTrack") })

#' Method to add Stacked Blocks Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("StackedBlocksTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="StackedBlocksTrack", ...)
  x
})

setGeneric("ScatterPlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("ScatterPlot") })

#' Method to add Scatter Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("ScatterPlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="ScatterPlot", ...)
  x
})

setGeneric("HeatmapPlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("HeatmapPlot") })

#' Method to add Heatmap Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("HeatmapPlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="HeatmapPlot", ...)
  x
})

setGeneric("LinePlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("LinePlot") })

#' Method to add Line Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("LinePlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="LinePlot", ...)
  x
})

setGeneric("StackedLinePlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("StackedLinePlot") })

#' Method to add Stacked Line Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("StackedLinePlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="StackedLinePlot", ...)
  x
})

setGeneric("LineTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("LineTrack") })

#' Method to add Line Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("LineTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="LineTrack", ...)
  x
})

setGeneric("StackedLineTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("StackedLineTrack") })

#' Method to add Stacked Line Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("StackedLineTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="StackedLineTrack", ...)
  x
})

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
