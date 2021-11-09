#' Generic methods to plot charts and add navigation regions
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
#' @examples
#' \dontrun{
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
#' }

setMethod("plot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, ...)
  x
})

#' Method to add Blocks Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' @rdname BlocksTrack-methods
setGeneric("BlocksTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("BlocksTrack") })

#' @rdname BlocksTrack-methods
setMethod("BlocksTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="BlocksTrack", ...)
  x
})

#' Method to add Stacked Blocks Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' @rdname StackedBlocksTrack-methods
setGeneric("StackedBlocksTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("StackedBlocksTrack") })

#' @rdname StackedBlocksTrack-methods
setMethod("StackedBlocksTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="StackedBlocksTrack", ...)
  x
})

#' Method to add Scatter Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' @rdname ScatterPlot-methods
setGeneric("ScatterPlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("ScatterPlot") })

#' @rdname ScatterPlot-methods
setMethod("ScatterPlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="ScatterPlot", ...)
  x
})

#' Method to add Heatmap Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' @rdname HeatmapPlot-methods
setGeneric("HeatmapPlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("HeatmapPlot") })

#' @rdname HeatmapPlot-methods
setMethod("HeatmapPlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="HeatmapPlot", ...)
  x
})

#' Method to add Line Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' @rdname LinePlot-methods
setGeneric("LinePlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("LinePlot") })

#' @rdname LinePlot-methods
setMethod("LinePlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="LinePlot", ...)
  x
})

#' Method to add Stacked Line Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' @rdname StackedLinePlot-methods
setGeneric("StackedLinePlot", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("StackedLinePlot") })

#' @rdname StackedLinePlot-methods
setMethod("StackedLinePlot", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="StackedLinePlot", ...)
  x
})

#' Method to add Line Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' @rdname LineTrack-methods
setGeneric("LineTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("LineTrack") })

#' @rdname LineTrack-methods
setMethod("LineTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="LineTrack", ...)
  x
})

#' Method to add Stacked Line Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @rdname StackedLineTrack-methods
#' @export
setGeneric("StackedLineTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("StackedLineTrack") })

#' @rdname StackedLineTrack-methods
setMethod("StackedLineTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="StackedLineTrack", ...)
  x
})

#' Method to add Multi Stacked Line Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @rdname MultiStackedLineTrack-methods
#' @export
setGeneric("MultiStackedLineTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("MultiStackedLineTrack") })

#' @rdname MultiStackedLineTrack-methods
setMethod("MultiStackedLineTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="MultiStackedLineTrack", ...)
  x
})

#' Method to add Transcript Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @rdname TranscriptTrack-methods
#' @export
setGeneric("TranscriptTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("TranscriptTrack") })

#' @rdname TranscriptTrack-methods
setMethod("TranscriptTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="TranscriptTrack", ...)
  x
})

#' Method to add Guide Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @rdname GuideTrack-methods
#' @export
setGeneric("GuideTrack", signature = c("x", "x"), 
           function(x, y, ...) { standardGeneric("GuideTrack") })

#' @rdname GuideTrack-methods
setMethod("GuideTrack", signature = c("EpivizEnvironment", "ANY"), function(x, y, ...) {
  x$plot(y, chart="GuideTrack", ...)
  x
})


#' Generic method to add navigation regions
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}}
#' @param chr chromosome id. ex. "chr11"
#' @param start genomic region start
#' @param end genomic region end
#' @param ... other parameters
#' @param return_parent To return the parent or the new navigation element. Defaults to FALSE
#' @rdname append_region-methods
#' @export
setGeneric("append_region", signature = "x", 
           function(x, ...) standardGeneric("append_region")) 

#' @rdname append_region-methods
setMethod("append_region", 
          signature = "EpivizEnvironment",
          function(x, chr, start, end, return_parent=FALSE) {
            child <- x$append_region(chr, start, end)
            if(return_parent) {
              return(x)
            }
            child
          })
