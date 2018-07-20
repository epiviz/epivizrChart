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

GenericAddChart <- function(x, y, chart, ...) {
  
  # if x is other bioconductor data objects
  if(class(x) %in% get_registered_data_types()) {
    chart <- epivizChart(x, y, chart=chart, ...)
    return(chart)
  }
  
  stop("object is not a registered data type.")
}

GenericEnvAddChart <- function(x, y, chart, ...) {
  
  # if x is environment or navigation
  if(class(x) %in% c("EpivizEnvironment", "EpivizNavigation")) {
    x$plot(y, chart=chart, ...)
    return(x)
  }
}

GenericEnvBlocksTrack <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="BlocksTrack", ...)
}

GenericEnvStackedBlocksTrack <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="StackedBlocksTrack", ...)
}

GenericEnvScatterPlot <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="ScatterPlot", ...)
}

GenericEnvHeatmapPlot <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="HeatmapPlot", ...)
}

GenericEnvLinePlot <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="LinePlot", ...)
}

GenericEnvStackedLinePlot <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="StackedLinePlot", ...)
}

GenericEnvLineTrack <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="LineTrack", ...)
}

GenericEnvStackedLineTrack <- function(x, y, ...) {
  GenericEnvAddChart(x, y, chart="StackedLineTrack", ...)
}

GenericBlocksTrack <- function(x, y, ...) {
  GenericAddChart(x, y, chart="BlocksTrack", ...)
}

GenericStackedBlocksTrack <- function(x, y, ...) {
  GenericAddChart(x, y, chart="StackedBlocksTrack", ...)
}

GenericScatterPlot <- function(x, y, ...) {
  GenericAddChart(x, y, chart="ScatterPlot", ...)
}

GenericHeatmapPlot <- function(x, y, ...) {
  GenericAddChart(x, y, chart="HeatmapPlot", ...)
}

GenericLinePlot <- function(x, y, ...) {
  GenericAddChart(x, y, chart="LinePlot", ...)
}

GenericStackedLinePlot <- function(x, y, ...) {
  GenericAddChart(x, y, chart="StackedLinePlot", ...)
}

GenericLineTrack <- function(x, y, ...) {
  GenericAddChart(x, y, chart="LineTrack", ...)
}

GenericStackedLineTrack <- function(x, y, ...) {
  GenericAddChart(x, y, chart="StackedLineTrack", ...)
}

setGeneric("BlocksTrack", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("BlocksTrack") })

#' Method to add Blocks Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("BlocksTrack", signature = c("EpivizEnvironment", "ANY"), GenericEnvBlocksTrack)
setMethod("BlocksTrack", signature = c("ANY", "missing"), GenericBlocksTrack)

setGeneric("StackedBlocksTrack", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("StackedBlocksTrack") })

#' Method to add Stacked Blocks Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("StackedBlocksTrack", signature = c("ANY", "ANY"), GenericStackedBlocksTrack)

setGeneric("ScatterPlot", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("ScatterPlot") })

#' Method to add Scatter Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("ScatterPlot", signature = c("EpivizEnvironment", "ANY"), GenericEnvScatterPlot)
setMethod("ScatterPlot", signature = c("ANY", "missing"), GenericScatterPlot)


setGeneric("HeatmapPlot", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("HeatmapPlot") })

#' Method to add Heatmap Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("HeatmapPlot", signature = c("EpivizEnvironment", "ANY"), GenericEnvHeatmapPlot)
setMethod("HeatmapPlot", signature = c("ANY", "missing"), GenericHeatmapPlot)

setGeneric("LinePlot", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("LinePlot") })

#' Method to add Line Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("LinePlot", signature = c("EpivizEnvironment", "ANY"), GenericEnvLinePlot)
setMethod("LinePlot", signature = c("ANY", "missing"), GenericLinePlot)

setGeneric("StackedLinePlot", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("StackedLinePlot") })

#' Method to add Stacked Line Plot
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("StackedLinePlot", signature = c("EpivizEnvironment", "ANY"), GenericEnvStackedLinePlot)
setMethod("StackedLinePlot", signature = c("ANY", "missing"), GenericStackedLinePlot)

setGeneric("LineTrack", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("LineTrack") })

#' Method to add Line Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("LineTrack", signature = c("EpivizEnvironment", "ANY"), GenericEnvLineTrack)
setMethod("LineTrack", signature = c("ANY", "missing"), GenericLineTrack)

setGeneric("StackedLineTrack", signature = c("x", "y"), 
           function(x, y, ...) { standardGeneric("StackedLineTrack") })

#' Method to add Stacked Line Track
#' 
#' @param x an object of type \code{\link{EpivizEnvironment}} or \code{\link{EpivizNavigation}}
#' @param y a genomic data object
#' @param ... other parameters for the plot method
#' @export
#' 
setMethod("StackedLineTrack", signature = c("EpivizEnvironment", "ANY"), GenericEnvStackedLineTrack)
setMethod("StackedLineTrack", signature = c("ANY", "missing"), GenericStackedLineTrack)

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