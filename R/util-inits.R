#' Initialize an \code{\link[epivizrChart]{EpivizChart}} object to visualize in viewer or knit to HTML.
#'
#' @param data_obj A data object that will register to an \code{\link[epivizrData]{EpivizData}} object.
#' @param measurements An \code{\link[epivizrData]{EpivizMeasurement}} object.
#' @param datasource_name A name for the datasource. For example, "Mean by Sample Type".
#' @param parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} or \code{\link[epivizrChart]{EpivizNavigation}} to append the chart within.
#' @param chart The chart type to be visualized: "BlocksTrack", HeatmapPlot", "LinePlot", "LineTrack", "ScatterPlot", "StackedLinePlot", "StackedLineTrack".
#' @param chr The chromosome to filter on, e.g., chr="chr11".
#' @param start The start location, e.g., start=110800000.
#' @param end The end location, e.g., end=130383180.
#' @param settings List of settings, e.g., list(tite="Blocks Chart"). When chart is rendered to html this will be converted to a string encoded as JSON.
#' @param colors List of colors. When chart is rendered to html this will be converted to a string encoded as JSON
#' @param ... Additional arguments passed to \code{\link[epivizrData]{register}}, e.g., type="bp", columns=c("normal, cancer").
#' @return An object of class \code{\link[epivizrChart]{EpivizChart}}.
#'
#' @examples
#' # See package vignette for example usage.
#'
#' @export
epivizChart <- function(data_obj=NULL, datasource_name=NULL, parent=NULL,
  measurements=NULL, chart=NULL, chr=NULL, start=NULL, end=NULL,
  settings=NULL, colors=NULL, ...) {

  EpivizChart(data_obj=data_obj, datasource_name=datasource_name,
    parent=parent, measurements=measurements, chart=chart, chr=chr,
    start=start, end=end, settings=settings, colors=colors, ...)
}

#' Initialize an \code{\link[epivizrChart]{EpivizNavigation}} object to visualize in viewer or knit to HTML.
#'
#' @param chr The chromosome to filter on, e.g., chr="chr11".
#' @param start The start location, e.g., start=99800000.
#' @param end The end location, e.g., end=130383180.
#' @param gene TODO
#' @param geneInRange TODO
#' @param parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} or \code{\link[epivizrChart]{EpivizNavigation}} to append the chart within.
#' @param ... Additional arguments passed to \code{\link[epivizrChart]{EpivizPolymer}}.
#' @return An object of class \code{\link[epivizrChart]{EpivizNavigation}}.
#'
#' @examples
#' epiviz <- epivizNav(chr="chr11", start=99800000, end=103383180)
#'
#' @export
epivizNav <- function(chr=NULL, start=NULL, end=NULL, gene=NULL,
  geneInRange=NULL, parent=NULL, ...) {

  EpivizNavigation(chr=chr, start=start, end=end, gene=gene,
    geneInRange=geneInRange, parent=parent, ...)
}


#' Initialize an \code{\link[epivizrChart]{EpivizEnvironment}} object.
#'
#' @param chr The chromosome to filter on, e.g., chr="chr11"
#' @param start The start location, e.g., start=99800000.
#' @param end The end location, e.g., end=130383180.
#' @param initializeRegions List of gene names or regions to intialize navigations.
#' @param ... Additional params to pass to \code{\link[epivizrChart]{EpivizPolymer}}
#' @return An object of class \code{\link[epivizrChart]{EpivizEnvironment}}
#'
#' @examples
#' epiviz <- epivizEnv(chr="chr11", start=99800000, end=103383180)
#'
#' @export
epivizEnv <- function(chr=NULL, start=NULL, end=NULL,
  initializeRegions=NULL, ...) {

  EpivizEnvironment(chr=chr, start=start, end=end,
    initializeRegions=initializeRegions, ...)
}
