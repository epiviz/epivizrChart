#' Create \code{\link{EpivizChart}} object.
#' @param data_object (EpivzData)
#' @param datasource_name (character) 
#' @param datasource_origin_name (character) 
#' @param data_mgr (EpivizChartDataMgr) 
#' @param chart_type (character) BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack
#' @param settings (list) list of settings
#' @param colors (list) list of colors
#' @param ... (type, columns, chr, start, end)
#' 
#' @return An object of class \code{\link{EpivizChart}}
#'
#' @examples
#' # see package vignette for example usage
#' 
#' @export
epivizPlot <- function(data_object, datasource_name, 
  datasource_origin_name=deparse(substitute(data_object)),
  data_mgr=EpivizChartDataMgr$new(),
  chart_type=NULL, settings=NULL, colors=NULL, ...) { 
  
  chart_obj <- (EpivizChart$new(mgr=data_mgr, obj=NULL, chart=NULL)
  
  chart <- chart_obj$plot(data_object, datasource_name, datasource_origin_name, 
       chart_type=chart_type, settings=settings, colors=colors, ...)
  
  return(chart)
}