#' Create EpivizChart plot.
#'
#' @param data_object (EpivzData)
#' @param datasource_name (character)
#' @param datasource_origin_name (character)
#' @param data_mgr (EpivizChartDataMgr)
#' @param chart_type (character) BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack
#' @param settings (list) list of settings
#' @param colors (list) list of colors
#' @param ... (type, columns, chr, start, end)
#'
#' @return An object of class \code{\link[epivizrChart]{EpivizChart}}
#'
#' @examples
#' # see package vignette for example usage
#'
#' @export
epivizPlot <- function(
  data_object,
  datasource_name,
  datasource_origin_name=deparse(substitute(data_object)),
  epiviz_env=NULL,
  chart_type=NULL,
  settings=NULL,
  colors=NULL,
  ...
  ) {

  if (is.null(epiviz_env)){
    data_mgr <- EpivizChartDataMgr$new()
  } else {
    data_mgr <- epiviz_env$get_data_mgr()
  }

  chart_obj <- EpivizChart$new(data_mgr=data_mgr, obj=NULL, tag=NULL)

  chart_obj <- chart_obj$plot(data_object, datasource_name, datasource_origin_name,
       chart_type=chart_type, settings=settings, colors=colors, ...)

  if (!is.null(epiviz_env)){
    epiviz_env$append_child(chart_obj)
  }

  return(chart_obj)
}
