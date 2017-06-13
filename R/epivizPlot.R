#' Create EpivizChart plot.
#'
#' @param data_object (EpivzData)
#' @param datasource_name (character)
#' @param datasource_origin_name (character)
#' @param data_mgr (EpivizChartDataMgr)
#' @param chart_type (character) BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack
#' @param settings (list) list of settings
#' @param colors (list) list of colors
#' @param chr (character)
#' @param start (numeric)
#' @param end (numeric)
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
  chr=NULL,
  start=NULL,
  end=NULL,
  settings=NULL,
  colors=NULL,
  ...
  ) {

  if (is.null(epiviz_env)){
    data_mgr <- EpivizChartDataMgr$new()

  } else {
    data_mgr <- epiviz_env$get_data_mgr()

    env_chr <- epiviz_env$get_chr()
    if (!is.null(env_chr)){
      chr <- env_chr
    }

    env_start <- epiviz_env$get_start()
    if (!is.null(env_start)){
      start <- env_start
    }

    env_end <- epiviz_env$get_end()
    if (!is.null(env_end)){
      end <- env_end
    }
  }

  chart_obj <- EpivizChart$new(data_mgr=data_mgr, obj=NULL, tag=NULL)

  chart_obj <- chart_obj$plot(data_object, datasource_name, datasource_origin_name,
       chart_type=chart_type, settings=settings, colors=colors, chr, start, end, ...)

  if (!is.null(epiviz_env)) {
    epiviz_env$append_child(chart_obj)
  }

  return(chart_obj)
}
