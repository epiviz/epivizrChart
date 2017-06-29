#' Data container for an epiviz chart.
#'
#' @field mgr  An object of class \code{\link[epivizrChart]{EpivizChartDataMgr}} used to serve data to epiviz chart.
#' @field obj An object of class \code{\link[epivizrData]{EpivizData}}.
#' @import epivizrData
#' @import htmltools
#' @export EpivizChart
#' @exportClass EpivizChart
EpivizChart <- setRefClass("EpivizChart",
  contains="EpivizPolymer",
  fields=list(
    obj="ANY"
  ),
  methods=list(
    initialize = function(data_object, datasource_name, datasource_origin_name,
      epiviz_env=NULL, chart_type=NULL, chr=NULL, start=NULL, end=NULL,
      settings=NULL, colors=NULL, ...) {
      # if an epiviz environment is provided, 
      # use its data manager, chr, start, and end
      if (is.null(epiviz_env)) {
        mgr <- EpivizChartDataMgr()
        
      } else {
        mgr <- epiviz_env$get_data_mgr()
        
        env_chr <- epiviz_env$get_chr()
        if (!is.null(env_chr)) {
          chr <- env_chr
        }
        
        env_start <- epiviz_env$get_start()
        if (!is.null(env_start)) {
          start <- env_start
        }
        
        env_end <- epiviz_env$get_end()
        if (!is.null(env_end)) {
          end <- env_end
        }
      }
      
      # register data
      if (missing(datasource_origin_name)) {
        datasource_origin_name <- deparse(substitute(data_object))
      }
      if (missing(datasource_name)) {
        datasource_name <- datasource_origin_name
      }
      ms_obj <- mgr$add_measurements(data_object, datasource_name=datasource_name,
        datasource_origin_name=datasource_origin_name, ...)
      .self$obj <- ms_obj
      
      # create polymer chart
      epiviz_tag <- .self$.create_chart_html(ms_obj, settings, colors, chart_type, chr, start, end)
      
      # initialize inherited fields (html tag attributes) of epiviz chart
      callSuper(data_mgr=mgr,
        name=epiviz_tag$name,
        class=tagGetAttribute(epiviz_tag, "class"),
        id=tagGetAttribute(epiviz_tag, "id"),
        measurements=tagGetAttribute(epiviz_tag, "measurements"),
        data=tagGetAttribute(epiviz_tag, "data"),
        tag=epiviz_tag)
      
      # if epiviz environment is provided,
      # append this chart to its children 
      if (!is.null(epiviz_env)) {
        epiviz_env$append_child(.self)
      }
      
      invisible(.self)
    },
    .create_chart_html = function(ms_obj, settings, colors, chart_type, chr, start, end) {
      "Creates a shiny.tag representing an epiviz chart
      \\describe{
      \\item{ms_obj}{EpivizData object}
      \\item{settings}{Chart settings}
      \\item{colors}{Chart colors}
      \\item{chart_type}{Chart type for plot (BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack)}}
      }"
      data_json <- ms_obj$toJSON(chr, start, end)

      tag_name <- .chart_type_to_html_tag(ms_obj, chart_type)

      epiviz_tag <- tag(
        tag_name,
        list(
          class="charts",
          id=ms_obj$get_id(),
          measurements=data_json$measurements,
          data=data_json$data,
          settings=settings,
          colors=colors))

      epiviz_tag
    },
    .chart_type_to_html_tag = function(ms_obj, chart_type) {
      "Return an html tag representing an epiviz chart
      \\describe{
      \\item{ms_obj}{EpivizData object}
      \\item{chart_type}{Chart type for plot (BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack)}}
      }"
      if (is.null(chart_type)) {
        chart_tag <- ms_obj$get_default_chart_type_html()
      } else {
        chart_tag <- switch(chart_type,
          BlocksTrack = "epiviz-json-blocks-track",
          HeatmapPlot = "epiviz-json-heatmap-plot",
          LinePlot = "epiviz-json-line-plot",
          LineTrack = "epiviz-json-line-track",
          ScatterPlot = "epiviz-json-scatter-plot",
          StackedLinePlot = "epiviz-json-stacked-line-plot",
          StackedLineTrack = "epiviz-json-stacked-line-track"
        )
      }

      chart_tag
    }
  )
)
