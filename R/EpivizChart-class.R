#' Class for knitting epiviz charts
#' 
#' @field mgr  An object of class \code{\link[epivizrChart]{EpivizChartDataMgr}} used to serve data to epiviz chart.
#' @field obj An object of class \code{\link[epivizrData]{EpivizData}}. 
#' @field chart An object of class \code{htmltools}{shiny.tag} representing an epiviz chart in html.
#' @importFrom epivizrServer json_writer
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @import epivizrData
#' @import htmltools
#' @exportClass EpivizChart
EpivizChart <- setRefClass("EpivizChart",
  fields=list(
    mgr="ANY", 
    obj="ANY", 
    chart="ANY"
  ),
  methods=list(
    plot = function(data_object, datasource_name, 
      datasource_origin_name=deparse(substitute(data_object)),
      chart_type=NULL, settings=NULL, colors=NULL, ...) { 
      "Return a shiny.tag representing an epiviz chart and adds it as a child of the epiviz environment tag
      \\describe{
      \\item{data_object}{GenomicRanges object to attach as chart's data}
      \\item{datasource_name}{Name for datasource}
      \\item{chart_type}{Type of chart for plot (BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack)}
      \\item{settings}{List of settings for chart}
      \\item{colors}{List of colors for chart}
      \\item{...}{Type and columns}
      }"
      if (missing(datasource_name)) {
        datasource_name <- datasource_origin_name
      }
      
      ms_obj <- .self$mgr$add_measurements(data_object, datasource_name=datasource_name, 
        datasource_origin_name=datasource_origin_name, ...)
      .self$obj <- ms_obj
      
      chart <- .self$.create_chart_html(ms_obj, settings, colors, chart_type,...)
      .self$chart <- chart

      invisible(.self)
    }, 
    .create_chart_html = function(ms_obj, settings, colors, chart_type, ...) {
      "Creates a shiny.tag representing an epiviz chart
      \\describe{
      \\item{ms_obj}{EpivizData object}
      \\item{settings}{Chart settings}
      \\item{colors}{Chart colors}
      \\item{chart_type}{Chart type for plot (BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack)}}
      }"
      data_json <- ms_obj$toJSON(...)
      
      chart_tag <- .chart_type_to_html_tag(ms_obj, chart_type)
      
      epiviz_chart <- tag(
        chart_tag, 
        list(
          class="charts",
          id=ms_obj$get_id(), 
          measurements=data_json$measurements,
          data=data_json$data,
          settings=settings, 
          colors=colors))
      
      return(epiviz_chart)
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
      return(chart_tag)
    },
    show = function() {
      "Show chart of this object"
      knit_print.shiny.tag(.self$chart)
    }
  )
)
