#' JSON parser used by this package
#'
#' Currently this just renames \code{\link{fromJSON}} in the \code{rjson} package.
#' @importFrom rjson fromJSON
#' @export
#'
#' @param json_str json string to parse
#' @param file file to read json_Str from
#' @param method method used to parse json
#' @param unexpected.escape handling escape characters, one of error, skip, keep
#' @return a JSON object
#'
#' @seealso \code{\link{fromJSON}}
#' @examples
#' json_parser('{"a":true, "b":false, "c":null}')

json_parser <- rjson::fromJSON

#' JSON writer used by this package
#'
#' Currently this just renames \code{\link{toJSON}} in the \code{rjson} package.
#' @importFrom rjson toJSON
#' @export
#'
#' @param x object to write to json
#' @param method method used to write json
#' @return a string with JSON encoding of object
#'
#' @seealso \code{\link{toJSON}}
#' @examples
#' json_writer(1:10)
json_writer <- rjson::toJSON

#' HTML dependencies of an EpivizChart
chart_dependencies <- function() {
  deps <- list(
    # TODO: fix version numbers, restructure dependencies
    webcomponents <- htmlDependency(
      name="webcomponents",
      version="1",
      src=system.file(package = "epivizrChart", "www", "lib/webcomponents"),
      #c(href="https://epiviz.github.io/polymer/charts/components/webcomponentsjs"),
      script="webcomponents-lite.js"
    ),
    epiviz_charts <- htmlDependency(
      name="epiviz-charts",
      version="1",
      src=system.file(package = "epivizrChart", "www", "lib/polymer"),
      #c(href="https://epiviz.github.io/polymer"),
      import="epiviz-charts.html"
    )#,
    # epiviz_data_source <- htmlDependency(
    #  name="epiviz-data-source",
    #  version="1",
    #  src=c(href="https://epiviz.github.io/polymer/charts/components/epiviz-data-source"),
    #  import="epiviz-data-source.html"
    # )
  )

  deps
}

#' Get epiviz chart component from data object
#'
#' @param ms_obj data infer chart type from data object
#' @param chart explicitly define chart type
#' @return epiviz chart component tag name
chart_type_to_tag_name = function(ms_obj, chart) {
  if (is.null(chart)) {
    chart_tag <- ms_obj$get_default_chart_type_html()
  } else {
    chart_tag <- switch(chart,
      BlocksTrack = "epiviz-json-blocks-track",
      HeatmapPlot = "epiviz-json-heatmap-plot",
      LinePlot = "epiviz-json-line-plot",
      LineTrack = "epiviz-json-line-track",
      ScatterPlot = "epiviz-json-scatter-plot",
      StackedLinePlot = "epiviz-json-stacked-line-plot",
      StackedLineTrack = "epiviz-json-stacked-line-track",
      stop(chart, " is not a valid chart type. See documentation for supported chart types", call. = FALSE)
    )
  }

  chart_tag
}


