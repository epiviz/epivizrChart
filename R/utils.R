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
#' @param knitr whether knitr is in progress
#' @return list of polymer dependencies
chart_dependencies <- function(knitr=FALSE) {
  
  polymer_lib = system.file(package = "epivizrChart", "www", "lib/polymer/", "epiviz-charts.html")
  
  if(!knitr) {
    polymer_lib = "lib/epiviz-charts-1/epiviz-charts.html"
  }
  
  deps <- list(
    webcomponents <- htmlDependency(
      name="webcomponents",
      version="0.7.24",
      src=system.file(package = "epivizrChart", "www", "lib/webcomponents"),
      script="webcomponents-lite.js"
    ),
    epiviz_charts <- htmlDependency(
      name="epiviz-charts",
      version="1",
      head = paste0("<link rel='import' href='",  polymer_lib, "'>"),
      src=system.file(package = "epivizrChart", "www", "lib/polymer"),
      all_files=TRUE
    )
  )

  deps
}

#' Get epiviz chart component from data object
#'
#' @param ms_obj data infer chart type from data object
#' @param chart explicitly define chart type
#' @return epiviz chart component tag name
#' 
#' @importFrom methods is
chart_type_to_tag_name <- function(ms_obj, chart) {
  if (is.null(chart)) {
    if (!is(ms_obj, "EpivizData"))
      stop(ms_obj, " must be of class EpivizData")

    chart_tag <- ms_obj$get_default_chart_type_html()
  } else {
    chart_tag <- switch(chart,
      GenesTrack = "epiviz-json-genes-track",
      BlocksTrack = "epiviz-json-blocks-track",
      HeatmapPlot = "epiviz-json-heatmap-plot",
      LinePlot = "epiviz-json-line-plot",
      LineTrack = "epiviz-json-line-track",
      ScatterPlot = "epiviz-json-scatter-plot",
      StackedLinePlot = "epiviz-json-stacked-line-plot",
      StackedLineTrack = "epiviz-json-stacked-line-track",
      stop(chart, " is not a valid chart type. See documentation for supported chart types")
    )
  }

  chart_tag
}


#' Get default chart settings and colors
#' @param json_chart_type chart type
#' @return list of settings and colors
#' 
chart_default_settings_colors <- function(json_chart_type) {
  # if (!is.null(ms_obj)) {
  #   chart_type <- ms_obj$get_default_chart_type_html()
  # }
  
  chart_settings <- switch(json_chart_type,
                           "epiviz-json-genes-track" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=25,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=23,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=20,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=10,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(
                               "#f9a65a",
                               "#599ad3",
                               "#79c36a",
                               "#f1595f",
                               "#727272",
                               "#cd7058",
                               "#d77fb3"
                             )
                           ),
                           "epiviz-json-blocks-track" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=25,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=23,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=20,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=10,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="minBlockDistance",
                                 "type"="number",
                                 "defaultValue"=5,
                                 "label"="Minimum block distance",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="useColorBy",
                                 "type"="boolean",
                                 "defaultValue"=FALSE,
                                 "label"="Use Block Color by",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="blockColorBy",
                                 "type"="measurementsMetadata",
                                 "defaultValue"="colLabel",
                                 "label"="Block color by",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(                               
                               "#f9a65a",
                               "#599ad3",
                               "#79c36a",
                               "#f1595f",
                               "#727272",
                               "#cd7058",
                               "#d77fb3"
                              )
                           ),
                           "epiviz-json-heatmap-plot" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=80,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=40,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=120,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=40,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="measurementGroupsAggregator",
                                 "type"="categorical",
                                 "defaultValue"="mean-stdev",
                                 "label"="Aggregator for measurement groups",
                                 "possibleValues"=c("mean-stdev", "quartiles", "count", "min", "max", "sum")
                               ),
                               list(
                                 "id"="colLabel",
                                 "type"="measurementsMetadata",
                                 "defaultValue"="colLabel",
                                 "label"="Columns labels",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="rowLabel",
                                 "type"="measurementsAnnotation",
                                 "defaultValue"="name",
                                 "label"="Row labels",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="showColorsForRowLabels",
                                 "type"="boolean",
                                 "defaultValue"=FALSE,
                                 "label"="Row labels as colors",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="maxColumns",
                                 "type"="number",
                                 "defaultValue"=40,
                                 "label"="Max columns",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMin",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Min Value",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMax",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Max Value",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="cluster",
                                 "type"="categorical",
                                 "defaultValue"="rows",
                                 "label"="Cluster",
                                 "possibleValues"=c("none", "rows", "columns", "both")
                               ),
                               list(
                                 "id"="clusteringAlg",
                                 "type"="categorical",
                                 "defaultValue"="none",
                                 "label"="Clustering Algorithm",
                                 "possibleValues"=c("none", "agglomerative")
                               ),
                               list(
                                 "id"="clusteringMetric",
                                 "type"="categorical",
                                 "defaultValue"="euclidean",
                                 "label"="Clustering Metric",
                                 "possibleValues"=c("euclidean")
                               ),
                               list(
                                 "id"="clusteringLinkage",
                                 "type"="categorical",
                                 "defaultValue"="complete",
                                 "label"="Clustering Linkage",
                                 "possibleValues"=c("complete")
                               ),
                               list(
                                 "id"="showDendrogram",
                                 "type"="boolean",
                                 "defaultValue"=TRUE,
                                 "label"="Show Dendrogram",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(
                               "#1859a9",
                               "#ed2d2e",
                               "#008c47",
                               "#010101",
                               "#f37d22",
                               "#662c91",
                               "#a11d20",
                               "#b33893"
                             )
                           ),
                           "epiviz-json-line-track" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=25,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=23,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=20,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=10,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="measurementGroupsAggregator",
                                 "type"="categorical",
                                 "defaultValue"="mean-stdev",
                                 "label"="Aggregator for measurement groups",
                                 "possibleValues"=c("mean-stdev", "quartiles", "count", "min", "max", "sum")
                               ),
                               list(
                                 "id"="step",
                                 "type"="number",
                                 "defaultValue"=50,
                                 "label"="Step",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="showPoints",
                                 "type"="boolean",
                                 "defaultValue"=FALSE,
                                 "label"="Show points",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="showLines",
                                 "type"="boolean",
                                 "defaultValue"=TRUE,
                                 "label"="Show lines",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="showErrorBars",
                                 "type"="boolean",
                                 "defaultValue"=TRUE,
                                 "label"="Show error bars",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="pointRadius",
                                 "type"="number",
                                 "defaultValue"=1,
                                 "label"="Point radius",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="lineThickness",
                                 "type"="number",
                                 "defaultValue"=1,
                                 "label"="Line thickness",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMin",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Min Y",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMax",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Max Y",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="interpolation",
                                 "type"="categorical",
                                 "defaultValue"="linear",
                                 "label"="Interpolation",
                                 "possibleValues"=c("linear", "step-before", "step-after", "basis", "basis-open", "basis-closed", "bundle", "cardinal", "cardinal-open", "monotone")
                               ),
                               list(
                                 "id"="abLine",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Draw abline",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(
                               "#1859a9",
                               "#ed2d2e",
                               "#008c47",
                               "#010101",
                               "#f37d22",
                               "#662c91",
                               "#a11d20",
                               "#b33893"
                             )
                           ),
                           "epiviz-json-line-plot" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=30,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=50,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=30,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=15,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="measurementGroupsAggregator",
                                 "type"="categorical",
                                 "defaultValue"="mean-stdev",
                                 "label"="Aggregator for measurement groups",
                                 "possibleValues"=c("mean-stdev", "quartiles", "count", "min", "max", "sum")
                               ),
                               list(
                                 "id"="colLabel",
                                 "type"="measurementsMetadata",
                                 "defaultValue"="colLabel",
                                 "label"="Columns labels",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="rowLabel",
                                 "type"="measurementsAnnotation",
                                 "defaultValue"="name",
                                 "label"="Row labels",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="showPoints",
                                 "type"="boolean",
                                 "defaultValue"=FALSE,
                                 "label"="Show points",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="showLines",
                                 "type"="boolean",
                                 "defaultValue"=TRUE,
                                 "label"="Show lines",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="showErrorBars",
                                 "type"="boolean",
                                 "defaultValue"=TRUE,
                                 "label"="Show error bars",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="pointRadius",
                                 "type"="number",
                                 "defaultValue"=4,
                                 "label"="Point radius",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="lineThickness",
                                 "type"="number",
                                 "defaultValue"=3,
                                 "label"="Line thickness",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMin",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Min Y",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMax",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Max Y",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="interpolation",
                                 "type"="categorical",
                                 "defaultValue"="basis",
                                 "label"="Interpolation",
                                 "possibleValues"=c("linear", "step-before", "step-after", "basis", "basis-open", "basis-closed", "bundle", "cardinal", "cardinal-open", "monotone")
                               ),
                               list(
                                 "id"="abLine",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Draw abline",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(
                               "#393b79",
                               "#5254a3",
                               "#6b6ecf",
                               "#9c9ede",
                               "#637939",
                               "#8ca252",
                               "#b5cf6b",
                               "#cedb9c",
                               "#8c6d31",
                               "#bd9e39",
                               "#e7ba52",
                               "#e7cb94",
                               "#843c39",
                               "#ad494a",
                               "#d6616b",
                               "#e7969c",
                               "#7b4173",
                               "#a55194",
                               "#ce6dbd",
                               "#de9ed6"
                             )
                           ),
                           "epiviz-json-scatter-plot" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=15,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=50,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=50,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=15,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="measurementGroupsAggregator",
                                 "type"="categorical",
                                 "defaultValue"="mean-stdev",
                                 "label"="Aggregator for measurement groups",
                                 "possibleValues"=c("mean-stdev", "quartiles", "count", "min", "max", "sum")
                               ),
                               list(
                                 "id"="circleRadiusRatio",
                                 "type"="number",
                                 "defaultValue"=0.015,
                                 "label"="Circle radius ratio",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="xMin",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Min X",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="xMax",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Max X",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMin",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Min Y",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="yMax",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Max Y",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="abLine",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Draw abline",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(
                               "#1f77b4",
                               "#ff7f0e",
                               "#2ca02c",
                               "#d62728",
                               "#9467bd",
                               "#8c564b",
                               "#e377c2",
                               "#7f7f7f",
                               "#bcbd22",
                               "#17becf"
                             )
                           ),
                           "epiviz-json-stacked-line-plot" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=30,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=50,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=30,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=15,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="measurementGroupsAggregator",
                                 "type"="categorical",
                                 "defaultValue"="mean-stdev",
                                 "label"="Aggregator for measurement groups",
                                 "possibleValues"=c("mean-stdev", "quartiles", "count", "min", "max", "sum")
                               ),
                               list(
                                 "id"="colLabel",
                                 "type"="measurementsMetadata",
                                 "defaultValue"="colLabel",
                                 "label"="Color by",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="rowLabel",
                                 "type"="measurementsAnnotation",
                                 "defaultValue"="name",
                                 "label"="Labels",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="offset",
                                 "type"="categorical",
                                 "defaultValue"="zero",
                                 "label"="Offset",
                                 "possibleValues"=c("zero", "wiggle")
                               ),
                               list(
                                 "id"="interpolation",
                                 "type"="categorical",
                                 "defaultValue"="step-after",
                                 "label"="Interpolation",
                                 "possibleValues"=c("linear", "step-before", "step-after", "basis", "basis-open", "basis-closed", "bundle", "cardinal", "cardinal-open", "monotone")
                               ),
                               list(
                                 "id"="scaleToPercent",
                                 "type"="boolean",
                                 "defaultValue"=TRUE,
                                 "label"="Scale to Percent",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="useGroupBy",
                                 "type"="boolean",
                                 "defaultValue"=FALSE,
                                 "label"="Use Group by",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="groupBy",
                                 "type"="measurementsAnnotation",
                                 "defaultValue"="name",
                                 "label"="Group By",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="hoverOpacity",
                                 "type"="number",
                                 "defaultValue"=0.6,
                                 "label"="Hover Opacity",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(
                               "#393b79",
                               "#5254a3",
                               "#6b6ecf",
                               "#9c9ede",
                               "#637939",
                               "#8ca252",
                               "#b5cf6b",
                               "#cedb9c",
                               "#8c6d31",
                               "#bd9e39",
                               "#e7ba52",
                               "#e7cb94",
                               "#843c39",
                               "#ad494a",
                               "#d6616b",
                               "#e7969c",
                               "#7b4173",
                               "#a55194",
                               "#ce6dbd",
                               "#de9ed6"
                             )
                           ),
                           "epiviz-json-stacked-line-track" = list(
                             settings = list(
                               list(
                                 "id"="title",
                                 "type"="string",
                                 "defaultValue"="",
                                 "label"="Title",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginTop",
                                 "type"="number",
                                 "defaultValue"=25,
                                 "label"="Top margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginBottom",
                                 "type"="number",
                                 "defaultValue"=23,
                                 "label"="Bottom margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginLeft",
                                 "type"="number",
                                 "defaultValue"=20,
                                 "label"="Left margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="marginRight",
                                 "type"="number",
                                 "defaultValue"=10,
                                 "label"="Right margin",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="measurementGroupsAggregator",
                                 "type"="categorical",
                                 "defaultValue"="mean-stdev",
                                 "label"="Aggregator for measurement groups",
                                 "possibleValues"=c("mean-stdev", "quartiles", "count", "min", "max", "sum")
                               ),
                               list(
                                 "id"="step",
                                 "type"="number",
                                 "defaultValue"=1,
                                 "label"="Step",
                                 "possibleValues"=NULL
                               ),
                               list(
                                 "id"="offset",
                                 "type"="categorical",
                                 "defaultValue"="zero",
                                 "label"="Offset",
                                 "possibleValues"=c("zero", "wiggle")
                               ),
                               list(
                                 "id"="interpolation",
                                 "type"="categorical",
                                 "defaultValue"="basis",
                                 "label"="Interpolation",
                                 "possibleValues"=c("linear", "step-before", "step-after", "basis", "basis-open", "basis-closed", "bundle", "cardinal", "cardinal-open", "monotone")
                               ),
                               list(
                                 "id"="abLine",
                                 "type"="number",
                                 "defaultValue"="default",
                                 "label"="Draw abline",
                                 "possibleValues"=NULL
                               )
                             ),
                             colors = c(
                               "#1f77b4",
                               "#ff7f0e",
                               "#2ca02c",
                               "#d62728",
                               "#9467bd",
                               "#8c564b",
                               "#e377c2",
                               "#7f7f7f",
                               "#bcbd22",
                               "#17becf"
                             )
                           ),
                           stop(json_chart_type,
                                " is not a valid chart type. See documentation for supported chart types") 
  )
  
  chart_settings
}

#' Random ID generator for epiviz charts
#'
#' @param prefix prefix for random ID
#' @return random ID
rand_id <- function(prefix = "") {
  sprintf("%s_%d", prefix, floor(stats::runif(1, 1e8, 1e9-1)))
}

#' (taken from epivizr) print settings in a readable format
#' 
.settings_as_df <- function(chart_settings) {
  ids <- sapply(chart_settings, function(x) x$id)
  labels <- sapply(chart_settings, function(x) x$label)
  values <- sapply(chart_settings, function(x) x$defaultValue)
  poss_values <- sapply(chart_settings, function(x) {
    paste0(x$possibleValues, collapse=",")
  })
  types <- sapply(chart_settings, function(x) x$type)
  out <- data.frame(id=ids,
                    label=labels,
                    default_value=values,
                    possible_values=poss_values,
                    type=types,
                    stringsAsFactors=FALSE)
  out
}