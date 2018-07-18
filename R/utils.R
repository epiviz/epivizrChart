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
#' @param simplify if TRUE, convert json-encoded lists to vectors
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

#' Get default chart settings and colors
#' @param chart_type chart type
#' @return list of settings and colors
#'
chart_default_settings_colors <- function(chart_type) {
  chart_settings_json <- json_parser(
    file=system.file("chart_defaults.JSON", package="epivizrChart"))
  
  if(!(chart_type %in% names(chart_settings_json))) {
    stop(chart_type,
         " is not a valid chart type. See documentation for supported chart types")
  }
  
  chart_settings <- chart_settings_json[[chart_type]]
  chart_settings
}

#' Random ID generator for epiviz charts
#'
#' @param prefix prefix for random ID
#' @return random ID
rand_id <- function(prefix="") {
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

#' Construct URL for Websocket connection between R and UI
#'
#' @param host host
#' @param port port
#' @param path path
#' @return url
.constructURL <- function(host="localhost", port=7123L, path="") {
  sprintf("ws://%s:%d/%s", host, port, path)
}

#' (taken from epivizr) register epiviz actions
#'
.register_all_the_epiviz_things <- function(srv, app) {
  # register actions requested from epiviz app
  srv$register_action("getMeasurements", function(request_data) {
    app$get_measurements()
  })
  
  srv$register_action("getRows", function(request_data) {
    app$get_rows(request_data$seqName,
                 request_data$start,
                 request_data$end,
                 request_data$metadata,
                 request_data$datasource)
  })
  
  srv$register_action("getValues", function(request_data) {
    app$get_values(request_data$seqName,
                   request_data$start,
                   request_data$end,
                   request_data$datasource,
                   request_data$measurement)
  })
  
  srv$register_action("getSeqInfos", function(request_data) {
    # TODO
  })
  
  srv$register_action("setChartSettings", function(request_data) {
    # TODO
  })
  
  # TODO: register action 'search'
  
  invisible()
}

#' Get Available Chart Types
#' @export
get_available_chart_types <- function() {
  return(c("BlocksTrack", "HeatmapPlot", 
           "LinePlot", "LineTrack", 
           "ScatterPlot", "StackedLinePlot", 
           "StackedLineTrack", "StackedBlocksTrack"))
}

#' Get Registered BioConductor Objects
#' @export
get_registered_data_types <- function() {
  return(c("GenomicRanges", "GRanges", "RangedSummarizedExperiment", 
           "ExpressionSet", "OrganismDb", 
           "TxDb", "EnsDb", 
           "data.frame"))
}
