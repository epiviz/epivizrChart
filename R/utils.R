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
#' @param indent integer specifying how much indentation to use when formatting the JSON object; if 0, no pretty-formatting is used
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
#' @param chart_settings chart settings
#' @return chart settings as data frame
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
#' @param srv epivizrServer object
#' @param app EpivizApp object
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
    return(list("hg19" = list(
      list("chr1", 1, 248956422),
      list("chr2", 1, 242193529),
      list("chr3", 1, 198295559),
      list("chr4", 1, 190214555),
      list("chr5", 1, 181538259),
      list("chr6", 1, 170805979),
      list("chr7", 1, 159345973),
      list("chr8", 1, 145138636),
      list("chr9", 1, 138394717),
      list("chr10", 1, 133797422),
      list("chr11", 1, 135086622),
      list("chr12", 1, 133275309),
      list("chr13", 1, 114364328),
      list("chr14", 1, 107043718),
      list("chr15", 1, 101991189),
      list("chr16", 1, 90338345),
      list("chr17", 1, 83257441),
      list("chr18", 1, 80373285),
      list("chr19", 1, 58617616),
      list("chr20", 1, 64444167),
      list("chr21", 1, 46709983),
      list("chr22", 1, 50818468),
      list("chrX", 1, 156040895),
      list("chrY", 1, 57227415)
    )))
  })
  
  srv$register_action("setChartSettings", function(request_data) {
    # TODO
  })
  
  # TODO: register action 'search'
  
  invisible()
}

#' Get Available Chart Types
#' @return url
get_available_chart_types <- function() {
  return(c("BlocksTrack", "HeatmapPlot", 
           "LinePlot", "LineTrack", 
           "ScatterPlot", "StackedLinePlot", 
           "StackedLineTrack", "StackedBlocksTrack", "MultiStackedLineTrack", "TranscriptTrack"))
}

#' Get Registered Chart Types
#' @return url
get_registered_data_types <- function() {
  return(c("BlocksTrack", "HeatmapPlot", 
           "LinePlot", "LineTrack", 
           "ScatterPlot", "StackedLinePlot", 
           "StackedLineTrack", "StackedBlocksTrack", "MultiStackedLineTrack", "TranscriptTrack"))
}
