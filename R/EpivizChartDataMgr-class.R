#' Class providing data manager for epiviz charts.
#' 
#' @field .ms_list (environment)
#' @field .ms_idCounter (integer)
#' 
#' @importMethodsFrom epivizrData register
#' @import epivizrData
#' @exportClass EpivizChartDataMgr
EpivizChartDataMgr <- setRefClass("EpivizChartDataMgr",
  fields = list(
    .ms_list = "environment",
    .ms_idCounter = "integer"
  ),
  methods=list(
    initialize=function() {
      .self$.ms_list <- new.env(parent=emptyenv())
      .self$.ms_idCounter <- 0L
    },
    add_measurements = function(obj, datasource_name=NULL, datasource_origin_name=deparse(substitute(obj)), ...) {
      "register measurements in data manager"
      if (missing(datasource_name) || is.null(datasource_name)) {
        datasource_name <- datasource_origin_name
      }
      
      if (!is.character(datasource_name)) {
        stop("data source name has to be a string: ", datasource_name)
      }
      
      ms_object <- epivizrData:::register(obj, ...)
      
      .self$.ms_idCounter <- .self$.ms_idCounter + 1L
      ms_id <- sprintf("%s_%d", datasource_name, .self$.ms_idCounter)
      ms_object$set_id(ms_id)
      ms_object$set_name(datasource_name)
      ms_object$set_source_name(datasource_origin_name)

      measurements <- ms_object$get_measurements()
      
      ms_record <- list(measurements=measurements,
        name=datasource_name, 
        source_name=datasource_origin_name,
        obj=ms_object)
      assign(ms_id, ms_record, envir=.self$.ms_list)
      
     return(ms_object)
    }
  )
)
