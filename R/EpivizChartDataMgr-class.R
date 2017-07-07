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
    .ms_obj_names = "list",
    .ms_idCounter = "integer"
  ),
  methods=list(
    initialize=function() {
      .self$.ms_list <- new.env(parent=emptyenv())
      .self$.ms_obj_names = list()
      .self$.ms_idCounter <- 0L
    },
    add_measurements = function(obj, datasource_name=NULL,
      datasource_obj_name=deparse(substitute(obj)), ...) {
      "register measurements in data manager"
      if (is.null(datasource_name))
        datasource_name <- datasource_obj_name

      if (!is.character(datasource_name))
        stop("data source name has to be a string: ", datasource_name)

      ms_object <- epivizrData:::register(obj, ...)

      .self$.ms_idCounter <- .self$.ms_idCounter + 1L
      ms_id <- sprintf("%s_%d", datasource_name, .self$.ms_idCounter)
      ms_object$set_id(ms_id)
      ms_object$set_name(datasource_name)
      ms_object$set_source_name(datasource_obj_name)

      measurements <- ms_object$get_measurements()

      ms_record <- list(measurements=measurements,
        name=datasource_name,
        source_name=datasource_obj_name,
        obj=ms_object)

      assign(ms_id, ms_record, envir=.self$.ms_list)
      .self$.ms_obj_names[[datasource_obj_name]] <- ms_id

     return(ms_object)
    },
    rm_measurements = function(ms_obj_or_id) {
      "remove registered measurements from a given data object"
      ms_obj <- .get_ms_object(ms_obj_or_id)

      if (!is(ms_obj, "EpivizData")) {
        stop("'ms_obj' must be an 'EpivizData' object")
      }

      id <- ms_obj$get_id()
      if (!exists(id, envir=.self$.ms_list, inherits=FALSE)) {
        stop("measurement with id ", id, " not found")
      }

      ms_record <- .self$.ms_list[[id]]
      ms_name <- ms_record$name
      ms <- ms_record$obj$get_measurements()
      rm(list=id, envir=.self$.ms_list)

      invisible()
    },
    rm_all_measurements = function() {
      "remove all registered measurements"
      ids <- ls(.self$.ms_list)
      if (length(ids)>0) {
        for (id in ids) {
          .self$rm_measurements(id)
        }
      }
    },
    .get_ms_object = function(ms_obj_or_id) {
      ms_obj <- NULL
      if (is.character(ms_obj_or_id)) {
        # passed the id instead of the object
        id <- ms_obj_or_id
        if (!exists(id, envir=.self$.ms_list, inherits=FALSE)) {
          stop("measurement with id ", id, " not found")
        }
        ms_obj <- .self$.ms_list[[id]]$obj
      } else {
        ms_obj <- ms_obj_or_id
      }
      ms_obj
    },
    get_ms_obj_names = function() {
      "Get key-value pair list of datasource object names (keys) with
      their corresponding ids (values) in data manager"
      .self$.ms_obj_names
    },
    get_data_json = function(ms_objs_or_ids, chr, start, end) {
      data <- list(format="epiviz")
      ms <- NULL

      for (ms_obj_or_id in ms_objs_or_ids) {
        if (!is(ms_obj_or_id)) stop(ms_obj_or_id, " must be of type EpivizData")
        ms_obj <- .get_ms_object(ms_obj_or_id)

        ms_data <- ms_obj$toJSON(chr, start, end)
        ms <- c(ms, ms_data$measurements)

        data[[obj$get_id()]] <- json_parser(ms_data$data)
      }

      list(measurements=ms, data=json_writer(data))
    }
  )
)
