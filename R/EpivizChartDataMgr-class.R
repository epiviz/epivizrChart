#' Class providing data manager for epiviz charts.
#'
#' @field .ms_list (environment) List of measurment records
#' @field .ms_idCounter (integer) Counter for ID generator
#'
#' @importMethodsFrom epivizrData register
#' @import epivizrData
#' @importFrom methods new
EpivizChartDataMgr <- setRefClass("EpivizChartDataMgr",
  fields = list(
    .ms_list="environment",
    .ms_idCounter="integer"
  ),
  methods=list(
    initialize=function() {
      .self$.ms_list <- new.env(parent=emptyenv())
      .self$.ms_idCounter <- 0L
    },
    add_measurements = function(obj, datasource_name=NULL,
      datasource_obj_name=deparse(substitute(obj)), ...) {
      "Register measurements in data manager"
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

      ms_object
    },
    rm_measurements = function(ms_obj_or_id) {
      "Remove registered measurements from a given data object"
      ms_obj <- .get_ms_object(ms_obj_or_id)

      if (!is(ms_obj, "EpivizData")) {
        stop("'ms_obj' must be an 'EpivizData' object")
      }

      id <- ms_obj$get_id()
      if (!exists(id, envir=.self$.ms_list, inherits=FALSE))
        stop("measurement with id ", id, " not found")

      rm(list=id, envir=.self$.ms_list)

      invisible()
    },
    rm_all_measurements = function() {
      "Remove all registered measurements"
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
    get_data = function(measurements, chr=NULL, start=NULL, end=NULL) {
      "Get data from data mgr based on measurements, chr, start, and end
      \\describe{
        \\item{measurements}{List of EpivizMeasurements}
        \\item{chr}{Chromosome}
        \\item{start}{Start location}
        \\item{end}{End location}
      }"
      data <- list(format="epiviz")
      ms_list <- NULL
      datasources <- lapply(measurements, function(ms) ms@datasourceId)

      for (datasource in unique(datasources)) {
        ms_obj <- .get_ms_object(datasource)

        ms_data <- ms_obj$get_data(chr, start, end)
        ms_list <- c(ms_list, ms_obj$get_measurements())

        data[[ms_obj$get_id()]] <- ms_data$data
      }

      list(measurements=ms_list, data=data)
    }
  )
)
