#' Class providing data manager for epiviz charts.
#'
#' @field .ms_list (environment) List of measurment records
#' @field .ms_idCounter (integer) Counter for ID generator
#'
#' @importMethodsFrom epivizrData register
#' @import epivizrData
#' @importFrom methods new
EpivizChartDataMgr <- setRefClass("EpivizChartDataMgr",
  fields=list(
    .ms_list="environment",
    .ms_idCounter="integer",
    .genome="ANY"
  ),
  methods=list(
    initialize=function() {
      .self$.ms_list <- new.env(parent=emptyenv())
      .self$.ms_idCounter <- 0L
    },
    add_measurements=function(obj, datasource_name=NULL,
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
    rm_measurements=function(ms_obj_or_id) {
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
    rm_all_measurements=function() {
      "Remove all registered measurements"
      ids <- ls(.self$.ms_list)
      if (length(ids)>0) {
        for (id in ids) {
          .self$rm_measurements(id)
        }
      }
    },
    .get_ms_object=function(ms_obj_or_id) {
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
    get_data=function(measurements, chr=NULL, start=NULL, end=NULL) {
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
    },
    get_measurements=function() {
      out <- epivizrData:::.emptyEpivizMeasurement()

      measurements <- list()
      ids <- ls(.self$.ms_list)
      if (length(ids) > 0) {
        for (id in ids) {
          ms_record <- .self$.ms_list[[id]]
          ms <- ms_record$obj$get_measurements()
          for (cur_ms in ms) {
            out <- epivizrData:::.appendEpivizMeasurement(out, cur_ms)
          }
        }
      }

      as.list(out)
    },
    get_seqinfo=function() {
      if(!is.null(.self$.genome)) {
        seqinfo <- seqinfo(.self$.genome)
        seqlengths <- seqlengths(seqinfo)+1
        # seqinfo_list <- structure(lapply(seqlengths, function(x) c(1,x)), names=names(seqlengths))
        seqinfo_list <- mapply(function(seqname, seqlength) list(seqname, 1, seqlength),
                               names(seqlengths), seqlengths,
                               SIMPLIFY=FALSE, USE.NAMES=FALSE)
        seqinfo_list
      }
    },
    get_rows=function(chr, start, end, metadata, datasource) {
      if (is.null(chr) || is.null(start) || is.null(end)) {
        query <- NULL
      } else {
        query <- GRanges(chr, ranges=IRanges(start, end))
      }
      ms_obj <- .self$.find_datasource(datasource)
      ms_obj$get_rows(query, metadata)
    },
    get_values=function(chr, start, end, datasource, measurement) {
      if (is.null(chr) || is.null(start) || is.null(end)) {
        query <- NULL
      } else {
        query <- GRanges(chr, ranges=IRanges(start, end))
      }
      ms_obj <- .self$.find_datasource(datasource)
      ms_obj$get_values(query, measurement)
    },
    .find_datasource=function(datasource) {
      if (!exists(datasource, .self$.ms_list, inherits=FALSE)) {
        stop("cannot find datasource", datasource)
      }
      ms_obj <- .self$.ms_list[[datasource]]$obj
    },
    add_shiny_handler=function(session) {
      "
      Handlers to enable interactions with Shiny session.
      \\describe{
        \\item{session}{Shiny session object}
      }"
      observeEvent(session$input[['epivizapi']], {
        params <- session$input[['epivizapi']]
        rid <- params[["_reqid"]]
        request_data <- params[["_args"]]
        method <- request_data$action
        
        if(method == "getMeasurements") {
          response <- list(requestId=rid)
          response["data"] <- json_writer(.self$get_measurements())
          session$sendCustomMessage(type = "epivizapi.callback", response);
        }
        else if (method == "getRows") {
          response <- list(requestId=rid)
          response["data"] <- json_writer(.self$get_rows(request_data$seqName,
                                                             request_data$start,
                                                             request_data$end,
                                                             request_data$metadata,
                                                             request_data$datasource))
          session$sendCustomMessage(type = "epivizapi.callback", response);
        }
        else if(method == "getValues") {
          response <- list(requestId=rid, "jsonType"="epivizr")
          result <- list()
          
          metadata <- request_data$metadata

          # if(is.null(metadata)) {
          #   metadata <- NULL
          # }

          values <- .self$get_values(request_data$seqName,
                                         request_data$start,
                                         request_data$end,
                                         request_data$datasource,
                                         request_data$measurement)

          resp_values <- list(
            globalStartIndex = values$globalStartIndex,
            values = list()
          )

          resp_values$values[[request_data$measurement]] <- values$values;

          result["values"] <- json_writer(resp_values)

          result["rows"] <- json_writer(.self$get_rows(request_data$seqName,
                                                           request_data$start,
                                                           request_data$end,
                                                           metadata,
                                                           request_data$datasource))
          response["data"] <- json_writer(result)
          
          # data <- .self$get_values(request_data$measurement,
          #                          request_data$seqName,
          #                          request_data$start,
          #                          request_data$end
          #                         )
          
          # response["data"] <- json_writer(data$data)
          session$sendCustomMessage(type = "epivizapi.callback", response)
        }
        else if(method == "getSeqInfos") {
          response <- list(requestId=rid)
          seqinfo_list <- .self$get_seqinfo()
          response[["data"]] <- json_writer(list("hg19"=seqinfo_list))
          session$sendCustomMessage(type = "epivizapi.callback", response);
        }
      })
    },
    add_genome=function(genome) {
      "
      Add genome to data manager (for seqInfo)
      \\describe{
        \\item{chr}{Chromosome}
        \\item{start}{Start location}
        \\item{end}{End location}
      }"
      .self$.genome <- genome
    }
  )
)
