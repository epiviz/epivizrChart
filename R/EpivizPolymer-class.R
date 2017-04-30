
#' Class for knitting epiviz charts
#' @field chr (character) chromosome to to display in environment plot.
#' @field start (integer) start location to display in environment plot.
#' @field end (integer) end location to to display in environment plot.
#' @field data_mgr An object of class \code{\link[epivizrData]{EpivizDataMgr}} used to serve data to epiviz app.
#' @field epiviz_envir An object of class \code{shiny.tag} used to nest chart tags in epiviz-environment tag
#' 
#' @importClassesFrom epivizrData EpivizDataMgr EpivizMeasurement
#' @importClassesFrom epivizrServer EpivizServer
#' @import GenomicRanges 
#' @import S4Vectors
#' @import methods
#' @import IRanges
#' @import htmltools
#' 
#' @exportClass EpivizPolymer
EpivizPolymer <- setRefClass("EpivizPolymer",
  fields=list(
    chr="character",
    start="numeric",
    end="numeric",
    data_mgr="EpivizDataMgr",
    epiviz_envir="ANY"
  ),
  methods=list(
    plot = function(data_object, datasource_name = NULL,
      #settings=NULL, colors=NULL,
      ...) { 
      "..."
      datasource_origin_name <- deparse(substitute(data_object))
      if (missing(datasource_name)) {
        datasource_name <- datasource_origin_name
      }
      ms_obj <- .self$data_mgr$add_measurements(data_object, datasource_name=datasource_name, 
        datasource_origin_name=datasource_origin_name, ...)

      chart <- .self$to_HTML(ms_obj)
      
      .self$epiviz_envir <- htmltools::tagAppendChild(.self$epiviz_envir, chart)
      
      invisible()
    }, 
    to_HTML = function(ms_obj) {
      "Return a shiny.tag representing an epiviz chart
      \\describe{
      \\item{ms_obj}{EpivizData object}
      \\item{...}{EpivizData object}
      }"
      chart_tag <- ms_obj$tag_HTML() # ADD THIS TO EPIVIZDATA CLASSES
      
      ms <- ms_obj$get_measurements()
      #ms_list <- lapply(ms, as.list)
      ms_list <- lapply(ms, function(x) {
        nms <- slotNames("EpivizMeasurement")
        out <- lapply(nms, function(slot_name) slot(x, slot_name))
        names(out) <- nms
        out
      })  
      ms_json <- epivizrServer::json_writer(ms_list)
      
      row_data <- .row_data(ms_obj)
      col_data <- NULL
      
      # Blocks Tracks and Genes Tracks do not use values
      if (ms_obj$get_default_chart_type() != "BlocksTrack" && 
          ms_obj$get_default_chart_type() != "GenesTrack") {
        col_data <- .col_data(ms_obj)                  
      }
      
      data_json <- paste0(row_data, col_data, collapse=",")
      polymer_chart <- .polymer_chart(chart_tag, ms_obj$get_id(), ms_json, data_json)
      
      return(polymer_chart)
    },
    .row_data = function(ms_obj) {
        query <- GenomicRanges::GRanges(.self$chr, ranges = IRanges::IRanges(.self$start, .self$end))
        result <- ms_obj$get_rows(query = query)
        json_row_data <- epivizrServer::json_writer(result)
        
        return(json_row_data)
    },
    .col_data = function(ms_obj) {
      query <- GenomicRanges::GRanges(.self$chr, ranges = IRanges::IRanges(.self$start, .self$end))
      
      ms_list <- ms_obj$get_measurements()
      col_data <- vector("list", length(ms_list)) 
      
      for (i in 1:length(ms_list)) {
        ms <- ms_list[[i]]
        values <- ms_obj$get_values(query=query, measurement=ms@id)
        values_json <- epivizrServer::json_writer(values)
        
        col_data[[i]] <- values_json
      }
      
      col_data <- paste0(col_data, collapse='')
      
      return(col_data)
    },
    .polymer_chart = function(chart_type, chart_id, ms, data) {
      chart <- htmltools::tag(chart_type, list(id=chart_id, measurement=ms, data=data))
      
      return(chart)
    },
    show = function() {
      "Print environment of this object"
      return(htmltools::knit_print.html(.self$epiviz_envir))
    }
  )
)
