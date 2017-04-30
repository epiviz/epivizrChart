
#' Class for knitting epiviz charts
#' @field chr (character) chromosome to to display in environment plot.
#' @field start (integer) start location to display in environment plot.
#' @field end (integer) end location to to display in environment plot.
#' @field data_mgr An object of class \code{\link[epivizrData]{EpivizDataMgr}} used to serve data to epiviz charts.
#' @field epiviz_envir An object of class \code{shiny.tag} used to nest chart tags in epiviz-environment tag.
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
    epiviz_envir="ANY" # TODO:  change to "shiny.tag" ?
  ),
  methods=list(
    plot = function(data_object, datasource_name, 
      datasource_origin_name=deparse(substitute(data_object)),  settings=NULL, colors=NULL, ...) { 
      "Return a shiny.tag representing an epiviz chart and appends to the epiviz environment
      \\describe{
      \\item{data_object}{GenomicRanges object to attach as chart's data}
      \\item{datasource_name}{Name for datasource}
      \\item{settings}{List of settings for chart}
      \\item{colors}{List of colors for chart}
      \\item{...}{Type and columns}
      }"
      
      if (missing(datasource_name)) {
        datasource_name <- datasource_origin_name
      }
      
      ms_obj <- .self$data_mgr$add_measurements(data_object, datasource_name=datasource_name, 
        datasource_origin_name=datasource_origin_name, ...)

      chart <- .self$.create_chart_HTML(ms_obj, settings, colors, ...)
      
      .self$epiviz_envir <- htmltools::tagAppendChild(.self$epiviz_envir, chart)
      
      invisible(chart)
    }, 
    .create_chart_HTML = function(ms_obj, settings, colors, ...) {
      "Creates a shiny.tag representing an epiviz chart
      \\describe{
      \\item{ms_obj}{EpivizData object}
      \\item{...}{EpivizData object}
      }"
      ms <- ms_obj$get_measurements()
      #ms_list <- lapply(ms, as.list)
      ms_list <- lapply(ms, function(x) {
        nms <- slotNames("EpivizMeasurement")
        out <- lapply(nms, function(slot_name) slot(x, slot_name))
        names(out) <- nms
        out
      })  
      ms_json <- epivizrServer::json_writer(ms_list)
      
      row_data <- .row_data_toJSON(ms_obj)
      col_data <- NULL
      
      # Blocks Tracks and Genes Tracks do not use values
      if (ms_obj$get_default_chart_type() != "BlocksTrack" && 
          ms_obj$get_default_chart_type() != "GenesTrack") {
        col_data <- .col_data_toJSON(ms_obj)                  
      }
      
      data_json <- paste0(row_data, col_data, collapse=",")
      chart_tag <- ms_obj$tag_HTML(...) # ADD SUPPORT FOR OTHER TYPES
      
      polymer_chart <- htmltools::tag(chart_tag, 
        list(id=ms_obj$get_id(), measurement=ms_json, data=data_json, settings=settings, colors=colors))
      
      return(polymer_chart)
    },
    update = function(chart, data_obj){
      # TO DO
    },
    remove = function(chart) {
      # TO DO
    },
    .row_data_toJSON = function(ms_obj) {
        query <- GenomicRanges::GRanges(.self$chr, ranges = IRanges::IRanges(.self$start, .self$end))
        result <- ms_obj$get_rows(query = query)
        json_row_data <- epivizrServer::json_writer(result)
        
        return(json_row_data)
    },
    .col_data_toJSON = function(ms_obj) {
      query <- GenomicRanges::GRanges(.self$chr, ranges = IRanges::IRanges(.self$start, .self$end))
      
      ms_list <- ms_obj$get_measurements()
      col_data <- vector("list", length(ms_list)) 
      
      for (i in 1:length(ms_list)) {
        ms <- ms_list[[i]]
        values <- ms_obj$get_values(query=query, measurement=ms@id)
        values_json <- epivizrServer::json_writer(values)
        
        col_data[[i]] <- values_json
      }
      
      col_data <- paste0(col_data, collapse=',')
      
      return(col_data)
    },
    show = function() {
      "Show environment of this object"
      htmltools::knit_print.shiny.tag(.self$epiviz_envir)
    }
  )
)
