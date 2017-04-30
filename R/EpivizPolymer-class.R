epivizEnvironment <- function(chr="chr11", start=99800000, end=103383180) {
  server <- epivizrServer::createServer(non_interactive=TRUE)
  data_mgr <- epivizrData::createMgr(server)
  epivizEnvir <- htmltools::tag("epiviz-environment" , list(chr=chr, start=start, end=end))
  epiviz <- EpivizPolymer$new(chr=chr, start=start, end=end,
    data_mgr=data_mgr, epivizEnvir=epivizEnvir)
  
  return(epiviz)
}

#' Class encapsulating a epiviz data for polymer
#' 
#' @import methods
#' 
EpivizPolymer <- setRefClass("EpivizPolymer",
  fields=list(
    chr="character",
    start="numeric",
    end="numeric",
    data_mgr="EpivizDataMgr",
    epivizEnvir="shiny.tag"
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

      chart <- .self$to_HTML(ms_obj, ...)
      
      htmltools::tagAppendChild(.self$epivizEnvir, chart)
      
      invisible()
    }, 
    to_HTML = function(ms_obj = NULL,...) {
      "Return a string representing a polymer object.
      \\describe{
      \\item{data}{List of row and column data in JSON format}
      }"
      if (is.null(ms_obj)){
	      stop("data missing")
      }
      
      chart_tag <- ms_obj$tag_HTML() #TODO MORE DATA
      
      ms <- ms_obj$get_measurements()
      ms_list <- lapply(ms, as.list)
      ms_json <- epivizrServer::json_writer(ms_list)
      
      row_data <- .row_data(ms_obj=ms_obj, chr=.self$chr, start=.self$start, end=.self$end)
      col_data <- NULL
      
      # Blocks Tracks and Genes Tracks do not use values
      if (ms_obj$get_default_chart_type() != "BlocksTrack" && 
          ms_obj$get_default_chart_type() != "GenesTrack") {
        col_data <- .col_data(ms_obj, .self$chr, .self$start, .self$end)                  
      }
      
      data_json <- paste0(row_data, col_data, collapse=",")
      polymer_chart <- .polymer_chart(chart_tag, ms_obj$get_id(), ms_json, data_json)
      return(polymer_chart)
    },
    .row_data = function(ms_obj, chr, start, end) {
        query <- GenomicRanges::GRanges(chr, ranges = IRanges::IRanges(start, end))
        result <- ms_obj$get_rows(query = query)
        json_row_data <- epivizrServer::json_writer(result)
        
        return(json_row_data)
    },
    .col_data = function(ms_obj, chr, start, end) {
      query <- GenomicRanges::GRanges(chr, ranges = IRanges::IRanges(start, end))
      
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
    .polymer_tag = function(datasourceJSON=TRUE) {
      tag <- NULL
      if (.self$.type == "epiviz.plugins.charts.BlocksTrack") {
        tag <- ifelse(datasourceJSON == TRUE, "epiviz-json-blocks-track", "epiviz-blocks-track")
      } else if (.self$.type == "epiviz.plugins.charts.HeatmapPlot") {
        tag <- ifelse(datasourceJSON == TRUE, "epiviz-json-heatmap-plot", "epiviz-heatmap-plot")
      } else if (.self$.type == "epiviz.plugins.charts.LinePlot") {
        tag <- ifelse(datasourceJSON == TRUE, "epiviz-json-line-plot", "epiviz-line-plot")
      } else if (.self$.type == "epiviz.plugins.charts.LineTrack") {
        tag <- ifelse(datasourceJSON == TRUE, "epiviz-json-line-track", "epiviz-line-track")
      } else if (.self$.type == "epiviz.plugins.charts.ScatterPlot") {
        tag <- ifelse(datasourceJSON == TRUE, "epiviz-json-scatter-plot", "epiviz-scatter-plot")
      } else if (.self$.type == "epiviz.plugins.charts.StackedLinePlot") {
        tag <- ifelse(datasourceJSON == TRUE, "epiviz-json-stacked-line-plot", "epiviz-stacked-line-plot")
      } else if (.self$.type == "epiviz.plugins.charts.StackedLineTrack") {
        tag <- ifelse(datasourceJSON == TRUE, "epiviz-json-stacked-line-track", "epiviz-stacked-line-track")
      } # else if (.self$.type == "epiviz.plugins.charts.GenesTrack")
      return(tag)
    },
    show = function() {
      "Print environment of this object"
      return(htmltools::knit_print.html(.self$epivizEnvir))
    }
  )
)
