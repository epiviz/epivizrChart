#' Class for knitting epiviz charts
#' 
#' @field chr (character) chromosome to to display in environment plot.
#' @field start (integer) start location to display in environment plot.
#' @field end (integer) end location to to display in environment plot.
#' @field data_mgr An object of class \code{\link[epivizrPolymer]{EpivizPolyDataMgr}} used to serve data to epiviz environment.
#' @field epiviz_envir An object of class \code{htmltools}{shiny.tag} used to nest chart tags in epiviz-environment tag.
#' 
#' @importFrom epivizrServer json_writer
#' @import GenomicRanges 
#' @import epivizr
#' @import epivizrData
#' @import IRanges
#' @import htmltools
#' 
#' @exportClass EpivizPolymer
EpivizPolymer <- setRefClass("EpivizPolymer",
  fields=list(
    chr="character",
    start="numeric",
    end="numeric",
    data_mgr="EpivizPolyDataMgr",
    epiviz_envir="ANY" 
  ),
  methods=list(
    get_environment = function() {
      "Return the epiviz-environment"
      return(.self$epiviz_envir)
    },
    plot = function(data_object, datasource_name, 
      datasource_origin_name=deparse(substitute(data_object)), 
      chart_type=NULL, settings=NULL, colors=NULL, ...) { 
      "Return a shiny.tag representing an epiviz chart and adds it as a child of the epiviz environment tag
      \\describe{
      \\item{data_object}{GenomicRanges object to attach as chart's data}
      \\item{datasource_name}{Name for datasource}
      \\item{chart_type}{Type of chart for plot (BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack)}
      \\item{settings}{List of settings for chart}
      \\item{colors}{List of colors for chart}
      \\item{...}{Type and columns}
      }"
      
      if (missing(datasource_name)) {
        datasource_name <- datasource_origin_name
      }
      
      ms_obj <- .self$data_mgr$add_measurements(data_object, datasource_name=datasource_name, 
        datasource_origin_name=datasource_origin_name, ...)
      
      chart <- .self$.create_chart_html(ms_obj, settings, colors, chart_type)
      
      .self$epiviz_envir <- tagAppendChild(.self$epiviz_envir, chart)
      
      invisible(chart)
    }, 
    .create_chart_html = function(ms_obj, settings, colors, chart_type) {
      "Creates a shiny.tag representing an epiviz chart
      \\describe{
      \\item{ms_obj}{EpivizData object}
      \\item{settings}{Chart settings}
      \\item{colors}{Chart colors}
      \\item{chart_type}{Chart type for plot (BlocksTrack, HeatmapPlot, LinePlot,LineTrack, ScatterPlot, StackedLinePlot, StackedLineTrack)}}
      }"
      data_json <- .data_toJSON(ms_obj)
      
      if (is.null(chart_type)) {
        chart_tag <- ms_obj$get_default_chart_type_html()
      } else {
        chart_tag <- .chart_type_to_html_tag(chart_type)
      }
    
      polymer_chart <- tag(
        chart_tag, 
        list(
          id=ms_obj$get_id(), 
          measurement=data_json$measurements, 
          data=data_json$data,
          settings=settings, 
          colors=colors))
      
      return(polymer_chart)
    },
    .data_toJSON = function(ms_obj) {
      ms <- ms_obj$get_measurements()
      ms_list <- lapply(ms, as.list)
      ms_json <- json_writer(ms_list)
      
      row_data <- .get_row_data(ms_obj)
      col_data <- NULL
      
      # Blocks Tracks and Genes Tracks do not use values
      if (ms_obj$get_default_chart_type() != "BlocksTrack" && 
          ms_obj$get_default_chart_type() != "GenesTrack") {
        col_data <- .get_col_data(ms_obj)                  
      }
      
      result <- list(rows=row_data, cols=col_data)
      data_json <- json_writer(result)
      
      return(list(measurements=ms_json, data=data_json))
    },
    .get_row_data = function(ms_obj) {
        query <- GenomicRanges::GRanges(.self$chr, ranges=IRanges(.self$start, .self$end))
        result <- ms_obj$get_rows(query = query, metadata=c()) 
        # TODO: change metadata value
        
        return(result)
    },
    .get_col_data = function(ms_obj) {
      query <- GRanges(.self$chr, ranges=IRanges(.self$start, .self$end))
      
      ms_list <- ms_obj$get_measurements()
      col_data <- vector("list", length(ms_list)) 
      
      for (i in 1:length(ms_list)) {
        ms <- ms_list[[i]]
        values <- ms_obj$get_values(query=query, measurement=ms@id)
        col_data[[i]] <- values
      }
      
      return(col_data)
    },
    .chart_type_to_html_tag = function(chart_type) {
      chart_tag <- switch(chart_type,
        BlocksTrack = "epiviz-json-blocks-track",
        HeatmapPlot = "epiviz-json-heatmap-plot",
        LinePlot = "epiviz-json-line-plot",
        LineTrack = "epiviz-json-line-track",
        ScatterPlot = "epiviz-json-scatter-plot",
        StackedLinePlot = "epiviz-json-stacked-line-plot",
        StackedLineTrack = "epiviz-json-stacked-line-track"
      )
      return(chart_tag)
    },
    show = function() {
      "Show environment of this object"
      knit_print.shiny.tag(.self$epiviz_envir)
    }
  )
)
