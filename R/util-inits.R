#' Initialize an [`EpivizChart`] object to visualize in viewer or knit to HTML.
#'
#' @param data_obj A data object that will register to an [`EpivizData`] object.
#' @param measurements An [`EpivizMeasurement`] object.
#' @param datasource_name A name for the datasource. For example, "Mean by Sample Type".
#' @param parent An object of class [`EpivizEnvironment`] or [`EpivizNavigation`] to append the chart within.
#' @param chart The chart type to be visualized: "BlocksTrack", HeatmapPlot", "LinePlot", "LineTrack", "ScatterPlot", "StackedLinePlot", "StackedLineTrack".
#' @param chr The chromosome to filter on, e.g., chr="chr11".
#' @param start The start location, e.g., start=110800000.
#' @param end The end location, e.g., end=130383180.
#' @param settings List of settings, e.g., list(title="Blocks Chart").
#' @param colors List of colors. When chart is rendered to html this will be converted to a string encoded as JSON
#' @param ... Additional arguments passed to [`epivizrData::register`], e.g., `type="bp"`, `columns=c("normal, cancer")`.
#' @return An object of class [`EpivizChart`].
#'
#' @examples
#' data(tcga_colon_blocks)
#' start <- 99800000
#' end <- 103383180
#' blocks_track <- epivizChart(tcga_colon_blocks, chr="chr11", start=start, end=end)
#' # See package vignette for more examples.
#'
#' @importFrom methods is
#' @importFrom BiocGenerics path
#' @export
#' @md
epivizChart <- function(data_obj=NULL, measurements=NULL,
  datasource_name=NULL, parent=NULL, chart=NULL, chr=NULL,
  start=NULL, end=NULL, settings=NULL, colors=NULL, ...) {
  
  if(class(data_obj) %in% c("BamFile", "BigWigFile", "BEDFile")) {
    
    chart <- "IGVTrack" 
    
    file_type <- switch(class(data_obj),
                        "BamFile"="alignment",
                        "BigWigFile"="wig",
                        "BEDFile"="annotation",
         stop(class(data_obj),  " is not a supported file object.",
              " See IGV documentation for supported file formats and types")
        )
    
    file_format <- switch(class(data_obj),
                        "BamFile"="bam",
                        "BigWigFile"="bigWig",
                        "BEDFile"="bed",
                        stop(class(data_obj),  " is not a supported file object.",
                             " See IGV documentation for supported file formats and types")
    )
    
    file_loc <- path(data_obj)

    data_mgr <- EpivizChartDataMgr()
    
    epiviz_chart <- .initialize_chart(
      chart_type=chart,
      data_mgr=data_mgr,
      file=file_loc,
      file_type=file_type,
      file_format=file_format,
      file_name=datasource_name,
      chr=chr,
      start=start,
      end=end,
      settings=settings,
      colors=colors,
      parent=parent)
  }
  else {
    if (is.null(data_obj) && is.null(measurements) && is.null(file))
      stop("You must pass either data or measurements or file location")
    
    # provider id for interactive charts
    p_id <- NULL
    
    # if parent environment/navigation is provided,
    # use its data manager, chr, start, and end
    if (!is.null(parent)) {
      if (!is(parent, "EpivizEnvironment"))
        stop("Parent must be an EpivizEnvironment or EpivizNavigation")
      
      if (parent$is_interactive()) {
        p_id <- parent$epiviz_ds$provider_id
      }
      
      data_mgr <- parent$get_data_mgr()
      
      parent_chr <- parent$get_chr()
      if (!is.null(parent_chr)) chr <- parent_chr
      
      parent_st <- parent$get_start()
      if (!is.null(parent_st)) start <- parent_st
      
      parent_end <- parent$get_end()
      if (!is.null(parent_end)) end <- parent_end
    } else {
      data_mgr <- EpivizChartDataMgr()
    }
    
    # register data -------------------------------------------------------------
    if (!is.null(data_obj)) {
      ms_obj <- data_mgr$add_measurements(data_obj,
                                          datasource_name=datasource_name,
                                          datasource_obj_name=deparse(substitute(data_obj)),
                                          ...)
      
      measurements <- ms_obj$get_measurements()
      
      if (is.null(chart))
        chart <- ms_obj$get_default_chart_type()
    } 
    else {
      # use measurements to plot data
      if (is.null(parent))
        stop("You must pass a 'parent' when using measurements")
      
      if (is.null(chart))
        stop("You must pass 'chart' type when using measurements")
    }
    
    if (is.null(p_id)) {
      # non-interactive, json will be used for data
      ms_data <- data_mgr$get_data(measurements=measurements,
                                   chr=chr, start=start, end=end)
      
    } else {
      # interactive, measurement will be used
      # to request data from data provider
      ms_data <- list(measurements=measurements)
    }

    # initialization ------------------------------------------------------------
    epiviz_chart <- .initialize_chart(
      chart_type=chart,
      data_mgr=data_mgr,
      measurements=ms_data$measurements,
      data=ms_data$data,
      chr=chr,
      start=start,
      end=end,
      settings=settings,
      colors=colors,
      parent=parent)
  }
  
  if (!is.null(parent)) parent$append_chart(epiviz_chart)
  
  return(epiviz_chart)
}

#' Initialize Epiviz Chart based on chart type
#' @param chart_type Chart type.
#' @param ... Arguments for [`EpivizChart`] objects.
#' @md
.initialize_chart <- function(chart_type, ...) {
  epiviz_chart <- switch(chart_type,
    GenesTrack=EpivizGenesTrack,
    BlocksTrack=EpivizBlocksTrack,
    StackedBlocksTrack=EpivizStackedBlocksTrack,
    HeatmapPlot=EpivizHeatmapPlot,
    LinePlot=EpivizLinePlot,
    LineTrack=EpivizLineTrack,
    ScatterPlot=EpivizScatterPlot,
    StackedLinePlot=EpivizStackedLinePlot,
    StackedLineTrack=EpivizStackedLineTrack,
    IGVTrack=EpivizIGVTrack,
    MultiStackedLineTrack=EpivizMultiStackedLineTrack,
    TranscriptTrack=EpivizTranscriptTrack,
    stop(chart_type,  " is not a valid chart type.",
      " See documentation for supported chart types")
  )

  epiviz_chart(...)
}


#' Initialize an [`EpivizNavigation`] object to visualize in viewer or knit to HTML.
#'
#' @param chr The chromosome to filter on, e.g., chr="chr11".
#' @param start The start location, e.g., start=99800000.
#' @param end The end location, e.g., end=130383180.
#' @param parent An object of class `[EpivizEnvironment`] or [`EpivizNavigation`] to append the chart within.
#' @param interactive (logical) enable if running a websocket/shiny server
#' @param ... Additional arguments for initializing navigation, e.g., gene and geneInRange.
#' @return An object of class [`EpivizNavigation`].
#'
#' @examples
#' epiviz <- epivizNav(chr="chr11", start=99800000, end=103383180)
#'
#' @importFrom methods is
#' @export
#' @md
epivizNav <- function(chr=NULL, start=NULL, end=NULL, parent=NULL, interactive=FALSE, shiny=FALSE, ...) {
  # use parent's data manager
  if (!is.null(parent)) {
    if (!is(parent, "EpivizEnvironment"))
      stop("Parent must be an EpivizEnvironment")

    data_mgr <- parent$get_data_mgr()

    # use parent's region if not provided
    if (is.null(chr)) chr <-  parent$get_chr()
    if (is.null(start)) start <- parent$get_start()
    if (is.null(end)) end <- parent$get_end()
  } else {
    data_mgr <- EpivizChartDataMgr()
  }
  
  if (interactive) {
    
    if(!is.null(parent)) {
      epiviz_ds <- parent$epiviz_ds
    }
    else {
      if (shiny) {
        epiviz_ds <- EpivizDataSource(
          provider_type="epiviz.data.ShinyDataProvider",
          provider_id=rand_id("epiviz"),
          provider_url=.constructURL(),
          data_mgr=data_mgr) 
      }
      else {
        epiviz_ds <- EpivizDataSource(
          provider_type="epiviz.data.WebsocketDataProvider",
          provider_id=rand_id("epiviz"),
          provider_url=.constructURL(),
          data_mgr=data_mgr) 
      }

    }
  } else {
    epiviz_ds <- NULL
  }

  epivizNav <- EpivizNavigation(chr=chr, start=start,
    end=end, parent=parent, data_mgr=data_mgr, interactive=interactive, epiviz_ds=epiviz_ds, ...)

  if (!is.null(parent)) parent$append_chart(epivizNav)

  epivizNav
}


#' Initialize an [`EpivizEnvironment`] object.
#'
#' @param chr The chromosome to filter on, e.g., `chr="chr11"`
#' @param start The start location, e.g., `start=99800000`.
#' @param end The end location, e.g., `end=130383180`.
#' @param interactive (logical) enable if running a websocket/shiny server
#' @param ... Additional params to pass to [`EpivizWebComponent`]
#' @return An object of class [`EpivizEnvironment`]
#'
#' @examples
#' epiviz <- epivizEnv(chr="chr11", start=99800000, end=103383180)
#'
#' @export
#' @md
epivizEnv <- function(chr=NULL, start=NULL, end=NULL, interactive=FALSE, shiny=FALSE, ...) {
  data_mgr <- EpivizChartDataMgr()

  if (interactive) {
    cat(shiny)
    if (shiny) {
      epiviz_ds <- EpivizDataSource(
        provider_type="epiviz.data.ShinyDataProvider",
        provider_id=rand_id("epiviz"),
        provider_url=.constructURL(),
        data_mgr=data_mgr) 
    }
    else {
      epiviz_ds <- EpivizDataSource(
        provider_type="epiviz.data.WebsocketDataProvider",
        provider_id=rand_id("epiviz"),
        provider_url=.constructURL(),
        data_mgr=data_mgr) 
    }
  } else {
    epiviz_ds <- NULL
  }

  EpivizEnvironment(chr=chr, start=start, end=end, data_mgr=data_mgr,
    interactive=interactive, epiviz_ds=epiviz_ds, ...)
}
