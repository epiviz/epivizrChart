#' Initialize an \code{\link[epivizrChart]{EpivizChart}} object to visualize in viewer or knit to HTML.
#'
#' @param data_obj A data object that will register to an \code{\link[epivizrData]{EpivizData}} object.
#' @param measurements An \code{\link[epivizrData]{EpivizMeasurement}} object.
#' @param datasource_name A name for the datasource. For example, "Mean by Sample Type".
#' @param parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} or \code{\link[epivizrChart]{EpivizNavigation}} to append the chart within.
#' @param chart The chart type to be visualized: "BlocksTrack", HeatmapPlot", "LinePlot", "LineTrack", "ScatterPlot", "StackedLinePlot", "StackedLineTrack".
#' @param chr The chromosome to filter on, e.g., chr="chr11".
#' @param start The start location, e.g., start=110800000.
#' @param end The end location, e.g., end=130383180.
#' @param settings List of settings, e.g., list(title="Blocks Chart").
#' @param colors List of colors. When chart is rendered to html this will be converted to a string encoded as JSON
#' @param ... Additional arguments passed to \code{\link[epivizrData]{register}}, e.g., type="bp", columns=c("normal, cancer").
#' @return An object of class \code{\link[epivizrChart]{EpivizChart}}.
#'
#' @examples
#' data(tcga_colon_blocks)
#' start <- 99800000
#' end <- 103383180
#' blocks_track <- epivizChart(tcga_colon_blocks, chr="chr11", start=start, end=end)
#' # See package vignette for more examples.
#'
#' @importFrom methods is
#' @export
epivizChart <- function(data_obj=NULL, measurements=NULL,
  datasource_name=NULL, parent=NULL, chart=NULL, chr=NULL,
  start=NULL, end=NULL, settings=NULL, colors=NULL, ...) {

  if (is.null(data_obj) && is.null(measurements))
    stop("You must pass either data or measurements")

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

  } else {
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

  if (!is.null(parent)) parent$append_chart(epiviz_chart)

  epiviz_chart
}

#' Initialize Epiviz Chart based on chart type
#' @field chart_type Chart type.
#' @field ... Arguments for \code{\link[epivizrChart]{EpivizChart}} objects.
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
    stop(chart_type,  " is not a valid chart type.",
      " See documentation for supported chart types")
  )

  epiviz_chart(...)
}


#' Initialize an \code{\link[epivizrChart]{EpivizNavigation}} object to visualize in viewer or knit to HTML.
#'
#' @param chr The chromosome to filter on, e.g., chr="chr11".
#' @param start The start location, e.g., start=99800000.
#' @param end The end location, e.g., end=130383180.
#' @param parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} or \code{\link[epivizrChart]{EpivizNavigation}} to append the chart within.
#' @param ... Additional arguments for initializing navigation, e.g., gene and geneInRange.
#' @return An object of class \code{\link[epivizrChart]{EpivizNavigation}}.
#'
#' @examples
#' epiviz <- epivizNav(chr="chr11", start=99800000, end=103383180)
#'
#' @importFrom methods is
#' @export
epivizNav <- function(chr=NULL, start=NULL, end=NULL, parent=NULL, interactive=FALSE, ...) {
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
    epiviz_ds <- EpivizDataSource(
      provider_type="epiviz.data.WebsocketDataProvider",
      provider_id=rand_id("epiviz"),
      provider_url=.constructURL(),
      data_mgr=data_mgr)
  } else {
    epiviz_ds <- NULL
  }

  epivizNav <- EpivizNavigation(chr=chr, start=start,
    end=end, parent=parent, data_mgr=data_mgr, interactive=interactive, epiviz_ds=epiviz_ds, ...)

  if (!is.null(parent)) parent$append_chart(epivizNav)

  epivizNav
}


#' Initialize an \code{\link[epivizrChart]{EpivizEnvironment}} object.
#'
#' @param chr The chromosome to filter on, e.g., chr="chr11"
#' @param start The start location, e.g., start=99800000.
#' @param end The end location, e.g., end=130383180.
#' @param interactive (logical) todo
#' @param ... Additional params to pass to \code{\link[epivizrChart]{EpivizWebComponent}}
#' @return An object of class \code{\link[epivizrChart]{EpivizEnvironment}}
#'
#' @examples
#' epiviz <- epivizEnv(chr="chr11", start=99800000, end=103383180)
#'
#' @export
epivizEnv <- function(chr=NULL, start=NULL, end=NULL, interactive=FALSE, ...) {
  data_mgr <- EpivizChartDataMgr()

  if (interactive) {
    epiviz_ds <- EpivizDataSource(
      provider_type="epiviz.data.WebsocketDataProvider",
      provider_id=rand_id("epiviz"),
      provider_url=.constructURL(),
      data_mgr=data_mgr)
  } else {
    epiviz_ds <- NULL
  }

  EpivizEnvironment(chr=chr, start=start, end=end, data_mgr=data_mgr,
    interactive=interactive, epiviz_ds=epiviz_ds, ...)
}
