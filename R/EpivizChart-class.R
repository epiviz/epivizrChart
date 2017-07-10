#' Data container for an epiviz chart.
#'
#' @field data Character string of an epiviz chart's data attribute.
#' @field colors Character string of an epiviz chart's colors attribute.
#' @field settings character string of an epiviz chart's settings attribute.
#' @field parent An EpivizEnvironment where the chart is nested or NULL
#' @import epivizrData
#' @import htmltools
#' @importFrom methods new
#' @export EpivizChart
#' @exportClass EpivizChart
EpivizChart <- setRefClass("EpivizChart",
  contains="EpivizPolymer",
  fields=list(
    data="list",
    colors="CharacterOrNULL",
    settings="CharacterOrNULL",
    parent="ANY"
  ),
  methods=list(
    initialize = function(data_obj=NULL, datasource_name=NULL, parent=NULL,
      measurements=NULL, chart=NULL, chr=NULL, start=NULL, end=NULL,
      settings=NULL, colors=NULL, ...) {
      if (is.null(data_obj) && is.null(measurements))
        stop("You must pass either data or measurements")

      .self$colors <- colors
      .self$settings <- settings

      chart_chr <- chr
      chart_start <- start
      chart_end <- end
      chart_ms <- measurements

      # if parent environment/navigation is provided,
      # use its data manager, chr, start, and end
      if (is.null(parent)) {
        mgr <- EpivizChartDataMgr()

      } else {
        if (!is(parent, "EpivizEnvironment"))
          stop("Parent must be an EpivizEnvironment or EpivizNavigation")

        mgr <- parent$get_data_mgr()

        parent_chr <- parent$get_chr()
        if (!is.null(parent_chr)) chart_chr <- parent_chr

        parent_st <- parent$get_start()
        if (!is.null(parent_st)) chart_start <- parent_st

        parent_end <- parent$get_end()
        if (!is.null(parent_end)) chart_end <- parent_end
      }
      .self$parent <- parent

      # register data ---------------------------
      if (!is.null(data_obj)) {
        datasource_obj_name <- deparse(substitute(data_obj))
        if (is.null(datasource_name)) datasource_name <- datasource_obj_name

        ms_obj <- mgr$add_measurements(data_obj,
          datasource_name=datasource_name,
          datasource_obj_name=datasource_obj_name, ...)

        chart_ms <- ms_obj$get_measurements()
      } else {
        # use measurements to plot data
        ms_obj <- NULL

        if (is.null(chart))
          stop("You must pass 'chart' type when using measurements")
      }

      ms_obj_data <- mgr$get_data(measurements=chart_ms,
        chr=chart_chr, start=chart_start, end=chart_end)

      .self$data <- ms_obj_data$data

      if (is.null(datasource_name)) datasource_name <- "chart"

      # initialize  ---------------------------
      callSuper(data_mgr=mgr,
        name=chart_type_to_tag_name(ms_obj, chart),
        class="charts",
        id=sprintf("%s_%d", datasource_name,  sample.int(1e9, 1)),
        measurements=ms_obj_data$measurements,
        chr=chart_chr,
        start=chart_start,
        end=chart_end)

      if (!is.null(parent)) parent$append_chart(.self)

      invisible(.self)
    },
    get_data = function() {
      "Get chart data"
      .self$data
    },
    get_colors = function() {
      "Get chart colors"
      .self$colors
    },
    get_settings = function() {
      "Get chart settings"
      .self$settings
    },
    set_data = function(data) {
      "Set chart data"
      .self$data <- data
      invisible()
    },
    set_colors = function(colors) {
      "Set chart colors"
      .self$colors <- colors
    },
    set_settings = function(settings) {
      "Set chart settings"
      .self$settings <- settings
    },
    get_attributes = function() {
      c(list(data=json_writer(.self$data),
        colors=.self$colors,
        settings=.self$settings),
        callSuper())
    },
    renderChart = function() {
     tag(.self$name, .self$get_attributes())
    },
    revisualize = function(chart_type) {
      tag_name <- chart_type_to_tag_name(.self$obj, chart_type)
      .self$set_name(tag_name)

      invisible(.self)
    },
    navigate = function(chr, start, end) {
      "Navigate a chart to a genomic location"
      .self$set_chr(chr)
      .self$set_start(start)
      .self$set_end(end)

      ms_data <- .self$data_mgr$get_data(measurements=.self$get_measurements(),
        chr=chr, start=start, end=end)$data
      .self$set_data(ms_data)

      invisible(.self)
    }
  )
)
