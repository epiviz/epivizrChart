#' Data container for an Epiviz chart component.
#'
#' @field data (list) Values of an epiviz chart's data attribute.
#' @field colors (CharacterOrNULL) Epiviz chart's colors attribute.
#' @field settings (ListOrNULL) Epiviz chart's settings attribute.
#' @field parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} where chart is appended.
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
    settings="ListOrNULL",
    parent="ANY"
  ),
  methods=list(
    initialize = function(data_obj=NULL, datasource_name=NULL, parent=NULL,
      datasource_obj_name=NULL, measurements=NULL, chart=NULL, chr=NULL,
      start=NULL, end=NULL, settings=NULL, colors=NULL, ...) {
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

      if (is.null(datasource_name)) datasource_name <- "epivizChart"

      # initialize  ---------------------------
      callSuper(data_mgr=mgr,
        name=chart_type_to_tag_name(ms_obj, chart),
        class="charts",
        id=rand_id(datasource_name),
        measurements=ms_obj_data$measurements,
        chr=chart_chr,
        start=chart_start,
        end=chart_end)

      # chart is appended at this point because id needs to be initialized
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
      "Get attributes for rendering chart. Fields that need to be in JSON
      will be converted"
      c(list(data=json_writer(.self$data),
        colors=json_writer(.self$colors),
        settings=json_writer(.self$settings)),
        callSuper())
    },
    renderChart = function() {
      "Render to html"
     tag(.self$name, .self$get_attributes())
    },
    revisualize = function(chart_type) {
      "Revisualize chart as the given chart type
      \\describe{
        \\item{chart_type}{The type of chart to be visualized
        (BlocksTrack, HeatmapPlot, LinePlot, LineTrack, ScatterPlot,
        StackedLinePlot, StackedLineTrack)}
      }"
      tag_name <- chart_type_to_tag_name(.self$obj, chart_type)
      .self$set_name(tag_name)

      invisible(.self)
    },
    navigate = function(chr, start, end) {
      "Navigate chart to a genomic location
      \\describe{
        \\item{chr}{Chromosome}
        \\item{start}{Start location}
        \\item{end}{End location}
      }"
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
