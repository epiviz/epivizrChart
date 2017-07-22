#' Data container for an Epiviz chart component.
#'
#' @field data (list) Values of an epiviz chart's data attribute.
#' @field colors (CharacterOrNULL) Epiviz chart's colors attribute.
#' @field settings (ListOrNULL) Epiviz chart's settings attribute.
#' @field parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} where chart is appended.
#' @import epivizrData
#' @import htmltools
#' @importFrom methods new
EpivizChart <- setRefClass("EpivizChart",
  contains="EpivizPolymer",
  fields=list(
    data="list",
    colors="CharacterOrNULL",
    settings="ListOrNULL",
    parent="ANY"
  ),
  methods=list(
    initialize=function(data=NULL, colors=NULL, settings=NULL, parent=NULL, ...) {
      .self$data <- data
      .self$colors <- colors

      # override default settings
      chart_settings <- .self$get_default_settings()
      for (setting in names(settings)) {
        if (setting %in% names(chart_settings))
          chart_settings[[setting]] <- settings[[setting]]
      }

      .self$settings <- chart_settings
      .self$parent <- parent
      .self$set_class("charts")
      .self$set_id(rand_id("chart"))

      callSuper(...)
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
    get_chart_type = function() {
      "Get chart type"
      .self$chart_type
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
      # override settings
      chart_settings <- .self$get_settings()
      for (setting in names(settings)) {
        if (setting %in% names(chart_settings))
          chart_settings[[setting]] <- settings[[setting]]
      }
      .self$settings <- chart_settings
    },
    get_parent = function() {
      "Get parent of EpivizChart"
      .self$parent
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
    # revisualize = function(chart_type) {
    #   "Revisualize chart as the given chart type
    #   \\describe{
    #     \\item{chart_type}{The type of chart to be visualized
    #     (BlocksTrack, HeatmapPlot, LinePlot, LineTrack, ScatterPlot,
    #     StackedLinePlot, StackedLineTrack)}
    #   }"
    #   TODO
    # },
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
    },
    get_available_settings = function() {
      chart_defaults <- chart_default_settings_colors(.self$get_name())
      print(.settings_as_df(chart_defaults$settings))
    }
  )
)
