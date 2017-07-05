#' Data container for an epiviz chart.
#'
#' @field obj An object of class \code{\link[epivizrData]{EpivizData}}.
#' @field data Character string of an epiviz chart's data attribute.
#' @field colors Character string of an epiviz chart's colors attribute.
#' @field settings character string of an epiviz chart's settings attribute.
#' @field parent An EpivizEnvironment where the chart is nested or NULL
#' @import epivizrData
#' @import htmltools
#' @export EpivizChart
#' @exportClass EpivizChart
EpivizChart <- setRefClass("EpivizChart",
  contains="EpivizPolymer",
  fields=list(
    obj="ANY",
    data="CharacterOrNULL",
    colors="CharacterOrNULL",
    settings="CharacterOrNULL",
    parent="ANY"
  ),
  methods=list(
    initialize = function(data_object, datasource_name=NULL,
      datasource_origin_name=NULL, parent=NULL, chart=NULL,
      chr=NULL, start=NULL, end=NULL, settings=NULL, colors=NULL, ...) {
      .self$colors <- colors
      .self$settings <- settings

      chart_chr <- chr
      chart_start <- start
      chart_end <- end

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
      if (is.null(datasource_origin_name))
        datasource_origin_name <- deparse(substitute(data_object))
      if (is.null(datasource_name)) datasource_name <- datasource_origin_name

      ms_obj <- mgr$add_measurements(
        data_object, datasource_name=datasource_name,
        datasource_origin_name=datasource_origin_name,
        ...
      )
      .self$obj <- ms_obj

      ms_obj_json <- ms_obj$toJSON(chart_chr, chart_start, chart_end)
      .self$data <- ms_obj_json$data

      # initialize  ---------------------------
      callSuper(data_mgr=mgr,
        name=chart_type_to_tag_name(ms_obj, chart),
        class="charts",
        id=ms_obj$get_id(),
        measurements=ms_obj_json$measurements,
        chr=chart_chr,
        start=chart_start,
        end=chart_end)

      if (!is.null(parent)) parent$append_child(.self)

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
      c(list(data=.self$data,
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
    }
  )
)
