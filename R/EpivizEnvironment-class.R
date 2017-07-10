#' Epiviz Environment Class
#'
#' @field range (CharacterOrNULL)
#' @field initializeRegions (CharacterOrNULL)
#' @field charts A list of EpivizChart/EpivizNavigation elements.
#' @import htmltools
#' @export EpivizEnvironment
#' @importFrom methods new
#' @exportClass EpivizEnvironment
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  contains="EpivizPolymer",
  fields=list(
    range="CharacterOrNULL",
    initializeRegions="CharacterOrNULL",
    charts="list"
  ),
  methods=list(
    initialize = function(name="epiviz-environment", chr=NULL, start=NULL, end=NULL, range=NULL,
      initializeRegions=NULL, ...) {
      .self$range <- range
      .self$initializeRegions <- initializeRegions
      .self$charts <- list()

      callSuper(name=name, chr=chr, start=start, end=end, ...)
    },
    append_chart = function(chart) {
      "Append chart or navigation to environment"
      if (!is(chart, "EpivizPolymer"))
        stop(chart, " must be an EpivizPolymer object")

      .self$charts[[chart$get_id()]] <- chart

      invisible(.self)
    },
    remove_chart = function(chart) {
      "Remove chart from environment"
      if (!is(chart, "EpivizPolymer"))
        stop(chart, " must be an EpivizPolymer object")

      chart_id <- chart$get_id()
      # TODO: Remove ms_obj from data manager if
      # there are no more charts using its data

      .self$charts[[chart_id]] <- NULL

      invisible(.self)
    },
    remove_all_charts = function() {
      "Remove all charts from environment"
      for (chart in .self$charts)
        .self$remove_chart(chart)

      invisible(.self)
    },
    get_range = function() {
      "Get range"
      .self$range
    },
    get_initializeRegions = function() {
      "Get initializeRegions"
      .self$initializeRegions
    },
    set_range = function(range) {
      "Set range"
      .self$range <- range
      invisible()
    },
    set_initializeRegions = function(initReg) {
      "Set initializeRegions"
      .self$initializeRegions <- initRegs
      invisible()
    },
    get_attributes = function() {
      "Get attributes for rendering chart. Fields that need to be in JSON
      will be converted"
      c(list(range=.self$range,
        initializeRegions=.self$initializeRegions),
        callSuper()
        )
    },
    renderChart = function() {
      "Render to html"
      tagSetChildren(tag=tag(.self$name, .self$get_attributes()),
        list=lapply(.self$charts, function(chart) chart$renderChart())
      )
    },
    navigate = function(chr = NULL, start = NULL, end = NULL) {
      "Navigate environment to a genomic location"
      .self$chr <- chr
      .self$start <- start
      .self$end <- end

      for (chart in .self$charts) {
        chart$navigate(chr, start, end)
      }
    }
  )
)
