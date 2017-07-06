#' Epiviz Environment Class
#'
#' @field range (CharacterOrNULL)
#' @field initializeRegions (CharacterOrNULL)
#' @field children A list of EpivizChart/EpivizNavigation elements.
#' @import htmltools
#' @export EpivizEnvironment
#' @exportClass EpivizEnvironment
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  contains="EpivizPolymer",
  fields=list(
    range="CharacterOrNULL",
    initializeRegions="CharacterOrNULL",
    children="list"
  ),
  methods=list(
    initialize = function(name="epiviz-environment", chr=NULL, start=NULL, end=NULL, range=NULL,
      initializeRegions=NULL, ...) {
      .self$range <- range
      .self$initializeRegions <- initializeRegions
      .self$children <- list()

      callSuper(name=name, chr=chr, start=start, end=end, ...)
    },
    append_child = function(child) {
      "Append chart or navigation to environment"
      if (!is(child, "EpivizPolymer"))
        stop(child, " must be an EpivizPolymer object")

      .self$children[[child$get_id()]] <- child

      invisible(.self)
    },
    remove_chart = function(child) {
      "Remove chart from environment"
      if (!is(child, "EpivizPolymer"))
        stop(child, " must be an EpivizPolymer object")

      if (is(child, "EpivizChart"))
        .self$data_mgr$rm_measurements(child$get_id())

      .self$children[[child$get_id()]] <- NULL

      invisible(.self)
    },
    remove_all_charts = function() {
      "Remove all charts from environment"
      for (child in .self$children)
        .self$remove_chart(child)

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
      c(list(range=.self$range,
        initializeRegions=.self$initializeRegions),
        callSuper()
        )
    },
    renderChart = function() {
      tagSetChildren(tag=tag(.self$name, .self$get_attributes()),
        list=lapply(.self$children, function(child) child$renderChart())
      )
    }
  )
)
