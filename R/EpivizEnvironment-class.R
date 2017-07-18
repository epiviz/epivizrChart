#' Data container for an Epiviz environment component.
#'
#' @field initializeRegions (ListOrNULL) List of gene names or regions to intialize navigations.
#' @field charts List of class \code{\link[epivizrChart]{EpivizPolymer}} used to track nested elements.
#' @import htmltools
#' @importFrom methods new
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  contains="EpivizPolymer",
  fields=list(
    initializeRegions="ListOrNULL",
    charts="list"
  ),
  methods=list(
    initialize = function(name="epiviz-environment", chr=NULL, start=NULL, end=NULL,
      initializeRegions=NULL, ...) {
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
    get_initializeRegions = function() {
      "Get initializeRegions"
      .self$initializeRegions
    },
    set_initializeRegions = function(initReg) {
      "Set initializeRegions"
      .self$initializeRegions <- initRegs
      invisible()
    },
    get_attributes = function() {
      "Get attributes for rendering chart"
      c(list(initializeRegions=json_writer(.self$initializeRegions)),
        callSuper())
    },
    get_charts = function() {
      "Get charts within environment"
      .self$charts
    },
    set_charts = function(charts) {
      .self$charts <- charts
    },
    renderChart = function() {
      "Render to html"
      tagSetChildren(tag=tag(.self$name, .self$get_attributes()),
        list=lapply(.self$charts, function(chart) chart$renderChart())
      )
    },
    navigate = function(chr=NULL, start=NULL, end=NULL) {
      "Navigate environment to genomic location
      \\describe{
        \\item{chr}{Chromosome}
        \\item{start}{Start location}
        \\item{end}{End location}
      }"
      .self$chr <- chr
      .self$start <- start
      .self$end <- end

      for (chart in .self$charts) {
        chart$navigate(chr, start, end)
      }
    },
    order_charts = function(ordered_charts) {
      "Order the charts within an environment
      \\describe{
      \\item{charts}{An ordered list of EpivizPolymer objects}
      }"
      if (length(ordered_charts) != length(.self$charts))
        stop("The charts to be ordered must include all
          charts from environment")

      chart_ids <- sapply(ordered_charts, function(chart) {
        if (!identical(chart$get_data_mgr(), .self$get_data_mgr()))
          stop(chart, " must be from the environment")

        chart$get_id()
      })

      names(ordered_charts) <- chart_ids
      .self$set_charts(ordered_charts)
    }
  )
)
