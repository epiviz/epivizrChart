#' Data container for an Epiviz environment component.
#'
#' @field charts List of class \code{\link[epivizrChart]{EpivizPolymer}} used to track nested elements.
#' @import htmltools
#' @importFrom methods new
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  contains="EpivizPolymer",
  fields=list(
    charts="list"
  ),
  methods=list(
    initialize = function(...) {
      .self$set_name("epiviz-environment")
      .self$charts <- list()

      callSuper(...)
    },
    append_chart = function(chart) {
      "Append chart or navigation to environment"
      if (!is(chart, "EpivizPolymer"))
        stop(chart, " must be an EpivizPolymer object")

      .self$charts[[chart$get_id()]] <- chart

      invisible(.self)
    },
    plot = function (...) {
      "Initialize an EpivizChart and append to environment"
      chart <- epivizChart(parent=.self, ...)
      .self$append_chart(chart)

      invisible(chart)
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
    get_attributes = function() {
      "Get attributes for rendering chart"
      c(callSuper())
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
    },
    init_region = function(chr=NULL, start=NULL, end=NULL) {
      "Initialize navigation based on a genomic region
      \\describe{
        \\item{chr}{Chromosome}
        \\item{chr}{Start location}
        \\item{chr}{End location}
      }"
      nav <- epivizNav(chr=chr, start=start, end=end, parent=.self)
      nav$clone_charts(.self$get_charts())

      invisible(nav)
    },
    init_regions = function(regions) {
      "Initialize navigations based on a genomic regions
      \\describe{
        \\item{regions}{List of named lists of genomic locations, e.g.,
        list(list(chr='chr11', start=99800000, end=103383180))}
      }"
      for (region in regions) {
        if (is.null(names(region)))
          stop(region, " must be named with chr, start, and end")

        .self$init_region(region$chr, region$start, region$end)
      }

      invisible(.self)
    },
    add_data = function(...) {
      "Add data to environment's data manager
      \\describe{
        \\item{...}{Arguments for add_measurements and register, e.g., data,
        datasource_name, datasource_obj_name, type, etc}
      }"
      .self$data_mgr$add_measurements(...)
    }
  )
)
