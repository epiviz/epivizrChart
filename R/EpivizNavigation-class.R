#' Data container for an Epiviz navigation component.
#'
#' @field gene (character) Gene
#' @field geneInRange (character) Nearest Gene in range.
#' @field parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} where navigation is appended.
#' @import htmltools
#' @importFrom methods new
EpivizNavigation <- setRefClass("EpivizNavigation",
  contains="EpivizEnvironment",
  fields=list(
    gene = "CharacterOrNULL",
    geneInRange = "CharacterOrNULL",
    parent="EpivizEnvOrNULL"
  ),
  methods=list(
    initialize = function(gene=NULL, geneInRange=NULL, parent=NULL,
      chr=NULL, start=NULL, end=NULL, ...) {
      .self$gene <- gene
      .self$geneInRange <- geneInRange
      .self$parent <- parent

      # check if region is provided
      if (is.null(chr) || is.null(start) || is.null(end)) {
        stop("EpivizNavigation must have a region: chr, start, and end")
      }

      .self$set_class("charts")
      .self$set_id(rand_id("EpivizNav"))

      callSuper(chr=chr, start=start, end=end,...)
    },
    set_gene = function(gene) {
      "Set gene"
      .self$gene <- gene
      invisible()
    },
    set_geneInRange = function(gene) {
      "Set step ratio"
      .self$geneInRange <- gene
      invisible()
    },
    get_name = function() {
      "Get name of Epiviz Web Component"
      return("epiviz-navigation")
    },
    get_gene = function() {
      "Get gene"
      .self$gene
    },
    get_geneInRange = function() {
      "Get gene in range"
      .self$geneInRange
    },
    get_attributes = function() {
      "Get attributes for rendering chart"
      c(list(gene=.self$gene, geneInRange=.self$geneInRange), callSuper())
    },
    clone_charts = function(charts) {
      "Clone EpivizCharts and append to navigation. Each chart must already
      exist in the navigation's data manager, otherwise an error will occur
      when attempting to intialize using their measurements
      \\describe{
      \\item{charts}{list of EpivizCharts whose data exists in the
      navigation's data manager }}"
      for (chart in charts) {
        if (!identical(chart$get_data_mgr(), .self$get_data_mgr()))
          stop(chart, " and navigation must share a data manager")

        if (is(chart, "EpivizChart")) {
          ms <- chart$get_measurements()

          epivizChart(measurements=ms,
            datasource_name=ms[[1]]@name,
            chart=chart$get_chart_type(),
            settings=chart$get_settings(),
            colors=chart$get_colors(), parent=.self)
        }
      }
    }
  )
)
