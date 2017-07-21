#' Data container for an Epiviz navigation component.
#'
#' @field gene (CharacterOrNULL) Gene
#' @field geneInRange (CharacterOrNULL) Nearest Gene in range.
#' @field parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} where navigation is appended.
#' @import htmltools
#' @importFrom methods new
EpivizNavigation <- setRefClass("EpivizNavigation",
  contains="EpivizEnvironment",
  fields=list(
    gene = "CharacterOrNULL",
    geneInRange = "CharacterOrNULL",
    parent="ANY"
  ),
  methods=list(
    initialize = function(chr=NULL, start=NULL, end=NULL, gene=NULL,
      geneInRange=NULL, parent=NULL, ...) {
      .self$gene <- gene
      .self$geneInRange <- geneInRange
      .self$parent <- parent

      nav_chr <- chr
      nav_start <- start
      nav_end <- end

      # if parent environment is provided, use its data manager
      if (is.null(parent)) {
        mgr <- EpivizChartDataMgr()

      } else {
        if (!is(parent, "EpivizEnvironment"))
          stop("Parent must be an EpivizEnvironment")

        mgr <- parent$get_data_mgr()

        if (is.null(chr)) nav_chr <-  parent$get_chr()
        if (is.null(start)) nav_start <- parent$get_start()
        if (is.null(end)) nav_end <- parent$get_end()
      }

      for (arg in list(nav_chr, nav_start, nav_end))
        if (is.null(arg))
          stop("EpivizNavigation must have chr, start, and end")

      callSuper(data_mgr=mgr,
        name="epiviz-navigation",
        id=rand_id("epivizNav"),
        class="charts",
        chr=nav_chr,
        start=nav_start,
        end=nav_end,
        ...)

      # nav is appended at this point because id needs to be initialized
      if (!is.null(parent)) parent$append_chart(.self)

      invisible(.self)
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
