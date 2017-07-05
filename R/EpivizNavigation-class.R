#' Epiviz Navigation Class
#' @field gene (CharacterOrNULL)
#' @field strRange (NumericOrNULL)
#' @field stepRatio (NumericOrNULL)
#' @field zoomRatio (NumericOrNULL)
#' @field collapsed (LogicalOrNULL)
#' @field geneInRange (CharacterOrNULL)
#' @field configSrc (CharacterOrNULL)
#' @field parent An EpivizEnvironment where the chart is nested or NULL
#' @import htmltools
#' @export EpivizNavigation
#' @exportClass EpivizNavigation
EpivizNavigation <- setRefClass("EpivizNavigation",
  contains="EpivizEnvironment",
  fields=list(
    gene = "CharacterOrNULL",
    strRange = "CharacterOrNULL",
    stepRatio = "NumericOrNULL",
    zoomRatio = "NumericOrNULL",
    collapsed = "LogicalOrNULL",
    geneInRange = "CharacterOrNULL",
    configSrc = "CharacterOrNULL",
    parent="ANY"
  ),
  methods=list(
    initialize = function(chr=NULL, start=NULL, end=NULL, gene=NULL,
      strRange=NULL, stepRatio=NULL, zoomRatio=NULL, collapsed=NULL,
      geneInRange=NULL, configSrc=NULL, parent=NULL, ...) {
      for (arg in list(chr, start, end))
        if (is.null(arg)) stop(arg, " cannot be NULL for an EpivizNavigation", call.=FALSE)

      .self$gene <- gene
      .self$strRange <- strRange
      .self$stepRatio <- stepRatio
      .self$zoomRatio <- zoomRatio
      .self$collapsed <- collapsed
      .self$geneInRange <- geneInRange
      .self$configSrc <- configSrc
      .self$parent <- parent
      nav_id <- NULL

      # if parent environment is provided, use its data manager
      if (is.null(parent)) {
        mgr <- EpivizChartDataMgr()

      } else {
        if (!is(parent, "EpivizEnvironment"))
          stop("Parent must be an EpivizEnvironment")

        mgr <- parent$get_data_mgr()

        # Because navigation is being nested inside an environment,
        # it is useful to have an id (random)
        nav_id <- paste0("epivizNav_",  sample.int(1e10, 1))
      }

      callSuper(data_mgr=mgr, name="epiviz-navigation", chr=chr, start=start,
        end=end, id=nav_id, ...)

      if (!is.null(parent)) parent$append_child(.self)

      invisible(.self)
    },
    set_gene = function(gene) {
      "Set gene"
      .self$gene <- gene
      invisible()
    },
    set_strRange = function(range) {
      "Set range"
      .self$strRange <- range
      invisible()
    },
    set_stepRatio = function(ratio) {
      "Set step ratio"
      .self$stepRatio <- ratio
      invisible()
    },
    set_zoomRatio = function(ratio) {
      "Set step ratio"
      .self$zoomRatio <- ratio
      invisible()
    },
    set_collapsed = function(collapse) {
      "Set step ratio"
      .self$collapsed <- collapse
      invisible()
    },
    set_geneInRange = function(gene) {
      "Set step ratio"
      .self$geneInRange <- gene
      invisible()
    },
    set_configSrc = function(src) {
      "Set step ratio"
      .self$configSrc <- src
      invisible()
    },
    get_gene = function() {
      "Get gene"
      .self$gene
    },
    get_strRange = function() {
      "Get range"
      .self$strRange
    },
    get_stepRatio = function() {
      "Get step ratio"
      .self$stepRatio
    },
    get_zoomRatio = function() {
      "Get step ratio"
      .self$zoomRatio
    },
    get_collapsed = function() {
      "Get collapsed"
      .self$collapsed
    },
    get_geneInRange = function() {
      "Get gene in range"
      .self$geneInRange
    },
    get_configSrc = function(src) {
      "Get config"
      .self$configSrc
    },
    get_attributes = function() {
      c(list(
        gene=.self$gene,
        strRange=.self$strRange,
        stepRatio=.self$stepRatio,
        zoomRatio=.self$zoomRatio,
        collapsed=.self$collapsed,
        geneInRange=.self$geneInRange,
        configSrc=.self$configSrc),
        callSuper())
    }
  )
)
