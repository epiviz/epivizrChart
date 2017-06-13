#' Epiviz Navigation Class
#'
#' @field gene
#' @field strRange
#' @field stepRatio
#' @field zoomRatio
#' @field collapsed
#' @field geneInRange
#' @field configSrc
#' @import htmltools
#' @exportClass EpivizNavigation
EpivizNavigation <- setRefClass("EpivizNavigation",
  contains="EpivizEnvironment",
  fields=list(
    gene = "character",
    strRange = "character",
    stepRatio = "numeric",
    zoomRatio = "numeric",
    collapsed = "logical",
    geneInRange = "character",
    configSrc = "character"
  ),
  methods=list(
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
      return(.self$gene)
    },
    get_strRange = function() {
      "Get range"
      return(.self$strRange)
    },
    get_stepRatio = function() {
      "Get step ratio"
      return(.self$stepRatio)
    },
    get_zoomRatio = function() {
      "Get step ratio"
      return(.self$zoomRatio)
    },
    get_collapsed = function() {
      "Get collapsed"
      return(.self$collapsed)
    },
    get_geneInRange = function() {
      "Get gene in range"
      return(.self$geneInRange)
    },
    get_configSrc = function(src) {
      "Get config"
      return(.self$configSrc)
    }
  )
)
