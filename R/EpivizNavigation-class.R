#' Epiviz Navigation Class
#' @field chr (character) chromosome to to display in environment plot.
#' @field start (numeric) start location to display in environment plot
#' @field end (numeric) end location to to display in environment plot.
#' @field gene (CharacterOrNULL)
#' @field strRange (NumericOrNULL)
#' @field stepRatio (NumericOrNULL)
#' @field zoomRatio (NumericOrNULL)
#' @field collapsed (LogicalOrNULL)
#' @field geneInRange (CharacterOrNULL)
#' @field configSrc (CharacterOrNULL)
#' @import htmltools
#' @export EpivizNavigation
#' @exportClass EpivizNavigation
EpivizNavigation <- setRefClass("EpivizNavigation",
  contains="EpivizEnvironment",
  fields=list(
    chr="character",
    start="numeric",
    end="numeric",
    gene = "CharacterOrNULL",
    strRange = "CharacterOrNULL",
    stepRatio = "NumericOrNULL",
    zoomRatio = "NumericOrNULL",
    collapsed = "LogicalOrNULL",
    geneInRange = "CharacterOrNULL",
    configSrc = "CharacterOrNULL"
  ),
  methods=list(
    initialize = function(chr, start, end, gene=NULL, strRange=NULL,
      stepRatio=NULL, zoomRatio=NULL, collapsed=NULL, geneInRange=NULL,
      configSrc=NULL, ...) {
      .self$gene <- gene
      .self$strRange <- strRange
      .self$stepRatio <- stepRatio
      .self$zoomRatio <- zoomRatio
      .self$collapsed <- collapsed
      .self$geneInRange <- geneInRange
      .self$configSrc <- configSrc
      
      epiviz_tag <- tag("epiviz-navigation",
        list(chr=chr, start=start, end=end, 
          gene=gene, strRange=strRange, stepRatio=stepRatio, zoomRatio=zoomRatio,
          collapsed=collapsed, geneInRange=geneInRange, configSrc=configSrc))
      
      callSuper(chr=chr, start=start, end=end, epiviz_tag=epiviz_tag, ...)
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
    }
  )
)
