#' Epiviz Environment Class
#'
#' @field chr (CharacterOrNULL) chromosome to to display in environment plot.
#' @field start (NumericOrNULL) start location to display in environment plot.
#' @field end (NumericOrNULL) end location to to display in environment plot.
#' @field range (CharacterOrNULL)
#' @field initializeRegions (CharacterOrNULL)
#' @import htmltools
#' @export EpivizEnvironment
#' @exportClass EpivizEnvironment
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  contains="EpivizPolymer",
  fields=list(
    chr="CharacterOrNULL",
    start="NumericOrNULL",
    end="NumericOrNULL",
    range="CharacterOrNULL",
    initializeRegions="CharacterOrNULL"
  ),
  methods=list(
    initialize = function(chr=NULL, start=NULL, end=NULL, range=NULL, 
      initializeRegions=NULL, ...) {
      
      .self$chr <- chr
      .self$start <- start
      .self$end <- end
      .self$range <- range
      .self$initializeRegions <- initializeRegions
      
      epiviz_env <- tag("epiviz-environment",
        list(chr=chr, start=start, end=end, 
          range=range, initializeRegions=initializeRegions))
      
      callSuper(name=epiviz_env$name, tag=epiviz_env, ...)
    },
    append_child = function(polymer_obj) {
      "Append chart or navigation to environment"
      if (!is(polymer_obj, "EpivizPolymer")) {
        stop(polymer_obj, " must be an 'EpivizPolymer' object")
      } else {
        .self$tag <- tagAppendChild(.self$tag, polymer_obj$get_tag())
      }
      invisible(.self)
    },
    set_chr = function(chr) {
      "Set the chromosome"
      .self$chr <- chr
      invisible()
    },
    set_start = function(start) {
      "Set start"
      .self$start <- start
      invisible()
    },
    set_end = function(end) {
      "Set end"
      .self$end <- end
      invisible()
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
    get_chr = function() {
      "Get chromosome"
      .self$chr
    },
    get_start = function() {
      "Get start"
      .self$start
    },
    get_end = function() {
      "Get end"
      .self$end
    },
    get_range = function() {
      "Get range"
      .self$range
    },
    get_initializeRegions = function() {
      "Get initializeRegions"
      .self$initializeRegions
    }
  )
)
