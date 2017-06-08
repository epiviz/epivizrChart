#' Epiviz Environment Class
#'
#' @field chr (character) chromosome to to display in environment plot.
#' @field start (integer) start location to display in environment plot.
#' @field end (integer) end location to to display in environment plot.
#' @field range
#' @field initializeRegions
#' @import htmltools
#' @exportClass EpivizEnvironment
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  contains="EpivizPolymer",
  fields=list(
    chr="character",
    start="numeric",
    end="numeric",
    range="character",
    initializeRegions="character"
  ),
  methods=list(
    add = function(polymer_obj) {
      "Add chart or navigation to environment"
      if (!is(polymer_obj, "EpivizPolymer")) {
        stop(polymer_obj, " must be an 'EpivizPolymer' object")
      } else {
        .self$tag <- tagAppendChild(.self$tag, polymer_obj$get_tag())
      }
      return(.self)
    }
  )
)
