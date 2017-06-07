#' Epiviz Navigation Class
#' 
#' @field chr (character) chromosome to to display in environment plot.
#' @field start (integer) start location to display in environment plot.
#' @field end (integer) end location to to display in environment plot.
#' @field nav An object of class \code{htmltools}{shiny.tag} used to nest chart tags in epiviz-navigation tag.
#' @import htmltools
#' @exportClass EpivizChart
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  fields=list(
    chr="character",
    start="numeric",
    end="numeric",
    nav="ANY" 
  ),
  methods=list(
    add = function(chart_object) {
      "Add chart to navigation"
      if (!is(chart, "EpivizChart")) {
        stop("'chart_object' must be an 'EpivizChart' object")
      }
      .self$.env <- tagAppendChild(.self$nav, chart_object)
      
      return(.self)
    },
    show = function() {
      "Show environment of this object"
      knit_print.shiny.tag(.self$nav)
    }
  )