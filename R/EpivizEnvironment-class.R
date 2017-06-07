#' Epiviz Environment Class
#' 
#' @field chr (character) chromosome to to display in environment plot.
#' @field start (integer) start location to display in environment plot.
#' @field end (integer) end location to to display in environment plot.
#' @field env An object of class \code{htmltools}{shiny.tag} used to nest chart tags in epiviz-environment tag.
#' @import htmltools
#' @exportClass EpivizChart
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  fields=list(
    chr="character",
    start="numeric",
    end="numeric",
    env="ANY" 
  ),
  methods=list(
    add = function(...) {
      "Add chart to environment"
      for (chart in ...) {
        if (!is(chart, "EpivizChart")) {
          stop("All arguments must be an 'EpivizChart' object")
        }
        .self$.env <- tagAppendChild(.self$env, chart_object)
      }

      return(.self)
    },
    show = function() {
      "Show environment of this object"
      knit_print.shiny.tag(.self$env)
    }
  )
)