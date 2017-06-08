#' Class for knitting epiviz charts
#'
#' @field tag An object of class \code{\link[htmltools]{shiny.tag}} representing an epiviz chart in html.
#' @import htmltools
#' @exportClass EpivizPolymer
EpivizPolymer <- setRefClass("EpivizPolymer",
  fields=list(
    chart_type="character",
    class="character",
    id="character",
    measurements="character",
    data="character",
    tag="ANY"
  ),
  methods=list(
    set_tag = function(epiviz_tag) {
      .self$tag <- epiviz_tag
      invisible()
    },
    get_tag = function() {
      return(.self$tag)
    },
    show = function() {
      "Show tag of this object"
      knit_print.shiny.tag(.self$tag)
    }
  )
)
