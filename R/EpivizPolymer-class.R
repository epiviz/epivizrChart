#' Parent data container for an epiviz chart.
#'
#' @field name Character string of an epiviz chart type.
#' @field class Character string of an epiviz chart's class attribute.
#' @field id Character string of an epiviz chart's id attribute.
#' @field measurement Character string of an epiviz chart's measurement attribute.
#' @field data Character string of an epiviz chart's data attribute.
#' @field tag An object of class \code{\link[htmltools]{shiny.tag}} representing an epiviz chart in html.
#' @import htmltools
#' @exportClass EpivizPolymer
EpivizPolymer <- setRefClass("EpivizPolymer",
  fields=list(
    name="character",
    class="character",
    id="character",
    measurements="character",
    data="character",
    tag="ANY"
  ),
  methods=list(
    set_name = function(name) {
      "Set name"
      .self$name <- name
      invisible()
    },
    set_class = function(class) {
      "Set chart class"
      .self$class <- class
      invisible()
    },
    set_id = function(id) {
      "Set chart id"
      .self$id <- id
      invisible()
    },
    set_measurements = function(ms) {
      "Set measurements"
      .self$measurements <- ms
      invisible()
    },
    set_data = function(data) {
      "Set data"
      .self$data <- data
      invisible()
    },
    set_tag = function(epiviz_tag) {
      "Set tag"
      .self$tag <- epiviz_tag
      invisible()
    },
    get_name = function() {
      "Get name"
      return(.self$name)
    },
    get_class = function() {
      "Get class"
      return(.self$class)
    },
    get_id = function() {
      "Get id"
      return(.self$id)
    },
    get_measurements = function() {
      "Get measurements"
      return(.self$measurements)
    },
    get_data = function() {
      "Get data"
      return(.self$data)
    },
    get_tag = function() {
      "Get tag"
      return(.self$tag)
    },
    show = function() {
      "Show tag of this object"
      return(.self$tag)
    }
  )
)
