setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("NumericOrNULL", c("numeric", "NULL"))
setClassUnion("LogicalOrNULL", c("logical", "NULL"))

#' Parent data container for an epiviz chart.
#'
#' @field data_mgr EpivizChartDataMgr.
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
    data_mgr="ANY",
    name="character",
    class="CharacterOrNULL",
    id="CharacterOrNULL",
    measurements="CharacterOrNULL",
    data="CharacterOrNULL",
    tag="ANY"
  ),
  methods=list(
    initialize = function(data_mgr=EpivizChartDataMgr(), name=NULL, class=NULL,
      id=NULL, measurements=NULL, data=NULL, tag=NULL) {
      .self$data_mgr <- data_mgr
      .self$name <- name
      .self$class <- class
      .self$id <- id
      .self$measurements <- measurements
      .self$data <- data
      
      # attach dependencies to tag
      # TODO: fix version numbers
      webcomponents <- htmlDependency(
        name="webcomponents",
        version="1",
        src=c(href="https://epiviz.github.io/polymer/charts/components/webcomponentsjs/"),
        script="webcomponents-lite.js"
      )
      
      epiviz_charts <- htmlDependency(
        name="epiviz-charts",
        version="1",
        src=c(href="https://epiviz.github.io/polymer"),
        import="epiviz-charts.html"
      )
      
      epiviz_data_source <- htmlDependency(
        name="epiviz-data-source",
        version="1",
        src=c(href="https://epiviz.github.io/polymer/charts/components/epiviz-data-source"),
        import="epiviz-data-source.html"
      )
      
      dependencies <- list(webcomponents, epiviz_charts, epiviz_data_source)
      for (dep in dependencies) {
        tag <- htmltools::attachDependencies(tag, dep, TRUE)
      }
      
      .self$tag <- tag
      
      invisible(.self)
    },
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
    get_data_mgr = function() {
      "Return EpivizChart Data Manager"
      .self$data_mgr
    },
    get_name = function() {
      "Get name"
      .self$name
    },
    get_class = function() {
      "Get class"
      .self$class
    },
    get_id = function() {
      "Get id"
      .self$id
    },
    get_measurements = function() {
      "Get measurements"
      .self$measurements
    },
    get_data = function() {
      "Get data"
      .self$data
    },
    get_tag = function() {
      "Get tag"
      .self$tag
    },
    show = function() {
      "Show tag of this object"
      # TODO
      
      # knit_print.html(.self$tag)
    }
  )
)
