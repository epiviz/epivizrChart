setClassUnion("NumericOrNULL", c("numeric", "NULL"))
setClassUnion("LogicalOrNULL", c("logical", "NULL"))
setClassUnion("CharacterOrNULL", c("character", "NULL"))
#' Parent data container for an epiviz chart.
#'
#' @field data_mgr EpivizChartDataMgr.
#' @field name Character string of an epiviz chart type.
#' @field class Character string of an epiviz chart's class attribute.
#' @field id Character string of an epiviz chart's id attribute.
#' @field data Character string of an epiviz chart's data attribute.
#' @field measurements .
#' @field tag An object of class \code{\link[htmltools]{shiny.tag}} representing an epiviz chart in html.
#'
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
      id=NULL, measurements=NULL, data=NULL, epiviz_tag=NULL) {
      .self$data_mgr <- data_mgr
      .self$name <- name
      .self$class <- class
      .self$id <- id
      .self$measurements <- measurements
      .self$data <- data
      .self$tag <- attachDependencies(epiviz_tag, c(chart_dependencies()))

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
      if (isTRUE(getOption('knitr.in.progress'))) {
        return(knit_print(.self))

      } else {
        # temporary directory for output
        tmp_dir <- tempfile(pattern=paste0("epivizrChart", "_", .self$get_id()))
        dir.create(tmp_dir)

        # output file
        index_html <- file.path(tmp_dir, "index.html")

        # save file
        save_html(.self$get_tag(), file=index_html)
        # on.exit(unlink(index_html))

        # server <- epivizrServer::createServer(static_site_path=tmp_dir, try_ports=TRUE)
        # server$start_server()
        # TODO: Need to stop server at some point?

        # view
        viewer <- getOption("viewer", utils::browseURL)
        viewer(index_html)

        invisible()
      }
    }
  )
)
