setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("NumericOrNULL", c("numeric", "NULL"))
setClassUnion("LogicalOrNULL", c("logical", "NULL"))
#' Parent data container for an epiviz chart.
#'
#' @field data_mgr EpivizChartDataMgr.
#' @field name Character string representing an epiviz chart type (tag name).
#' @field class Character string of an epiviz chart's class attribute.
#' @field id Character string of an epiviz chart's id attribute.
#' @field chr (CharacterOrNULL) chromosome to to display in environment plot.
#' @field start (NumericOrNULL) start location to display in environment plot.
#' @field end (NumericOrNULL) end location to to display in environment plot.
#' @field measurements Character string of an epiviz chart's measurements.
#'
#' @import htmltools
#' @importFrom methods new
#' @exportClass EpivizPolymer
EpivizPolymer <- setRefClass("EpivizPolymer",
  fields=list(
    data_mgr="ANY",
    name="character",
    class="CharacterOrNULL",
    id="CharacterOrNULL",
    measurements="CharacterOrNULL",
    chr="CharacterOrNULL",
    start="NumericOrNULL",
    end="NumericOrNULL"
  ),
  methods=list(
    initialize = function(data_mgr=NULL, name=NULL, class=NULL,
      id=NULL, measurements=NULL, chr=NULL, start=NULL, end=NULL) {
      if (is.null(data_mgr)) {
        .self$data_mgr <- EpivizChartDataMgr()
      } else {
        .self$data_mgr <- data_mgr
      }
      .self$name <- name
      .self$class <- class
      .self$id <- id
      .self$measurements <- measurements
      .self$chr <- chr
      .self$start <- start
      .self$end <- end

      invisible(.self)
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
    get_attributes = function() {
      list(class=.self$class,
        id=.self$id,
        measurements=.self$measurements,
        chr=.self$chr,
        start=.self$start,
        end=.self$end)
    },
    show = function() {
      if (isTRUE(getOption('knitr.in.progress'))) {
        knitr::knit_print(attachDependencies(.self$renderChart(),
            c(chart_dependencies())))

      } else {
        # temporary directory for output
        tmp_dir <- tempfile(pattern=paste0("epivizrChart", "_", .self$get_id()))
        dir.create(tmp_dir)

        # output file
        index_html <- file.path(tmp_dir, "index.html")

        # save file
        save_html(attachDependencies(.self$renderChart(),
          c(chart_dependencies())), file=index_html)
        # on.exit(unlink(index_html))

        # view
        viewer <- getOption("viewer", utils::browseURL)
        viewer(index_html)

        invisible()
      }
    }
  )
)
