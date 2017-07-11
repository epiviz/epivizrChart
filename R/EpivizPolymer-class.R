setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("NumericOrNULL", c("numeric", "NULL"))
setClassUnion("ListOrNULL", c("list", "NULL"))
#' Data container for an epiviz web component.
#'
#' @field data_mgr \code{\link[epivizrChart]{EpivizChartDataMgr}}
#' @field name (character) Epiviz chart type (tag name).
#' @field class (CharacterOrNULL) Epiviz chart's class attribute.
#' @field id (CharacterOrNULL) Epiviz chart's id attribute.
#' @field chr (CharacterOrNULL) Chromosome location.
#' @field start (NumericOrNULL) Start location.
#' @field end (NumericOrNULL) End location.
#' @field measurements (ListOrNULL) list of measurements of class \code{\link[epivizrData]{EpivizMeasurement}}.
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
    measurements="ListOrNULL",
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
      "Get attributes for rendering chart"
      list(class=.self$class,
        id=.self$id,
        measurements=json_writer(lapply(.self$measurements, as.list)),
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

        # view
        viewer <- getOption("viewer", utils::browseURL)
        viewer(index_html)

        invisible()
      }
    }
  )
)
