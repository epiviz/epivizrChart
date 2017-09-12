setClassUnion("NumericOrNULL", c("numeric", "NULL"))
setClassUnion("ListOrNULL", c("list", "NULL"))
#' Data container for an Epiviz web component.
#'
#' @field chr (CharacterOrNULL) Chromosome location.
#' @field start (NumericOrNULL) Start location.
#' @field end (NumericOrNULL) End location.
#' @field measurements (ListOrNULL) list of measurements of class \code{\link[epivizrData]{EpivizMeasurement}}.
#' @import htmltools
#' @importFrom methods new
EpivizViewComponent <- setRefClass("EpivizViewComponent",
  contains="EpivizWebComponent",
  fields=list(
    measurements="ListOrNULL",
    chr="CharacterOrNULL",
    start="NumericOrNULL",
    end="NumericOrNULL"
  ),
  methods=list(
    initialize=function(measurements=NULL, chr=NULL,
      start=NULL, end=NULL, ...) {

      .self$measurements <- measurements
      .self$chr <- chr
      .self$start <- start
      .self$end <- end

      callSuper(...)
    },
    get_measurements=function() {
      "Get measurements"
      .self$measurements
    },
    get_chr=function() {
      "Get chromosome"
      .self$chr
    },
    get_start=function() {
      "Get start"
      .self$start
    },
    get_end=function() {
      "Get end"
      .self$end
    },
    set_measurements=function(ms) {
      "Set measurements"
      .self$measurements <- ms
      invisible()
    },
    set_chr=function(chr) {
      "Set the chromosome"
      .self$chr <- chr
      invisible()
    },
    set_start=function(start) {
      "Set start"
      .self$start <- start
      invisible()
    },
    set_end=function(end) {
      "Set end"
      .self$end <- end
      invisible()
    },
    get_attributes=function() {
      "Get attributes for rendering chart"
      list(measurements=json_writer(lapply(.self$measurements, as.list)),
        chr=.self$chr, start=.self$start, end=.self$end, callSuper())
    },
    get_dependencies=function(knitr=FALSE) {
      polymer_lib <- system.file(package="epivizrChart",
        "www", "lib/polymer/", "epiviz-charts.html")

      if(!knitr) {
        polymer_lib <- "lib/epiviz-charts-1/epiviz-charts.html"
      }

      list(
        webcomponents=htmlDependency(
          name="webcomponents",
          version="0.7.24",
          src=system.file(package="epivizrChart", "www", "lib/webcomponents"),
          script="webcomponents-lite.js"),
        polymer=htmlDependency(
          name="epiviz-charts",
          version="1",
          head=paste0("<link rel='import' href='",  polymer_lib, "'>"),
          src=system.file(package="epivizrChart", "www", "lib/polymer"),
          all_files=TRUE)
      )
    }
  )
)
