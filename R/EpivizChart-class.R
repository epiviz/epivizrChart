setClassUnion("EpivizEnvOrNULL", c("EpivizEnvironment", "NULL"))
setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("ListOrNULL", c("list", "NULL"))
#' Data container for an Epiviz chart component.
#'
#' @field data (list) Values of an epiviz chart's data attribute.
#' @field colors (character) Epiviz chart's colors attribute.
#' @field settings (list) Epiviz chart's settings attribute.
#' @field parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} where chart is appended.
#' @import epivizrData
#' @import htmltools
#' @importFrom methods new
EpivizChart <- setRefClass("EpivizChart",
  contains="EpivizViewComponent",
  fields=list(
    data="list",
    colors="CharacterOrNULL",
    settings="ListOrNULL",
    parent="EpivizEnvOrNULL"
  ),
  methods=list(
    initialize=function(data=NULL, colors=NULL,
      settings=NULL, parent=NULL, ...) {
      .self$data <- data

      .self$colors <- .self$get_default_colors()
      .self$set_colors(colors)

      .self$settings <- .self$get_default_settings()
      .self$set_settings(settings)

      .self$parent <- parent

      callSuper(class="charts", ...)
    },
    get_data=function() {
      "Get chart data"
      .self$data
    },
    get_colors=function() {
      "Get chart colors"
      .self$colors
    },
    get_settings=function() {
      "Get chart settings"
      .self$settings
    },
    get_chart_type=function() {
      "Get chart type"
      .self$chart_type
    },
    set_data=function(data) {
      "Set chart data"
      .self$data <- data
      invisible()
    },
    set_colors=function(colors) {
      "Set chart colors"
      .self$colors[seq_len(length(colors))] <- colors
      invisible()
    },
    set_settings=function(settings) {
      "Modify current settings
      \\describe{
        \\item{settings}{List of new settings.
          Call get_available_settings for settings available to modify.
        }
      }"
      chart_settings <- .self$settings
      for (setting in names(settings)) {
        if (setting %in% names(chart_settings))
          chart_settings[[setting]] <- settings[[setting]]
      }
      .self$settings <- chart_settings

      invisible()
    },
    get_parent=function() {
      "Get parent"
      .self$parent
    },
    get_attributes=function() {
      "Get attributes for rendering component"
      c(list(data=json_writer(.self$data),
        colors=json_writer(.self$colors),
        settings=json_writer(.self$settings)),
        callSuper())
    },
    render_component=function() {
      "Render to html"
      tag(.self$get_name(), .self$get_attributes())
    },
     revisualize=function(chart_type) {
       "Revisualize chart as the given chart type
       \\describe{
         \\item{chart_type}{The type of chart to be visualized
         (BlocksTrack, HeatmapPlot, LinePlot, LineTrack, ScatterPlot,
         StackedLinePlot, StackedLineTrack)}
       }"
       epiviz_chart <- .initialize_chart(
         chart_type=chart_type,
         data_mgr=.self$get_data_mgr(),
         measurements=.self$get_measurements(),
         data=.self$data,
         chr=.self$get_chr(),
         start=.self$get_start(),
         end=.self$get_end(),
         settings=.self$settings,
         colors=.self$colors,
         parent=.self$parent)

       if (!is.null(.self$parent))
         .self$parent$append_chart(epiviz_chart)

       epiviz_chart
     },
    navigate=function(chr, start, end) {
      "Navigate chart to a genomic location
      \\describe{
        \\item{chr}{Chromosome}
        \\item{start}{Start location}
        \\item{end}{End location}
      }"
      .self$set_chr(chr)
      .self$set_start(start)
      .self$set_end(end)

      ms_data <- .self$data_mgr$get_data(
        measurements=.self$get_measurements(),
        chr=chr, start=start, end=end)

      .self$data <- ms_data$data

      invisible(.self)
    },
    get_available_settings=function() {
      "Get available settings"
      chart_defaults <- chart_default_settings_colors(
        .self$get_chart_type())

      print(.settings_as_df(chart_defaults$settings))
    },
    get_dependencies=function(knitr=FALSE) {
      # TODO
      # c(list(charts=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper(knitr))
      callSuper(knitr)
    }
  )
)
