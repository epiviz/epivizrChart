#' Data container for an Epiviz Genes Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizGenesTrack <- setRefClass("EpivizGenesTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-genes-track")

      callSuper(...)
    },
    # get_dependencies=function() {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10
      )
    }
  )
)

#' Data container for an Epiviz Blocks Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizBlocksTrack <- setRefClass("EpivizBlocksTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-blocks-track")

      callSuper(...)
    },
    # get_dependencies=function() {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10,
        minBlockDistance=5,
        blockColorBy="colLabel"
      )
    }
  )
)

#' Data container for an Epiviz Heatmap Plot.
#'
#' @import htmltools
#' @importFrom methods new
EpivizHeatmapPlot <- setRefClass("EpivizHeatmapPlot",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-heatmap-plot")

      callSuper(...)
    },
    # get_dependencies=function() {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=80,
        marginBottom=40,
        marginLeft=120,
        marginRight=40,
        measurementGroupsAggregator="colLabel",
        colLabel="colLabel",
        rowLabel="name",
        showColorsForRowLabels=FALSE,
        maxColumns=120,
        yMin="default",
        yMax="default",
        cluster="rows",
        clusteringAlg="none",
        clusteringMetric="euclidean",
        clusteringLinkage="complete",
        showDendrogram=TRUE
      )
    }
  )
)

#' Data container for an Epiviz Line Plot.
#'
#' @import htmltools
#' @importFrom methods new
EpivizLinePlot <- setRefClass("EpivizLinePlot",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-line-plot")

      callSuper(...)
    },
    # get_dependencies=function() {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=30,
        marginBottom=50,
        marginLeft=30,
        marginRight=15,
        measurementGroupsAggregator="colLabel",
        colLabel="colLabel",
        rowLabel="name",
        showPoints=FALSE,
        showLines=TRUE,
        showErrorBars=TRUE,
        pointRadius=4,
        lineThickness=1.1,
        yMin="default",
        yMax="default",
        interpolation="basis",
        abLine="default"
      )
    }
  )
)

#' Data container for an Epiviz Line Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizLineTrack <- setRefClass("EpivizLineTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-line-track")

      callSuper(...)
    },
    # get_dependencies=function() {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=25,
        marginBottom=20,
        marginLeft=20,
        marginRight=10,
        measurementGroupsAggregator="mean-stdev",
        step=1,
        showPoints=FALSE,
        showLines=TRUE,
        showErrorBars=TRUE,
        pointRadius=1,
        lineThickness=1.1,
        yMin="default",
        yMax="default",
        interpolation="linear",
        abLine="default"
      )
    }
  )
)

#' Data container for an Epiviz Scatter Plot.
#'
#' @import htmltools
#' @importFrom methods new
EpivizScatterPlot <- setRefClass("EpivizScatterPlot",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-scatter-plot")

      callSuper(...)
    },
    # get_dependencies=function(knitr=FALSE) {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=15,
        marginBottom=50,
        marginLeft=50,
        marginRight=15,
        measurementGroupsAggregator="mean-stdev",
        circleRadiusRatio=0.015,
        xMin="default",
        xMax="default",
        yMin="default",
        yMax="default",
        abLine="default"
      )
    }
  )
)

#' Data container for an Epiviz Stacked Line Plot.
#'
#' @import htmltools
#' @importFrom methods new
EpivizStackedLinePlot <- setRefClass("EpivizStackedLinePlot",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-stacked-line-plot")

      callSuper(...)
    },
    # get_dependencies=function() {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=30,
        marginBottom=50,
        marginLeft=30,
        marginRight=15,
        measurementGroupsAggregator="mean-stdev",
        colLabel="colLabel",
        rowLabel="name",
        offset="zero",
        interpolation="step-after",
        scaleToPercent=TRUE,
        useGroupBy="name",
        hoverOpacity=.06
      )
    }
  )
)

#' Data container for an Epiviz Stacked Line Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizStackedLineTrack <- setRefClass("EpivizStackedLineTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      .self$set_name("epiviz-json-stacked-line-track")

      callSuper(...)
    },
    # get_dependencies=function() {},
    get_default_settings=function() {
      list(
        title="",
        marginTop=25,
        marginBottom=20,
        marginLeft=20,
        marginRight=10,
        measurementGroupsAggregator="mean-stdev",
        step=1,
        offset="zero",
        interpolation="basis",
        abLine="default"
      )
    }
  )
)
