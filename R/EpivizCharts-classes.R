#' Data container for an Epiviz Genes Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizGenesTrack <- setRefClass("EpivizGenesTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-genes-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("GenesTrack")
    },
    get_default_settings=function() {
      "Get default settings"
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10
      )
    },
    get_default_colors=function() {
      "Get default colors"
      c("#f9a65a",
        "#599ad3",
        "#79c36a",
        "#f1595f",
        "#727272",
        "#cd7058",
        "#d77fb3"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizGenesTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
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
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-blocks-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("BlocksTrack")
    },
    get_default_settings=function() {
      "Get default settings"
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10,
        minBlockDistance=5,
        blockColorBy="colLabel"
      )
    },
    get_default_colors=function() {
      "Get default colors"
      c("#f9a65a",
        "#599ad3",
        "#79c36a",
        "#f1595f",
        "#727272",
        "#cd7058",
        "#d77fb3"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizBlocksTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
    }
  )
)

#' Data container for an Epiviz Stacked Blocks Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizStackedBlocksTrack <- setRefClass("EpivizStackedBlocksTrack",
   contains="EpivizChart",
   methods=list(
     initialize=function(...) {
       callSuper(...)
     },
     get_name=function() {
       "Get name of Epiviz Web Component"
       "epiviz-stacked-blocks-track"
     },
     get_component_type=function() {
       "Get component type for prefix of random id generator"
       return("StackedBlocksTrack")
     },
     get_default_settings=function() {
       "Get default settings"
       list(
         title="",
         marginTop=25,
         marginBottom=23,
         marginLeft=20,
         marginRight=10,
         minBlockDistance=5,
         blockColorBy="colLabel"
       )
     },
     get_default_colors=function() {
       "Get default colors"
       c("#f9a65a",
         "#599ad3",
         "#79c36a",
         "#f1595f",
         "#727272",
         "#cd7058",
         "#d77fb3"
       )
     },
     get_dependencies=function(shiny=FALSE) {
       # TODO
       # c(list(EpivizBlocksTrack=htmlDependency(
       #  name="",
       #  version=0,
       #  head="",
       #  src="",
       #  all_files=TRUE)),
       #  callSuper())
       callSuper(shiny)
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
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-heatmap-plot"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("HeatmapPlot")
    },
    get_default_settings=function() {
      "Get default settings"
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
    },
    get_default_colors=function() {
      "Get default colors"
      c("#1859a9",
        "#ed2d2e",
        "#008c47",
        "#010101",
        "#f37d22",
        "#662c91",
        "#a11d20",
        "#b33893"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizHeatmapPlot=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
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
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-line-plot"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("LinePlot")
    },
    get_default_settings=function() {
      "Get default settings"
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
    },
    get_default_colors=function() {
      "Get default colors"
      c("#393b79",
        "#5254a3",
        "#6b6ecf",
        "#9c9ede",
        "#637939",
        "#8ca252",
        "#b5cf6b",
        "#cedb9c",
        "#8c6d31",
        "#bd9e39",
        "#e7ba52",
        "#e7cb94",
        "#843c39",
        "#ad494a",
        "#d6616b",
        "#e7969c",
        "#7b4173",
        "#a55194",
        "#ce6dbd",
        "#de9ed6"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizLinePlot=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
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
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-line-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("LineTrack")
    },
    get_default_settings=function() {
      "Get default settings"
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
    },
    get_default_colors=function() {
      "Get default colors"
      c("#1859a9",
        "#ed2d2e",
        "#008c47",
        "#010101",
        "#f37d22",
        "#662c91",
        "#a11d20",
        "#b33893"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizLineTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
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
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-scatter-plot"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("ScatterPlot")
    },
    get_default_settings=function() {
      "Get default settings"
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
    },
    get_default_colors=function() {
      "Get default colors"
      c("#1f77b4",
        "#ff7f0e",
        "#2ca02c",
        "#d62728",
        "#9467bd",
        "#8c564b",
        "#e377c2",
        "#7f7f7f",
        "#bcbd22",
        "#17becf"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizScatterPlot=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
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
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-stacked-line-plot"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("StackedLinePlot")
    },
    get_default_settings=function() {
      "Get default settings"
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
    },
    get_default_colors=function() {
      "Get default colors"
      c("#393b79",
        "#5254a3",
        "#6b6ecf",
        "#9c9ede",
        "#637939",
        "#8ca252",
        "#b5cf6b",
        "#cedb9c",
        "#8c6d31",
        "#bd9e39",
        "#e7ba52",
        "#e7cb94",
        "#843c39",
        "#ad494a",
        "#d6616b",
        "#e7969c",
        "#7b4173",
        "#a55194",
        "#ce6dbd",
        "#de9ed6"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizStackedLinePlot=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
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
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-stacked-line-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("StackedLineTrack")
    },
    get_default_settings=function() {
      "Get default settings"
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
    },
    get_default_colors=function() {
      "Get default colors"
      c("#1f77b4",
        "#ff7f0e",
        "#2ca02c",
        "#d62728",
        "#9467bd",
        "#8c564b",
        "#e377c2",
        "#7f7f7f",
        "#bcbd22",
        "#17becf"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizStackedLineTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
    }
  )
)

#' Data container for an Epiviz Multi Stacked Line Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizMultiStackedLineTrack <- setRefClass("EpivizMultiStackedLineTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-multistacked-line-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("MultiStackedLineTrack")
    },
    get_default_settings=function() {
      "Get default settings"
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
        abLine="default",
        showTracks="default",
        showYAxis=TRUE,
        autoScale=TRUE,
        showFill=TRUE
      )
    },
    get_default_colors=function() {
      "Get default colors"
      c("#1f77b4",
        "#ff7f0e",
        "#2ca02c",
        "#d62728",
        "#9467bd",
        "#8c564b",
        "#e377c2",
        "#7f7f7f",
        "#bcbd22",
        "#17becf"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizMultiStackedLineTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
    }
  )
)


#' Data container for an Epiviz Genes Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizIGVTrack <- setRefClass("EpivizIGVTrack",
  contains="EpivizChart",
  fields=list(
    file="character",
    file_type="character",
    file_format="character",
    file_name="CharacterOrNULL"
  ),
  methods=list(
    initialize=function(file, file_type, file_format, file_name, ...) {
      
      .self$file <- file
      .self$file_type <- file_type
      .self$file_format <- file_format
      .self$file_name <- file_name
      
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-igv-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("IGVTrack")
    },
    get_default_settings=function() {
      "Get default settings"
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10
      )
    },
    get_default_colors=function() {
      "Get default colors"
      c("#f9a65a",
        "#599ad3",
        "#79c36a",
        "#f1595f",
        "#727272",
        "#cd7058",
        "#d77fb3"
      )
    },
    get_attributes=function() {
      "Get attributes for rendering component"
      
      tracks <- list(list(name= "Genes", type= "annotation", format= "bed",
                    sourceType= "file",
                    url= "https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg19/genes/refGene.hg19.bed.gz",
                    indexURL= "https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg19/genes/refGene.hg19.bed.gz.tbi"),
               list(name=.self$file_name, type=.self$file_type, format=.self$file_format, sourceType="file",
                    url=.self$file))
      
      c(list(
        tracks=json_writer(tracks),
        colors=json_writer(.self$colors),
        settings=json_writer(.self$settings)),
        callSuper())
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizGenesTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
    }
  )
)

#' Data container for an Epiviz Transcript Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizTranscriptTrack <- setRefClass("EpivizTranscriptTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-transcript-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("TranscriptTrack")
    },
    get_default_settings=function() {
      "Get default settings"
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10
      )
    },
    get_default_colors=function() {
      "Get default colors"
      c("#f9a65a",
        "#599ad3",
        "#79c36a",
        "#f1595f",
        "#727272",
        "#cd7058",
        "#d77fb3"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizTranscriptTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
    }
  )
)

#' Data container for an Epiviz Genes Track.
#'
#' @import htmltools
#' @importFrom methods new
EpivizGuideTrack <- setRefClass("EpivizGuideTrack",
  contains="EpivizChart",
  methods=list(
    initialize=function(...) {
      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      "epiviz-guide-track"
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("GuideTrack")
    },
    get_default_settings=function() {
      "Get default settings"
      list(
        title="",
        marginTop=25,
        marginBottom=23,
        marginLeft=20,
        marginRight=10
      )
    },
    get_default_colors=function() {
      "Get default colors"
      c("#f9a65a",
        "#599ad3",
        "#79c36a",
        "#f1595f",
        "#727272",
        "#cd7058",
        "#d77fb3"
      )
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizGuideTrack=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(shiny)
    }
  )
)
