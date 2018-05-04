#' Data container for an Epiviz navigation component.
#'
#' @field gene (character) Gene
#' @field geneInRange (character) Nearest Gene in range.
#' @field parent An object of class \code{\link[epivizrChart]{EpivizEnvironment}} where navigation is appended.
#' @import htmltools
#' @importFrom methods new
EpivizNavigation <- setRefClass("EpivizNavigation",
  contains="EpivizEnvironment",
  fields=list(
    gene="CharacterOrNULL",
    geneInRange="CharacterOrNULL",
    parent="EpivizEnvOrNULL"
  ),
  methods=list(
    initialize=function(gene=NULL, geneInRange=NULL, parent=NULL,
      chr=NULL, start=NULL, end=NULL, ...) {
      .self$gene <- gene
      .self$geneInRange <- geneInRange
      .self$parent <- parent

      # check if region is provided
      if (is.null(chr) || is.null(start) || is.null(end)) {
        stop("EpivizNavigation must have a region: chr, start, and end")
      }

      callSuper(chr=chr, start=start, end=end, class="charts", ...)
    },
    set_gene=function(gene) {
      "Set gene"
      .self$gene <- gene
      invisible()
    },
    set_geneInRange=function(gene) {
      "Set step ratio"
      .self$geneInRange <- gene
      invisible()
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      return("epiviz-navigation")
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("EpivizNavigation")
    },
    get_gene=function() {
      "Get gene"
      .self$gene
    },
    get_geneInRange=function() {
      "Get gene in range"
      .self$geneInRange
    },
    get_attributes=function() {
      "Get attributes for rendering chart"
      c(list(gene=.self$gene, geneInRange=.self$geneInRange), callSuper())
    },
    clone_charts=function(charts) {
      "Clone EpivizCharts and append to navigation. Each chart must already
      exist in the navigation's data manager, otherwise an error will occur
      when attempting to intialize using their measurements
      \\describe{
      \\item{charts}{list of EpivizCharts whose data exists in the
      navigation's data manager }}"
      for (chart in charts) {
        if (!identical(chart$get_data_mgr(), .self$get_data_mgr()))
          stop(chart, " and navigation must share a data manager")

        if (is(chart, "EpivizChart")) {
          ms <- chart$get_measurements()

          epivizChart(measurements=ms,
            datasource_name=ms[[1]]@name,
            chart=chart$get_component_type(),
            settings=chart$get_settings(),
            colors=chart$get_colors(), parent=.self)
        }
      }
    },
    render_component=function(shiny=FALSE) {
      "Render to html"
      env <- tag(.self$name, .self$get_attributes())
      env <- htmltools::attachDependencies(env, .self$get_dependencies(shiny))
      
      tags <- tagSetChildren(tag=env, list=lapply(.self$charts,
                                                  function(chart) chart$render_component(shiny)))
      
      if (.self$is_interactive()) {
        tags <- tagList(.self$epiviz_ds$render_component(shiny), tags)
      }
      
      deps <- htmltools::htmlDependencies(tags)
      # This will remove redundant dependencies
      # (e.g., when an environment has multiple similar chart types)
      deps <- htmltools::resolveDependencies(deps)
      
      htmltools::attachDependencies(tags, deps)
    },
    add_genome=function(genome, type="gene_info", datasource_name = NULL) {
      "
      Add a genome to the view, and a genes-track.
      \\describe{
        \\item{genome}{annotation object. eg. Homo.sapiens}
      }"
      .self$plot(genome, type)
      callSuper(genome, type, datasource_name)
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizNavigation=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper(shiny))
      callSuper(shiny)
    }
  )
)
