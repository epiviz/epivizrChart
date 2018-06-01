setClassUnion("EpivizDSorNULL", c("EpivizDataSource", "NULL"))
#' Data container for an Epiviz environment component.
#'
#' @field charts List of class \code{\link[epivizrChart]{EpivizViewComponent}} used to track nested elements.
#' @field interactive Logical value of whether componenet is interactive with data source componenet..
#' @field epiviz_ds  \code{\link[epivizrChart]{EpivizDataSource}} object for interactive documents.
#' @import htmltools
#' @importFrom methods new
EpivizEnvironment <- setRefClass("EpivizEnvironment",
  contains="EpivizViewComponent",
  fields=list(
    charts="list",
    interactive="logical",
    epiviz_ds='EpivizDSorNULL'
  ),
  methods=list(
    initialize=function(interactive=FALSE, epiviz_ds=NULL, ...) {
      .self$charts <- list()
      .self$interactive <- interactive
      .self$epiviz_ds <- epiviz_ds

      callSuper(...)
    },
    plot=function(...) {
      "Plot an EpivizChart within the environment
       \\describe{
        \\item{...}{Arguments for epivizChart}
      }"
      epivizChart(parent=.self, ...)
    },
    append_chart=function(chart) {
      "Append chart or navigation to environment"
      if (!is(chart, "EpivizViewComponent"))
        stop(chart, " must be an EpivizViewComponent object")

      .self$charts[[chart$get_id()]] <- chart

      invisible(.self)
    },
    remove_chart=function(chart) {
      "Remove chart from environment"
      if (!is(chart, "EpivizViewComponent"))
        stop(chart, " must be an EpivizViewComponent object")

      chart_id <- chart$get_id()
      # TODO: Remove ms_obj from data manager if
      # there are no more charts using its data

      .self$charts[[chart_id]] <- NULL

      invisible(.self)
    },
    remove_all_charts=function() {
      "Remove all charts from environment"
      for (chart in .self$charts)
        .self$remove_chart(chart)

      invisible(.self)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      return("epiviz-environment")
    },
    get_component_type=function() {
      "Get component type for prefix of random id generator"
      return("EpivizEnvironment")
    },
    get_charts=function() {
      "Get charts within environment"
      .self$charts
    },
    set_charts=function(charts) {
      "Set charts of environment"
      .self$charts <- charts
    },
    render_component=function(shiny=FALSE) {
      "Render to html
      \\describe{
        \\item{shiny}{ if rendering component in a shiny environment}
      }"
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
    navigate=function(chr=NULL, start=NULL, end=NULL) {
      "Navigate environment to genomic location
      \\describe{
        \\item{chr}{Chromosome}
        \\item{start}{Start location}
        \\item{end}{End location}
      }"
      .self$set_chr(chr)
      .self$set_start(start)
      .self$set_end(end)

      for (chart in .self$charts) {
        chart$navigate(chr, start, end)
      }
    },
    order_charts=function(ordered_charts) {
      "Order the charts within an environment
      \\describe{
      \\item{charts}{An ordered list of EpivizViewComponent objects}
      }"
      if (length(ordered_charts) != length(.self$charts))
        stop("Ordering charts requires all charts from environment")

      chart_ids <- sapply(ordered_charts, function(chart) {
        chart_id <- chart$get_id()
        if (!(chart_id %in% names(.self$charts)))
          stop(chart, " must be from the environment")

        chart_id
      })

      names(ordered_charts) <- chart_ids
      .self$charts <- ordered_charts
    },
    init_region=function(chr=NULL, start=NULL, end=NULL) {
      "Initialize navigation based on a genomic region
      \\describe{
        \\item{chr}{Chromosome}
        \\item{start}{Start location}
        \\item{end}{End location}
      }"
      nav <- epivizNav(chr=chr, start=start, end=end, parent=.self)
      nav$clone_charts(.self$charts)

      invisible(nav)
    },
    init_regions=function(regions) {
      "Initialize navigations based on genomic regions
      \\describe{
        \\item{regions}{List of named lists of genomic locations, e.g.,
        list(list(chr='chr11', start=99800000, end=103383180))}
      }"
      for (region in regions) {
        if (is.null(names(region)))
          stop(region, " must be named with chr, start, and end")

        .self$init_region(region$chr, region$start, region$end)
      }

      invisible(.self)
    },
    add_data=function(...) {
      "Add data to environment's data manager
      \\describe{
        \\item{...}{Arguments for add_measurements and register, e.g., data,
        datasource_name, datasource_obj_name, type, etc}
      }"
      .self$data_mgr$add_measurements(...)
    },
    append_region=function(chr=NULL, start=NULL, end=NULL, ...){
      "Add a child Navigation element to the given genomic region"
      nav <- epivizNav(chr=chr, start=start, end=end, parent=.self)
      return(nav)
    },
    get_measurements=function(){
      "Get measurements from environment's data manager"
      .self$data_mgr$get_measurements()
    },
    get_rows=function(...){
      "Get row data from environment's data manager"
      .self$data_mgr$get_rows(...)
    },
    get_values=function(...){
      "Get value data from environment's data manager"
      .self$data_mgr$get_values(...)
    },
    get_provider_id=function(){
      if (.self$is_interactive()) {
        .self$epiviz_ds$get_provider_id()

      } else {
        NULL
      }
    },
    is_interactive=function() {
      "Return whether the environment is interactive with a data source"
      .self$interactive && !is.null(.self$epiviz_ds)
    },
    register_shiny_handler=function(session) {
      "
      Enable components to interact with Shiny session.
      \\describe{
        \\item{session}{Shiny session object}
      }"
      .self$data_mgr$register_shiny_handler(session) 
      invisible()
    },
    add_genome=function(genome, type="gene_info", datasource_name = NULL) {
      "
      Add a genome to the view, and a genes-track.
      \\describe{
        \\item{genome}{annotation object. eg. Homo.sapiens}
      }"
      # .self$data_mgr$add_measurements(genome, datasource_name)
      .self$data_mgr$add_genome(genome)
      invisible()
    },
    get_dependencies=function(shiny=FALSE) {
      # TODO
      # c(list(EpivizEnvironment=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  lapply(.self$charts, function(chart) chart$get_dependencies()),
      #  callSuper())
      callSuper(shiny)
    }
  )
)
