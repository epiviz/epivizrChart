#' Data container for an Epiviz Data Source component.
#'
#' @field provider_type (character)
#' @field provider_id (character)
#' @field provider_url (character)
#' @import htmltools
#' @importFrom methods new
EpivizDataSource <- setRefClass("EpivizDataSource",
  contains="EpivizWebComponent",
  fields=list(
    provider_type="character",
    provider_id="character",
    provider_url="character"
  ),
  methods=list(
    initialize=function(provider_type="", provider_id="", provider_url="", ...) {
      .self$provider_type <- provider_type
      .self$provider_id <- provider_id
      .self$provider_url <- provider_url

      callSuper(...)
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      return("epiviz-data-source")
    },
    get_provider_type=function() {
      "Get provider type"
      .self$provider_type
    },
    get_provider_id=function() {
      "Get provider id"
      .self$provider_id
    },
    get_provider_url=function() {
      "Get provider url"
      .self$provider_url
    },
    set_provider_type=function(type) {
      "Set provider type"
      .self$provider_type <- type
      invisible()
    },
    set_provider_id=function(id) {
      "Set provider id"
      .self$provider_id <- id
      invisible()
    },
    set_provder_url=function(url) {
      "Set provider url"
      .self$provider_url <- url
      invisible()
    },
    get_component_type=function(){
      "Get component type for prefix of random id generator"
      return("EpivizDataSource")
    },
    get_attributes=function() {
      "Get attributes for rendering component"
      c(list("provider-type"=.self$provider_type,
        "provider-id"=.self$provider_id,
        "provider-url"=.self$provider_url),
        callSuper())
    },
    render_component=function() {
      "Render to html"
      tag(.self$get_name(), .self$get_attributes())
    },
    get_dependencies=function(knitr=FALSE) {
      # TO DO: remove other dependencies
      polymer_lib <- system.file(package="epivizrChart",
        "www", "lib/polymer/", "epiviz-charts.html")
      polymer_ds_lib <- system.file(package="epivizrChart",
        "www", "lib/polymer/", "epiviz-data-source.html")

      if(!knitr) {
        polymer_lib <- "lib/epiviz-charts-1/epiviz-charts.html"
        polymer_ds_lib <- "lib/epiviz-charts-1/epiviz-data-source.html"
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
          all_files=TRUE),
        data_source=htmlDependency(
          name="epiviz-data-source",
          version="1",
          head=paste0("<link rel='import' href='",  polymer_ds_lib, "'>"),
          src=system.file(package="epivizrChart", "www", "lib/polymer"),
          all_files=TRUE)
      )
    }
   )
  )
