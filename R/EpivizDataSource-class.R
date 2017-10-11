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
    initialize=function(provider_type="epiviz.data.WebsocketDataProvider",
      provider_id=rand_id("epiviz"), provider_url="",...) {
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
      # TODO
      # c(list(EpivizDataSource=htmlDependency(
      #  name="",
      #  version=0,
      #  head="",
      #  src="",
      #  all_files=TRUE)),
      #  callSuper())
      callSuper(knitr)
    }
   )
  )
