setClassUnion("CharacterOrNULL", c("character", "NULL"))
#' Data container for an Epiviz Data Source component.
#'
#' @field name (character) Epiviz Data Source (tag name).
#' @field class (CharacterOrNULL) Epiviz chart's class attribute.
#' @field id (character) Epiviz chart's id attribute.
#' @field provider_type (character)
#' @field provider_id (character)
#' @field provider_url (character)
#' @import htmltools
#' @importFrom methods new
EpivizDataSource <- setRefClass("EpivizDataSource",
  fields=list(
    name="character",
    class="CharacterOrNULL",
    id="character",
    provider_type="character",
    provider_id="character",
    provider_url="character"
  ),
  methods=list(
    initialize=function(class=NULL, id=rand_id("datasource"),
      provider_type="", provider_id="", provider_url="") {
      .self$name <- .self$get_name()
      .self$class <- class
      .self$id <- id
      .self$provider_type <- provider_type
      .self$provider_id <- provider_id
      .self$provider_url <- provider_url

      invisible(.self)
    },
    get_name=function() {
      "Get name"
      return("epiviz-data-source")
    },
    get_class=function() {
      "Get class"
      .self$class
    },
    get_id=function() {
      "Get id"
      .self$id
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
    set_name=function(name) {
      "Set name"
      .self$name <- name
      invisible()
    },
    set_class=function(class) {
      "Set chart class"
      .self$class <- class
      invisible()
    },
    set_id=function(id) {
      "Set chart id"
      .self$id <- id
      invisible()
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
    get_attributes=function() {
      "Get attributes for rendering chart"
      list(class=.self$class,
        id=.self$id,
        "provider-type"=.self$provider_type,
        "provider-id"=.self$provider_id,
        "provider_url"=.self$provider_url)
    },
    render_component=function() {
      "Render to html"
      tag(.self$get_name(), .self$get_attributes())
    },
    show=function() {
      if (isTRUE(getOption('knitr.in.progress'))) {
        knitr::knit_print(attachDependencies(.self$render_component(),
          .self$get_dependencies(knitr=TRUE)))

      } else {
        # temporary directory for output
        tmp_dir <- tempfile(pattern=rand_id("epiviz"))
        dir.create(tmp_dir)

        # output file
        index_html <- file.path(tmp_dir, "index.html")

        # save file
        save_html(attachDependencies(.self$render_component(),
          .self$get_dependencies()), file=index_html)

        # view
        viewer <- getOption("viewer", utils::browseURL)
        viewer(index_html)

        invisible()
      }
    },
    get_dependencies=function(knitr=FALSE) {
      # TODO
    }
  )
)
