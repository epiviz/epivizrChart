setClassUnion("CharacterOrNULL", c("character", "NULL"))
#' Data container for an Epiviz Web component.
#'
#' @field data_mgr \code{\link[epivizrChart]{EpivizChartDataMgr}}
#' @field name (character) Epiviz chart type (tag name).
#' @field class (CharacterOrNULL) Epiviz chart's class attribute.
#' @field id (character) Epiviz chart's id attribute.
#'
#' @import htmltools
#' @importFrom methods new
EpivizWebComponent <- setRefClass("EpivizWebComponent",
  contains="VIRTUAL",
  fields=list(
    data_mgr="EpivizChartDataMgr",
    name="character",
    class="CharacterOrNULL",
    id="character"
  ),
  methods=list(
    initialize=function(data_mgr=EpivizChartDataMgr(), class=NULL,
      id=rand_id(.self$get_component_type())) {

      .self$data_mgr <- data_mgr
      .self$name <- .self$get_name()
      .self$class <- class
      .self$id <- id

      invisible(.self)
    },
    get_data_mgr=function() {
      "Get data manager"
      .self$data_mgr
    },
    get_name=function() {
      "Get name of Epiviz Web Component"
      stop("'get_name' called on virtual class object")
    },
    get_class=function() {
      "Get class"
      .self$class
    },
    get_id=function() {
      "Get id"
      .self$id
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
    get_attributes=function() {
      "Get attributes for rendering web component"
      list(class=.self$class, id=.self$id)
    },
    show=function() {
      if (isTRUE(getOption('knitr.in.progress'))) {
        knitr::knit_print(.self$render_component()) 
      } else {
        # temporary directory for output
        tmp_dir <- tempfile(pattern=rand_id("epiviz"))
        dir.create(tmp_dir)

        # output file
        index_html <- file.path(tmp_dir, "index.html")

        # save file
        save_html(.self$render_component(), file=index_html) 

        # view
        viewer <- getOption("viewer", utils::browseURL)
        viewer(index_html)

        invisible()
      }
    },
    get_dependencies=function(shiny=FALSE) {
      if (isTRUE(getOption('knitr.in.progress'))) { 
        polymer_lib <- system.file(package="epivizrChart", 
                                   "www", "lib/polymer/", "epiviz-charts.html") 
        polymer_ds_lib <- system.file(package="epivizrChart", 
                                      "www", "lib/polymer/", "epiviz-data-source.html") 
      } else if (shiny) { 
        shiny::addResourcePath('epiviz', 
                               system.file(package="epivizrChart", "www/lib/polymer")) 
        polymer_lib <- "epiviz/epiviz-charts.html" 
        polymer_ds_lib <- "epiviz/epiviz-data-source.html" 
      } else { 
        polymer_lib <- "lib/epiviz-charts-1/epiviz-charts.html" 
        polymer_ds_lib <- "lib/epiviz-charts-1/epiviz-data-source.html" 
      } 

      list(
        # jquery=htmlDependency(
        #   name="jquery",
        #   version="1.12.4",
        #   src=system.file(package="epivizrChart", "www", "lib"),
        #   script="jquery.js"),
        # jquery_ui=htmlDependency(
        #   name="jquery-ui",
        #   version="1.12.1",
        #   src=system.file(package="epivizrChart", "www", "lib"),
        #   script="jquery-ui.js"),
        # jquery_ui_css=htmlDependency(
        #   name="jquery-ui-css",
        #   version="1.12.1",
        #   src=system.file(package="epivizrChart", "www", "lib"),
        #   stylesheet="jquery-ui.css"),
        webcomponents=htmlDependency(
          name="webcomponents",
          version="0.7.24",
          src=system.file(package="epivizrChart", "www", "lib/webcomponents"),
          script="webcomponents-lite.js"),
        # data_source=htmlDependency(
        #   name="epiviz-data-source",
        #   version="1",
        #   head=paste0("<link rel='import' href='",  polymer_ds_lib, "'>"),
        #   src=system.file(package="epivizrChart", "www", "lib/polymer"),
        #   all_files=TRUE),
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
