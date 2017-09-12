#' Data container for an Epiviz App.
#'
#' @field server
#' @field epiviz_data_source
#' @field data_mgr
#'
#' @import epivizrServer
#' @importFrom methods new
EpivizApp <- setRefClass("EpivizApp",
  fields=list(
    server="EpivizServer",
    epiviz_data_source="EpivizDataSource",
    data_mgr="EpivizDataMgr"
  ),
  methods=list(
    initialize=function(server, epiviz_data_source, data_mgr) {
      .self$server <- server
      .self$epiviz_data_source <- epiviz_data_source
      .self$data_mgr <- data_mgr

      invisible(.self)
    }
  )
)
