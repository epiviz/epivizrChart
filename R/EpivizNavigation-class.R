#' Epiviz Navigation Class
#'
#' @import htmltools
#' @exportClass EpivizNavigation
EpivizNavigation <- setRefClass("EpivizNavigation",
  contains="EpivizEnvironment",
  methods=list(
    add = function(chart_object) {
      "Add chart to navigation"
    }
  )
)
