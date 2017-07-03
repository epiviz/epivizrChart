json_parser <- rjson::fromJSON

#' JSON writer used by this package
#'
#' Currently this just renames \code{\link{toJSON}} in the \code{rjson} package.
#' @export
#'
#' @param x object to write to json
#' @param method method used to write json
#' @return a string with JSON encoding of object
#'
#' @seealso \code{\link{toJSON}}
#' @examples
#' json_writer(1:10)
json_writer <- rjson::toJSON

#' Knit print method for EpivizPolymer objects
#' @export
knit_print.EpivizPolymer <- function(x, ...) {
  knitr::knit_print(x$get_tag())
}

# HTML dependencies of an EpivizChart
chart_dependencies <- function() {
  deps <- list(
    # TODO: fix version numbers, restructure dependencies
    webcomponents <- htmltools::htmlDependency(
      name="webcomponents",
      version="1",
      src=system.file(package = "epivizrChart", "www", "lib/webcomponents"),
      #c(href="https://epiviz.github.io/polymer/charts/components/webcomponentsjs"),
      script="webcomponents-lite.js"
    ),
    epiviz_charts <- htmltools::htmlDependency(
      name="epiviz-charts",
      version="1",
      src=system.file(package = "epivizrChart", "www", "lib/polymer"),
      #c(href="https://epiviz.github.io/polymer"),
      import="epiviz-charts.html"
    )#,
    # epiviz_data_source <- htmlDependency(
    #  name="epiviz-data-source",
    #  version="1",
    #  src=c(href="https://epiviz.github.io/polymer/charts/components/epiviz-data-source"),
    #  import="epiviz-data-source.html"
    # )
  )

  deps
}
