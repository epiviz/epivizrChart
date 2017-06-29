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

#' Generic Plot for EpivizPolyer objects
#' @import epivizrServer
#' @export
plot.EpivizPolymer <- function(x, ...){
  # TODO: dependencies should be added when tag is created.
  webcomponents <- htmltools::htmlDependency(
    name="webcomponents",
    version="1",
    src=c(href="https://epiviz.github.io/polymer/charts/components"),
    script="webcomponentsjs/webcomponents-lite.js"
  )
  
  epiviz_charts <- htmltools::htmlDependency(
    name="epiviz-charts",
    version="1",
    src=c(href="https://epiviz.github.io/polymer/charts/components"),
    import="epiviz-charts/epiviz-charts.html"
  )
  
  epiviz_data_source <- htmltools::htmlDependency(
    name="epiviz-data-source",
    version="1",
    src=c(href="https://epiviz.github.io/polymer/charts/components"),
    import="epiviz-data-source/epiviz-data-source.html"
  )

  main_css <- htmltools::htmlDependency(
    name="main",
    version="1",
    src=c(href="https://epiviz.github.io/polymer/charts/components/epiviz-imports/src"),
    stylesheet="css/main.css"
  )

  svg_css <- htmltools::htmlDependency(
    name="svg",
    version="1",
    src=c(href="https://epiviz.github.io/polymer/charts/components/epiviz-imports/src"),
    stylesheet="css/svg.css"
  )

   icons_css <- htmltools::htmlDependency(
     name="epiviz-icons",
     version="1",
     src=c(href="https://epiviz.github.io/polymer/charts/components/epiviz-imports/src"),
     stylesheet="css/icomoon/epiviz-icons.css"
   )
   
   dependencies <- list(webcomponents, epiviz_charts, epiviz_data_source, main_css, svg_css, icons_css)
   tag <- x$get_tag()
   for (dep in dependencies) {
     tag <- htmltools::attachDependencies(tag, dep, TRUE)
   }

  # temporary directory for output
  tmp_dir <- tempfile(pattern=paste0("epivizrChart", "_", x$get_id()))
  dir.create(tmp_dir)
  
  # output file
  index_html <- file.path(tmp_dir, "index.html")
  
  # save file
  htmltools::save_html(tag, file=index_html)
  # on.exit(unlink(index_html))

  server <- epivizrServer::createServer(static_site_path = tmp_dir, try_ports=TRUE)
  server$start_server()
  # TODO: Need to stop server at some point?
  
  
  # view
  viewer <- getOption("viewer", utils::browseURL)
  viewer(index_html)
  
  invisible()
}