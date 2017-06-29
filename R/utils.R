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
  # temporary directory for output
  tmp_dir <- tempfile(pattern=paste0("epivizrChart", "_", x$get_id()))
  dir.create(tmp_dir)
  
  # output file
  index_html <- file.path(tmp_dir, "index.html")
  
  # save file
  htmltools::save_html(x$get_tag(), file=index_html)
  # on.exit(unlink(index_html))

  server <- epivizrServer::createServer(static_site_path=tmp_dir, try_ports=TRUE)
  server$start_server()
  # TODO: Need to stop server at some point?
  
  # view
  viewer <- getOption("viewer", utils::browseURL)
  viewer(index_html)
  
  invisible()
}