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