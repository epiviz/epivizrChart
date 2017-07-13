.onLoad <- function(libname, pkgname) {
  # TODO after pull request is accepted in htmltools
  # unlockBinding("renderDependencies", as.environment("package:htmltools")) 
  # unlockBinding("htmlDependency", as.environment("package:htmltools")) 
  
  # library(htmltools)
  # 
  # assignInNamespace("renderDependencies", renderDependencies, "htmltools")
  # assignInNamespace("htmlDependency", htmlDependency, "htmltools")
  
  invisible()
}