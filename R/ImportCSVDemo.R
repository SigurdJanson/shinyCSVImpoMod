

#' @title ImportCSVDemo - sample app to show how the package works.
#' @export
#' @examples \dontrun{ImportCSVDemo()}
ImportCSVDemo <- function(){
  loc <- system.file("examples", "moduleExample", package = "shiny.CSVImport")
  shinyAppDir(loc)
}
