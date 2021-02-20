

#' @title ImportCSVDemo - sample app to show how the package works.
#' @export
#' @example ImportCSVDemo()
ImportCSVDemo <- function(){
  loc <- system.file("examples", "moduleExample", package = "shiny.CSVImport")
  shinyAppDir(loc)
}
