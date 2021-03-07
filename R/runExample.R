#' @title Run shiny.CSVImport Example Applications
#' @description Launch shiny.CSVImport example applications, and optionally, your system's web browser.
#' @param example The name of the example to run, or `NA` (the default) to
#'   list the available examples.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the `shiny.host` option, if set, or `"127.0.0.1"` if not.
#' @param display.mode The mode in which to display the example. Defaults to
#'   `showcase`, but may be set to `normal` to see the example without
#'   code or commentary.
#' @inheritParams shiny::runApp
#' @examples
#' # List all available examples
#' # (be sure to use the name space prefix to avoid conflicts)
#' shiny.CSVImport::runExample()
#'
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # Run one of the examples
#'   shiny.CSVImport::runExample("01_hello")
#' }
#'
#' # Find the directory containing the code for all examples
#' system.file("examples", package="shiny.CSVImport")
#' @export
runExample <- function (example = NA,
                        port = getOption("shiny.port"),
                        launch.browser = getOption("shiny.launch.browser", interactive()),
                        host = getOption("shiny.host", "127.0.0.1"),
                        display.mode = c("auto", "normal", "showcase"))
{
  # PRECONDITIONS
  if (!isTRUE(is.na(example)) && !isTruthy(example)) stop("Type of 'examples' argument is invalid")
  if (length(example) > 1) stop("Only one example at a time is possible")

  # Get path and list of existing samples
  examplesDir <- system.file("examples", package = "shiny.CSVImport")
  ListOfSamples <- list.files(examplesDir)

  # Find out if example exists
  if (!is.na(example)) {
    index <- pmatch(example, ListOfSamples)
    if (!is.na(index)) {
      example <- ListOfSamples[index]
      dir <- file.path(examplesDir, example)
    }
    else
      dir <- NULL
  } else {
    dir   <- NULL
  }

  # Give message or run example if it exists
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ""
    }
    else {
      errFun <- stop
      errMsg <- paste0("Example '", example, "' does not exist. ")
    }
    errFun(errMsg, "Valid examples are \"",
           paste(list.files(examplesDir), collapse = "\", \""), "\"")
  }
  else {
    runApp(dir, port = port, host = host, launch.browser = launch.browser,
           display.mode = display.mode)
  }
}
