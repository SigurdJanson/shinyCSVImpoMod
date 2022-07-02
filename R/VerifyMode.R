

#' VerifyMode
#' Tests and verifies the argument `Mode` of the module server function
#' `ModuleImportServer`.
#' @details matches the argument against it's default values.
#' @param Mode The interaction mode for the CSV import module.
#'
#' @return If successfully verified the function returns the correct mode
#' (see also [match.arg()]).
VerifyMode <- function(Mode) {
  formal.args <- formals(sys.function(sysP <- sys.parent()))
  Defaults <- eval(formal.args$Mode, envir = sys.frame(sysP))

  match.arg(Mode, Defaults, several.ok = FALSE)
}


# Test <- function(Mode = c("AsIs", "Desired", "UserDefined")) {
#   VerifyMode(Mode)
# }
# Test("X")
