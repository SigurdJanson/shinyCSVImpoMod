
.DefaultOptions = list(
  UILang = "en"
)

VerifyNSetupOptions <- function(Options) {
  if (is.null(Options)) {
    Options <- .DefaultOptions
    return(Options)
  } else {
    #TODO: FixupList
  }

  return(Options)
}
