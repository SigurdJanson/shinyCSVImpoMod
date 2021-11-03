
.DefaultOptions = list(
  UILang = "en"
)

VerifyNSetupOptions <- function(Options) {
  if (is.null(Options))
    return(.DefaultOptions)
  else
    return(FixupList(Options, .DefaultOptions))
}
