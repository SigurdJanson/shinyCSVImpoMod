


VerifyNSetupOptions <- function(Options, I18n) {
  if (is.null(Options)) {
    Options$UILang <- "en"
  }

  # setup translator (if language isn't available, default is en)
  UiLng <- ifelse(Options$UILang %in% I18n$get_languages(), UiLng, "en")
  I18n$set_translation_language(UiLng)

  return(Options)
}
