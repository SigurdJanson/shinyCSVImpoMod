



#' @title Extracts the primary language subtags from a list of language tags
#' @param Code A code as string in the format "xx-XX" or just "xx".
#' @note The function does not verify if the country code exists
#' @returns the second subtag(s) extracted from the given codes.
GetLanguageTag <- function(Code) {
  if (missing(Code) || length(Code) == 0 || is.null(Code) ||
      is.na(Code) || !is.atomic(Code))
    stop("Expected code is not truthy")
  tryCatch({
    Split <- strsplit(Code, "[-_]")
    Language <- tolower(sapply(Split, `[[`, 1))
  }, error = function(e) stop(sprintf("Invalid language code: %s", e)))
  return(Language)
}


#' @title Extracts the country subtags from a list of language tags
#' @param Code A code as string in the format "xx-XX" or just "xx".
#' @note The function does not verify if the country code exists
#' @returns the second subtag(s) extracted from the given codes. If a
#' country code is missing the function uses `NA`.
GetCountryTag <- function(Code) {
  if (missing(Code) || length(Code) == 0 || is.null(Code) ||
      is.na(Code) || !is.atomic(Code))
    stop("Expected code is not truthy")
  tryCatch({
    Split <- strsplit(Code, "[-_]")
    Language <- sapply(Split, function(x) { tryCatch(return(x[[2]]), error = function(e) NA_character_) } )
    Language <- toupper(Language)
  }, error = function(e) stop(sprintf("Invalid language code: %s", e)))
  return(Language)
}


#' @title Fixes typical inconsistencies in the use of language codes
#' @param Code A code as string in the format "xx-XX" or just "xx".
#' @description This function replaces '_' characters with '-'. Primary language codes are
#' forced to lower case and country codes are forced to upper case letters.
FormatLocaleCode <- function(Code) {
  if (missing(Code) || length(Code) == 0 || is.null(Code) ||
      is.na(Code) || !is.atomic(Code))
    stop("Expected code is not truthy")
  tryCatch({
    Split <- strsplit(Code, "[-_]")
    Code <- sapply(Split, function(x) ifelse(is.na(x[2]), tolower(x[1]), paste(tolower(x[1]), toupper(x[2]), sep="-")))
  }, error = function(e) stop(sprintf("Invalid language code: %s", e)))
  return(Code)
}


# GetLanguageTag(c("de-de", "de-CH", "EN-GB", "ar", "	AF"))
# #> [1] "de" "de" "en" "ar" "af"
#
# GetCountryTag(c("de-de", "de-CH", "EN-GB", "es"))
# #> "DE" "CH" "GB" NA
#
# FormatLocaleCode(c("de-de", "de-CH", "EN-GB", "Es"))
# #> [1] "de-DE" "de-CH" "en-GB" "es"
