#' Convert characters to utf8 representation
#' @param x A string
#' @return All characters are replaced by unicode representations
.HandleUTF8 <- function(x){
  map <- function(x) {
    m <- utf8ToInt(x)
    #-if (is.na(m)) x <- enc2utf8(x)
    return(ifelse(is.na(m), x, sprintf("&#%d;", m)))
  }
  xs <- strsplit(as.character(x), "")[[1]]
  paste0(sapply(xs, map), collapse="")
}


#' HtmlAttrStr
#' Convert the arguments into strings to be used as attributes in HTML.
#' @param ... Named or unnamed arguments.
#' @details See examples (should be self-explanatory)
#' @return A vector of strings.
#'
#' * Named arguments are converted in the form of `attribute="Value"`.
#' * Unnamed arguments are converted as Boolean HTML attributes
#' (like `disabled` or `required`).
#'
#' The characters ".#:" and spaces
#'
#' @examples
#' cat(HtmlAttrStr(id="my id", A="A", "B", C=NULL, D=NA, E=Inf, F=NaN, NA, H=x, Y="Fa\"il", `Zor>ro`="Z"))
#' #> id="my id" A="A" B  D="NA" NA H="2" Y="Fa_il" Zor_ro="Z"
#' @references
#' https://html.spec.whatwg.org/multipage/syntax.html#attributes-2
HtmlAttrStr <- function(...) {
  .cpaste <- function(a, b, sep = "=")
    paste0(
      ifelse(isTruthy(a), a, ""),
      ifelse(isTruthy(a) && isTruthy(b), paste0(sep, "\"", b, "\""), ""),
      ifelse(!isTruthy(a) && isTruthy(b), b, "")
    )
  .is.invalid <- function(x) is.infinite(x) || is.nan(x)

  nonChar  <- "\UFDD0-\UFDEF\UFFFE\UFFFF\U1FFFE\U1FFFF\U2FFFE\U2FFFF\U3FFFE\U3FFFF\U4FFFE\U4FFFF\U5FFFE\U5FFFF\U6FFFE\U6FFFF\U7FFFE\U7FFFF\U8FFFE\U8FFFF\U9FFFE\U9FFFF\UAFFFE\UAFFFF\UBFFFE\UBFFFF\UCFFFE\UCFFFF\UDFFFE\UDFFFF\UEFFFE\UEFFFF\UFFFFE\UFFFFF\U10FFFE\U10FFFF"
  ctrlChar <- "\U0001-\U001F\U007F-\U009F"
  noNameChar <- r"{[:blank:].#:"'>/=}"
  CleanNameRegex <- paste0("[", nonChar, ctrlChar, noNameChar, "]")


  attrvalues <- list(...)
  if (length(attrvalues) == 0) return(NULL)

  # Drop invalid args
  attrvalues <- attrvalues[!sapply(attrvalues, .is.invalid)]
  if (length(attrvalues) == 0) {
    return(NULL)
  }

  # Necessary to make factors show up as level names, not numbers
  attrvalues[] <- lapply(attrvalues, paste, collapse = " ")

  # Replace forbidden characters with '_'
  # Forbidden are control characters, SPACE, "'>/=, and non-characters
  attrnames  <- gsub(CleanNameRegex, "_", names(attrvalues) )
  # Replace " with _ because it is forbidden
  attrvalues <- gsub(r"["]", "_", attrvalues )

  Result <- mapply(function(n, x) .cpaste(n, x),
                   attrnames,
                   attrvalues,
                   SIMPLIFY = TRUE, USE.NAMES = TRUE)
  # strip attributes and return
  return(`attributes<-`(Result, NULL))
}



#' FixupList
#' @description Checks a list for falsy or missing values and replaces them by
#' values taken from a default.
#' Remove extraneous elements in x that aren't part of the default.
#' Add missing elements in x.
#' Replace falsy values in x by default values.
#' @param x A named list to fix
#' @param Default A named list of default values
#'
#' @return The fixed list
#' @examples
#' # FixupList(list(a=1, b=NA, c=3, d=NA), list(a=4, d=8, c=9, x=7))
#' #> list(a=1, c=3, d=8, x=7)
FixupList <- function(x, Default) {
  stopifnot(!(is.null(x) || is.null(Default)))

  # Remove extra values in x
  x <- x[names(x) %in% names(Default)]
  # Replace falsy values in x[..] by Default[..]
  Falsies <- !isTruthyInside(x)
  Result <- replace(x,
                    Falsies,
                    Default[match(names(x), names(Default))][Falsies])
  # Add missings in x
  Result <- c(Result, Default[!(names(Default) %in% names(x))])

  return(Result)
}
