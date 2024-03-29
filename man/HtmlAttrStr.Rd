% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helpers.R
\name{HtmlAttrStr}
\alias{HtmlAttrStr}
\title{HtmlAttrStr
Convert the arguments into strings to be used as attributes in HTML.}
\usage{
HtmlAttrStr(...)
}
\arguments{
\item{...}{Named or unnamed arguments.}
}
\value{
A vector of strings.
\itemize{
\item Named arguments are converted in the form of \code{attribute="Value"}.
\item Unnamed arguments are converted as Boolean HTML attributes
(like \code{disabled} or \code{required}).
}

The characters ".#:" and spaces
}
\description{
HtmlAttrStr
Convert the arguments into strings to be used as attributes in HTML.
}
\details{
See examples (should be self-explanatory)
}
\examples{
\dontrun{
cat(
  HtmlAttrStr(id="my id", A="A", "B", C=NULL, D=NA, E=Inf, F=NaN, NA, H=x, Y="Fa\"il", `Zor>ro`="Z")
)
#> id="my id" A="A" B  D="NA" NA H="2" Y="Fa_il" Zor_ro="Z"
}
}
\references{
https://html.spec.whatwg.org/multipage/syntax.html#attributes-2
}
