% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helpers.R
\name{FixupList}
\alias{FixupList}
\title{FixupList}
\usage{
FixupList(x, Default)
}
\arguments{
\item{x}{A named list to fix}

\item{Default}{A named list of default values}
}
\value{
The fixed list
}
\description{
Checks a list for falsy or missing values and replaces them by
values taken from a default.
Remove extraneous elements in x that aren't part of the default.
Add missing elements in x.
Replace falsy values in x by default values.
}
\examples{
# FixupList(list(a=1, b=NA, c=3, d=NA), list(a=4, d=8, c=9, x=7))
#> list(a=1, c=3, d=8, x=7)
}
