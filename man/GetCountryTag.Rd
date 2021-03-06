% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LanguageCodes.R
\name{GetCountryTag}
\alias{GetCountryTag}
\title{Extracts the country subtags from a list of language tags}
\usage{
GetCountryTag(Code)
}
\arguments{
\item{Code}{A code as string in the format "xx-XX" or just "xx".}
}
\value{
the second subtag(s) extracted from the given codes. If a
country code is missing the function uses \code{NA}.
}
\description{
Extracts the country subtags from a list of language tags
}
\note{
The function does not verify if the country code exists
}
