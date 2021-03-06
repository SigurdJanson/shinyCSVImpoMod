% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Converter.R
\name{ColumnConvert}
\alias{ColumnConvert}
\title{Convert columns of a data frame from character to their desired type}
\usage{
ColumnConvert(Columns, Converter, Format, Locale)
}
\arguments{
\item{Columns}{A data frame with columns to cast types}

\item{Converter}{A list (see details)}

\item{Format}{A list of formats (see details)}

\item{Locale}{A locale that can be used in case format is missing}
}
\value{
a data frame with changed column data types
}
\description{
Convert columns of a data frame from character to their desired type
}
\details{
Valid converters: date, time, datetime, character, factor, logical, number,
double, integer, find, regexfind. \code{NULL} drops a column. Everything else will be
guessed by \code{\link[readr:parse_guess]{readr::parse_guess()}}.

The types date, time, and datetime \strong{support} a \code{Format} specification. If none is available
the \code{Locale} is used.

The types "find" and "regexfind" \strong{require} a \code{Format} specification.
}
