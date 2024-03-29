% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImportModule.R
\name{ModuleImportServer}
\alias{ModuleImportServer}
\title{The server function of the CSV import module}
\usage{
ModuleImportServer(
  Id,
  Mode = .ImpModes,
  ColSpec = NULL,
  FileSpec = NULL,
  Options = NULL
)
}
\arguments{
\item{Id}{Module name space}

\item{Mode}{Interaction mode of the module. What possibilities shall the
user have to modify the way the file is to be imported? One of three options:
"AsIs", "Desired", "UserDefined".}

\item{ColSpec}{A list specifying the columns to import.}

\item{FileSpec}{A list describing the expected CSV format.
(see details).}

\item{Options}{Use \code{Options$UILang} for the language
in the user interface (allowed values: "en", "de").}
}
\value{
A data frame containing the uploaded CSV file
}
\description{
The server function of the CSV import module
}
\details{
\code{ColSpec} can have different formats. It can be a col_spec
object (see \code{\link[readr:spec]{readr::cols_condense()}}),
a tibble or a named list with the following format:
\describe{
\item{Name}{A list of variable (i.e. column) names
that shall replace the column heads in the file (character
vector).}
\item{NameInFile}{A list of column heads in the CSV
file (character vector).}
\item{Type}{The data types of each variable (character vector).
If no value (i.e. falsy values) is given, it will be
guessed.}
\item{Format}{An additional format specification (character vector).
That is supported by "datetime", "date", and "time". If none
is given the \code{Expected} options are used.}
}

\code{FileSpec} can have these fields:
\describe{
\item{LangCode}{Language code (e.g. "de" or "en)}
\item{Header}{Does the CSV file have a header? (\code{TRUE} (default)/\code{FALSE};
see \code{\link[utils:read.table]{utils::read.csv()}} argument \code{header}). If a column specification
is available, \code{Header} will be coerced to \code{TRUE}.}
\item{ColSep}{A character separating columns (
see \code{\link[utils:read.table]{utils::read.csv()}} argument \code{sep})}
\item{ThousandsSep}{The character that separates thousands in numbers.}
\item{DecimalsSep}{The character used decimal points in the file
(see \code{\link[utils:read.table]{utils::read.csv()}} argument \code{dec})}
\item{DateFormat}{Format used for dates in the file
(format specification by \code{\link[base:strptime]{base::strptime()}}).}
\item{TimeFormat}{Format used for temporal data in the file
(format specification by \code{\link[base:strptime]{base::strptime()}}).}
\item{Quote}{Character to identify text,
see \code{\link[utils:read.table]{utils::read.csv()}} argument \code{quote}}
\item{StringsAsFactors}{Convert all strings to factors (\code{TRUE}/\code{FALSE} is default;
see \code{\link[utils:read.table]{utils::read.csv()}} argument \code{stringsAsFactors}).}
}
}
