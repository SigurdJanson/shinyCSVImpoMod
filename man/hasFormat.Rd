% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataTypes.R
\name{hasFormat}
\alias{hasFormat}
\alias{hasFormat.collector}
\alias{hasFormat.col_spec}
\alias{hasFormat.function}
\title{hasFormat}
\usage{
hasFormat(...)

\method{hasFormat}{collector}(...)

\method{hasFormat}{col_spec}(...)

\method{hasFormat}{`function`}(...)
}
\arguments{
\item{...}{A collection of objects or atomic vectors. All elements must
have the same type.}
}
\value{
A list of logicals. If the input contains vectors with length > 1 or
lists the returned list is nested.
}
\description{
hasFormat
}
\details{
Basic \code{lists} are not supported, i.e.
\verb{hasFormat(c(col_time(), col_double())} will not work.
}
\section{Methods (by class)}{
\itemize{
\item \code{hasFormat(collector)}: Accepts lists of S3 class \code{collector} as generated
by \link[vroom:cols]{vroom::col*_()}.

\item \code{hasFormat(col_spec)}: Accepts column specification objects

\item \code{hasFormat(`function`)}: Accepts \link[vroom:cols]{vroom::col*_()} functions.
No nesting allowed (because it would create a list).

}}
\examples{
hasFormat("t", "d", "i", "T")
hasFormat(c("t", "d", "i", "T"))
}
