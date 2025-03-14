% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/navr-generics-getting.R
\name{filter_times}
\alias{filter_times}
\alias{filter_times.navr}
\title{Filters navr object to only include times in between}
\usage{
filter_times(obj, times, zero_based, ...)

\method{filter_times}{navr}(obj, times, zero_based = FALSE)
}
\arguments{
\item{obj}{object to filter}

\item{times}{a vector of length 2, or a matrix with two columns,
start time and end time in seconds}

\item{zero_based}{signifying if you passed seconds from start or real time.
\emph{Defaults} to FALSE (real time passed)}

\item{...}{aditional arguments}
}
\description{
Filters navr object to only include times in between
}
\section{Methods (by class)}{
\itemize{
\item \code{filter_times(navr)}: Filtering navr object

}}
