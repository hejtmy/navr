% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/navr-areas.R
\name{add_areas}
\alias{add_areas}
\title{Adds information about what area does the current position belong to}
\usage{
add_areas(obj, areas, ...)
}
\arguments{
\item{areas}{list of \code{\link{AreaObject}}. Needs to be a list of length(areas), even if you are passing
a single area, you should pass it as a list(your_area)}

\item{...}{}
}
\value{

}
\description{
adds a column which contains name of an area the current positions belongs to.
beware that the areas can NOT overlap each other. Each point can only belong to a single area,
otherwise newly added area might overrite the previous one. See more information about areas
in the area vignette
}
