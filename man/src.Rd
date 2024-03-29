% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directories.R
\name{src}
\alias{src}
\title{Source helpers}
\usage{
src(..., recursive = TRUE)
}
\arguments{
\item{...}{Arguments passed to \code{\link[=here_src]{here_src()}}.}

\item{recursive}{A \code{logical} variable passed to \code{\link[=list.files]{list.files()}}.}
}
\value{
The function returns \code{invisible(TRUE)}.
}
\description{
This function sources R scripts in \verb{src/}.
}
\author{
Edward Lavender
}
