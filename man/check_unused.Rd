% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_unused}
\alias{check_unused}
\title{Check for unused files}
\usage{
check_unused(
  path_check = here::here("inst"),
  path_contains = here::here("R"),
  pattern = NULL,
  ...
)
}
\arguments{
\item{path_check}{A string that defines the directory in which to check for unused files (e.g., \code{"./inst/"} or \code{"./inst/extdata/"} for \code{R} packages).}

\item{path_contains}{A character vector that defines the directory or directories that contain files expected to reference files in \code{path_check} by name.}

\item{pattern}{(optional) A character, a character vector or a list of characters, with one element for each element in \code{path_contains}, that defines the pattern(s) of files to select in \code{path_contains}. A list can be used to include \code{NULL} elements (see Examples).}
}
\value{
The function returns a \code{\link{character}} vector of the elements in \code{path_check} that are not directly referenced (by name) by files in \code{path_contains}.
}
\description{
This function is designed to check for 'unused' files (e.g., datasets) in an \code{R} package.
}
\details{
This function was motivated by the need to check that files stored in the \verb{./inst/} directory of an \code{R} package are used by \code{R} scripts (in \verb{./R/}) or associated files. In the early stages of package development, it is common to add files to a package that are later not required. This function flags unused files, supporting the development of 'clean' \code{R} packages.

Under the default settings, the function checks for any elements in \verb{./inst/} (\code{path_check}) that are not referenced (by name) within the contents of \verb{./R/} (\code{path_contains}). Files are listed recursively in both directories.

Note that the function identifies 'unused' files as those whose names are not directly quoted by any files in \code{path_contains}. Flagged files may contain 'used' files if file names are assembled (e.g., via \code{\link[=paste]{paste()}}) rather than directly quoted by files in \code{path_contains}. Hence, the outputs of this function need to be inspected by a programmer with knowledge of the code in the package to check which subset of files is really unused.

This function requires the \code{here} and \code{readr} packages.
}
\examples{
\dontrun{

#### Example (1): Implement function with default options
check_unused()

#### Example (2): Implement function with multiple `path_contains`
# This means that elements in `./inst/` will only be flagged as unused
# ... if they are not contained within `./R/` & .md files in `./inst/app/www/md/`
ck_1 <-
  check_unused(
    path_contains = c(
      here::here("R"),
      here::here("inst", "app", "www", "md")
    ),
    pattern = list(NULL, ".md")
  )
# Alternatively, you can implement the function for each `path_contains`
# ... And we can show that ck_1 is identical to the intersection between
# ... both of those queries
ck_2 <- check_unused(path_contains = here::here("R"))
ck_3 <- check_unused(
  path_contains = here::here("inst", "app", "www", "md"),
  pattern = ".md"
)
identical(ck_1, Reduce(intersect, list(ck_2, ck_3)))
}

}
\author{
Edward Lavender
}
\keyword{internal}
