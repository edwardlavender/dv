#' @title Open a file
#' @description This function opens input file(s) (or directories).
#' @param file A system path.
#' @return The function is called for the side effect of opening a file.
#' @author Edward Lavender
#' @name open_sys

#' @rdname open_sys
#' @keywords internal

.open_sys <- function(file) system(sprintf("open %s", shQuote(file)))

#' @rdname open_sys
#' @export
open_sys <- function(file) invisible(sapply(file, .open_sys))


#' @title Optionally run an expression
#' @description This function optionally runs an expression & saves the output to a file, if the file does not exist (or `overwrite = TRUE`) or reads the file.
#' @param file A character that defines the file path.
#' @param overwrite A logical variable that defines whether or not to overwrite `file`, if it already exists. A `message` is given if so.
#' @param expr An expression to run.
#' @param read,write Read and write functions.
#' @param verbose A logical variable that defines whether or not to print the time.
#' @details This function a simple wrapper for:
#' `
#' if (!file.exists(file)) {
#'   out <- eval(substitute(expr))
#'   write(out)
#' } else {
#'   out <- read(file)
#' }
#' `
#'
#' @examples
#' f <- tempfile()
#' a <- run(f, overwrite = FALSE, Sys.time())
#' Sys.sleep(0.5)
#' b <- run(f, overwrite = FALSE, Sys.time())
#' stopifnot(all.equal(a, b))
#'
#' @return The function invisibly returns \code{NULL} or the output of \code{expr}.
#' @author Edward Lavender
#' @export

run <- function(file, overwrite = FALSE, expr, read = readRDS, write = saveRDS, verbose = TRUE) {
  if (verbose) {
    rlang::check_installed("tictoc")
    tictoc::tic()
  }
  out <- NULL
  if (overwrite | !file.exists(file)) {
    if (file.exists(file)) {
      message(glue::glue("'{basename(file)}' will be overwritten."))
    }
    stopifnot(dir.exists(dirname(file)))
    out <- eval(substitute(expr))
    if (!is.null(write)) {
      success <- tryCatch(write(out, file), error = function(e) e)
      if (inherits(success, "error")) {
        rlang::warn(paste("Writing the file failed with the following error:", as.character(success)))
      }
    }
  } else {
    if (!is.null(read)) {
      out <- read(file)
    }
  }
  if (verbose) {
    tictoc::toc()
  }
  invisible(out)
}
