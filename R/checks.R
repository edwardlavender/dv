#' @title Check an object's class
#' @description This function checks an object's class.
#' @param x An object.
#' @param req A character that defines the required class.
#' @return If `x` does not inherit `req`, the function returns an error; otherwise `x` is invisibly returned.
#' @examples
#' \dontrun{
#' check_class(as.numeric(1:10), "numeric") # works
#' check_class(1:10, "character")           # error
#' }
#' @author Edward Lavender
#' @export

check_class <- function(x, req){
  if (!inherits(req, "character"))
    rlang::abort("'req' should be a character.")
  if (!inherits(x, req))
    rlang::abort(glue::glue("{deparse(substitute(x))} should be of class(es) {req}."))
  invisible(x)
  }


#' @title Check a directory exists
#' @description This function checks that a directory exists.
#' @param x A string that defines a directory.
#' @param type A character that defines whether or not to throw a warning or an error.
#' @return If the directory `x` does not exist, the function returns a warning or error; otherwise `x` is invisibly returned.
#' @examples
#' \dontrun{
#' check_dir_exists(tempdir())                     # works
#' check_dir_exists(file.path(tempdir(), "blah"))  # error
#' }
#' @author Edward Lavender
#' @export

check_dir_exists <- function(x, type = c("abort", "warn")){
  type <- match.arg(type)
  exist <- sapply(x, dir.exists)
  if (any(!exist)) {
    msg <-
      glue::glue("The following input(s) to 'x' do not exist: '",
             glue::glue_collapse(x[!exist], sep = "', '"), "'.")
    switch(type,
           abort = rlang::abort(msg),
           warn  = rlang::warn(msg))
  }
  invisible(x)
}


#' @title Check a file exists
#' @description This function checks that a file exists.
#' @param x A string that defines a file path.
#' @param type A character that defines whether or not to throw a warning or an error.
#' @examples
#' \dontrun{
#' check_file_exists(letters[1], type = "warn")
#' check_file_exists(letters[1:3], type = "warn")
#' check_file_exists(letters[1:3], type = "abort")
#' }
#' @return If the file `x` does not exist, the function returns a warning or error; otherwise a logical vector that defines whether or not each file exists is returned.
#' @author Edward Lavender
#' @export

check_file_exists <- function(x, type = c("abort", "warn")){
  type <- match.arg(type)
  exist <- sapply(x, file.exists)
  if (any(!exist)) {
    msg <- glue_items("These file(s) do not exist:", x[!exist])
    switch(type,
           abort = rlang::abort(msg),
           warn  = rlang::warn(msg))
  }
  invisible(exist)
}
