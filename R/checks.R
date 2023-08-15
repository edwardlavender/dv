#' @title Check an object's class
#' @description This function checks an object's class.
#' @param x An object.
#' @param req A character that defines the required class.
#' @return If `x` does not inherit `req`, the function returns an error; otherwise `x` is invisibly returned.
#' @examples
#' \dontrun{
#' check_class(as.numeric(1:10), "numeric") # works
#' check_class(1:10, "character") # error
#' }
#' @author Edward Lavender
#' @export

check_class <- function(x, req) {
  if (!inherits(req, "character")) {
    rlang::abort("'req' should be a character.")
  }
  if (!inherits(x, req)) {
    rlang::abort(glue::glue("{deparse(substitute(x))} should be of class(es) {req}."))
  }
  invisible(x)
}


#' @title Check a directory exists
#' @description This function checks that a directory exists.
#' @param x A string that defines a directory.
#' @param type A character that defines whether or not to throw a warning or an error.
#' @return If the directory `x` does not exist, the function returns a warning or error; otherwise `x` is invisibly returned.
#' @examples
#' \dontrun{
#' check_dir_exists(tempdir()) # works
#' check_dir_exists(file.path(tempdir(), "blah")) # error
#' }
#' @author Edward Lavender
#' @export

check_dir_exists <- function(x, type = c("abort", "warn")) {
  type <- match.arg(type)
  exist <- sapply(x, dir.exists)
  if (any(!exist)) {
    msg <-
      glue::glue(
        "The following input(s) to 'x' do not exist: '",
        glue::glue_collapse(x[!exist], sep = "', '"), "'."
      )
    switch(type,
      abort = rlang::abort(msg),
      warn  = rlang::warn(msg)
    )
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

check_file_exists <- function(x, type = c("abort", "warn")) {
  type <- match.arg(type)
  exist <- sapply(x, file.exists)
  if (any(!exist)) {
    msg <- glue_items("These file(s) do not exist:", x[!exist])
    switch(type,
      abort = rlang::abort(msg),
      warn  = rlang::warn(msg)
    )
  }
  invisible(exist)
}

#' @title Check for unused files
#' @importFrom rlang .data
#' @description This function is designed to check for 'unused' files (e.g., datasets) in an `R` package.
#' @param path_check A string that defines the directory in which to check for unused files (e.g., `"./inst/"` or `"./inst/extdata/"` for `R` packages).
#' @param path_contains A character vector that defines the directory or directories that contain files expected to reference files in `path_check` by name.
#' @param pattern (optional) A character, a character vector or a list of characters, with one element for each element in `path_contains`, that defines the pattern(s) of files to select in `path_contains`. A list can be used to include `NULL` elements (see Examples).
#'
#' @details This function was motivated by the need to check that files stored in the `./inst/` directory of an `R` package are used by `R` scripts (in `./R/`) or associated files. In the early stages of package development, it is common to add files to a package that are later not required. This function flags unused files, supporting the development of 'clean' `R` packages.
#'
#' Under the default settings, the function checks for any elements in `./inst/` (`path_check`) that are not referenced (by name) within the contents of `./R/` (`path_contains`). Files are listed recursively in both directories.
#'
#' Note that the function identifies 'unused' files as those whose names are not directly quoted by any files in `path_contains`. Flagged files may contain 'used' files if file names are assembled (e.g., via [`paste()`]) rather than directly quoted by files in `path_contains`. Hence, the outputs of this function need to be inspected by a programmer with knowledge of the code in the package to check which subset of files is really unused.
#'
#' This function requires the `here` and `readr` packages.
#'
#' @return The function returns a [`character`] vector of the elements in `path_check` that are not directly referenced (by name) by files in `path_contains`.
#'
#' @examples
#' \dontrun{
#'
#' #### Example (1): Implement function with default options
#' check_unused()
#'
#' #### Example (2): Implement function with multiple `path_contains`
#' # This means that elements in `./inst/` will only be flagged as unused
#' # ... if they are not contained within `./R/` & .md files in `./inst/app/www/md/`
#' ck_1 <-
#'   check_unused(
#'     path_contains = c(
#'       here::here("R"),
#'       here::here("inst", "app", "www", "md")
#'     ),
#'     pattern = list(NULL, ".md")
#'   )
#' # Alternatively, you can implement the function for each `path_contains`
#' # ... And we can show that ck_1 is identical to the intersection between
#' # ... both of those queries
#' ck_2 <- check_unused(path_contains = here::here("R"))
#' ck_3 <- check_unused(
#'   path_contains = here::here("inst", "app", "www", "md"),
#'   pattern = ".md"
#' )
#' identical(ck_1, Reduce(intersect, list(ck_2, ck_3)))
#' }
#'
#' @author Edward Lavender
#' @keywords internal

check_unused <- function(path_check = here::here("inst"),
                         path_contains = here::here("R"),
                         pattern = NULL, ...) {
  #### Checks
  rlang::check_installed(c("here", "readr"))
  if (is.null(pattern)) pattern <- lapply(length(path_contains), function(i) NULL)
  if (!is.null(pattern) && length(pattern) != length(path_contains)) {
    stop("`pattern` and `path_contains` should contain the same number of elements.", call. = FALSE)
  }
  if (!inherits(path_contains, "character")) {
    stop("`path_contains` should be a character vector.", call. = FALSE)
  }

  #### List files in path_check
  # These files are expected to be named (called) by files in path_contains
  file_check <- list.files(path_check, recursive = TRUE)

  #### List files in path_contains
  # These are the files that should contain references to files in path_check
  file_contain <-
    mapply(
      function(path, patt) {
        f <- list.files(path, pattern = patt, full.names = TRUE, recursive = TRUE)
        if (length(f) == 0L) stop("No files contained in path.", call. = FALSE)
        f
      }, path_contains, pattern,
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    ) |>
    unlist()

  #### Read files in path_contains & check they contain references to files in path_check
  check <-
    lapply(file_contain, function(f) {
      # Load a file
      txt <- readr::read_file(f)
      # Define a dataframe with:
      # ... a) the file name
      # ... b) whether or not the file text contains a reference to any file names in path_check
      data.frame(
        file_contain = f,
        file_check = file_check,
        present = stringr::str_detect(txt, basename(file_check))
      )
    }) |>
    dplyr::bind_rows()

  #### Extract the files in path_check that are not referenced (directly) by any files in path_contain
  check |>
    dplyr::group_by(.data$file_check) |>
    dplyr::summarise(count = sum(.data$present + 0)) |>
    dplyr::filter(.data$count == 0) |>
    dplyr::pull(.data$file_check)
}
