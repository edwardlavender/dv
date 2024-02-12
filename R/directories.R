#' @title Directory helper: construct file paths
#' @description These functions are wrappers for [`file.path()`] and [`here::here()`] plus [`dir.create()`]. The functions require a character vector. If the directory name for the path does not exist, it is created recursively. The full path is then returned.
#' @param ... Character vectors (see [`file.path()`] and [`here::here()`]).
#' @details
#' `.path()` is an internal function. [`file_path()`] and [`here_path()`] are exported. Both of the latter functions are designed to be coupled to a `save` function (e.g., [`saveRDS()`]) because they both create a directory (if necessary) and return the path, which can then be passed to the `save` function.
#' @return The functions return `file.path(...)` or `here::here(...)`. If the path up to but excluding the last separator (i.e., `dirname(file.path(...))` or `dirname(here::here(...))`) does not exist, that part of the path is created as a side effect.
#' @examples
#' #### Example (1): Implementation when the dirname exists
#' # The function simply returns the path
#' file_path(tempdir(), "tmp.rds")
#'
#' #### Example (2): Implementation when the dirname does not exist
#' # The function creates the dirname (with a message) and returns the full path
#' file_path(tempdir(), "tmp_1", "tmp_2", "tmp.rds")
#' list.dirs(tempdir(), full.names = FALSE, recursive = TRUE)
#' # Reimplementing file_path() does not recreate existing directories
#' file_path(tempdir(), "tmp_1", "tmp_2", "tmp.rds")
#'
#' #### Example (3): Implementation within a `save` function
#' # The function is useful within a save function because it
#' # ... creates the necessary directory before the file is saved
#' saveRDS("x", file_path(tempdir(), "tmp", "data", "x.rds"))
#' list.files(tempdir(), recursive = TRUE)
#' @seealso [`here_`]
#' @author Edward Lavender
#' @name file_path

#' @rdname file_path
#' @keywords internal

.path <- function(..., f) {
  path <- f(...)
  if (tools::file_ext(path) == "") {
    rlang::abort("File path does not have an extension.")
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    message(glue::glue("New directory '{dir}' created."))
    dir.create(dir, recursive = TRUE)
  }
  path
}

#' @rdname file_path
#' @export

file_path <- function(...) {
  .path(..., f = file.path)
}

#' @rdname file_path
#' @export

here_path <- function(...) {
  rlang::check_installed("here")
  .path(..., f = here::here)
}

#' @title Directory helper: [`here::here()`] wrappers
#' @description These functions are simple wrappers for [`here::here()`] for standard RStudio Project directories created by [`use_template_proj()`].
#' @param ... Character vectors (see [`here::here()`]).
#' @return The functions return a file path.
#' @examples
#' here_data_raw()
#' here_data()
#' here_r()
#' here_dev()
#' here_fig()
#' here_doc()
#' @seealso [`file_path()`], [`here_path()`]
#' @author Edward Lavender
#' @name here_

#' @rdname here_
#' @export

here_data_raw <- function(...) here::here("data-raw", ...)

#' @rdname here_
#' @export

here_data <- function(...) here::here("data", ...)

#' @rdname here_
#' @export

here_r <- function(...) here::here("R", ...)

#' @rdname here_
#' @export

here_src <- function(...) here::here("src", ...)

#' @rdname here_
#' @export

here_dev <- function(...) here::here("dev", ...)

#' @rdname here_
#' @export

here_fig <- function(...) here::here("fig", ...)

#' @rdname here_
#' @export

here_doc <- function(...) here::here("doc", ...)

#' @title Source helpers
#' @description This function sources R scripts in `src/`.
#' @param ... Arguments passed to [`here_src()`].
#' @param recursive A `logical` variable passed to [`list.files()`].
#' @return The function returns `invisible(TRUE)`.
#' @author Edward Lavender
#' @export

src <- function(..., recursive = TRUE) {
  files <- list.files(here_src(...), full.names = TRUE, pattern = "\\.R$", recursive = recursive)
  if (length(files) > 0L) {
    lapply(files, source)
  }
  invisible(TRUE)
}

#' @title Directory helper: check path validity
#' @description This function identifies whether or not a file or directory path is valid and, if not, determines the valid portion of the path.
#' @param path A character string defining the file/directory path.
#'
#' @details This function is designed to facilitate quick identification of the valid/invalid components of manually specified paths (i.e., the point in the string at which the path becomes invalid).
#'
#' @return The function returns a character string. If `path`, as inputted, is valid, it is returned unchanged. If `path` is invalid, the portion of the path that is valid is returned.
#'
#' @examples
#' #### Example (1): Valid paths are returned unchanged
#' repair_path(tempdir())
#' file.create(file.path(tempdir(), "tmp.txt"))
#' repair_path(file.path(tempdir(), "tmp.txt"))
#'
#' #### Example (2): If `path` is invalid, the valid portion of the path is returned
#' # Path broken at the 'top level'
#' repair_path("temp")
#' # Path broken after tempdir()
#' repair_path(file.path(tempdir(), "temp"))
#'
#' #### Example (3): Examples with longer directories and files
#' # Generate new directory
#' dir.create(file.path(tempdir(), "temporary"), showWarnings = FALSE)
#' # Path broken after temporary/
#' repair_path(file.path(tempdir(), "temporary", "tmp2"))
#' # Create new file
#' file.create(file.path(tempdir(), "temporary", "file.txt"))
#' # File path is returned unchanged:
#' repair_path(file.path(tempdir(), "temporary", "file.txt"))
#' # Incorrectly specified path is broken after temporary/
#' repair_path(file.path(tempdir(), "temporary", "blah.txt"))
#'
#' @author Edward Lavender
#' @export

repair_path <- function(path) {
  valid <- any(dir.exists(path) | file.exists(path))
  if (valid) {
    cat("`path` as inputted is valid. Full `path` returned.\n")
  } else {
    cat("`path` as inputted is not valid. Valid portion of `path` returned.\n")
  }
  while (!valid && path != ".") {
    path <- dirname(path)
    valid <- dir.exists(path)
  }
  path
}
