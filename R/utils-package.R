#' @title Clear the console
#' @description This function is a wrapper for `cat("\014")`.
#' @export
clear <- function() cat("\014")

#' @title Glue items into a sentence
#' @param text A character string.
#' @param items A character vector.
#' @details This is a simple wrapper for [`glue::glue()`] that appends a list of items to an introductory sentence.
#' @return The function returns a character string.
#' @examples
#' \dontrun{
#'
#' glue_items("This is a list of item(s):", letters[1:5])
#' }
#' @author Edward Lavender
#' @keywords internal

glue_items <- function(text, items) {
  if (all(items == "")) {
    rlang::warn("`items` is an empty string ('').")
    out <- paste0(text, "\n")
  } else {
    out <- glue::glue(
      "{text} '",
      glue::glue_collapse(items, sep = "', '"), "'.\n
             "
    )
  }
  out
}
