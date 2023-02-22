#' @title `dv`: Development tools for R
#' @author Edward Lavender
#'
#' @description `dv` provides tools to support the development of RStudio projects and R Packages.
#'
#' ## Directory helpers
#'
#' * [`file_path()`] and [`here_path()`] construct file paths;
#' * [`here_()`] functions are [`here::here()`] wrappers for standard directories;
#' * [`repair_path()`]  checks file/directory path validity;
#'
#' ## RStudio Project tools (`use_*`)
#'
#' * [`use_template_proj()`] creates a template RStudio Project;
#' * [`use_template_gitignore()`] creates a template .gitignore file;
#' * [`use_template_readme()`] creates a template README file;
#' * [`use_template_script()`] creates a template script;
#' * [`use_template_tree()`] creates a directory tree;
#'
#' ## R package checks (`check_*`)
#'
#' * [`check_class`] checks an object's class
#' * [`check_dir_exists()`] checks a directory exists;
#' * [`check_file_exists()`] checks a file exists;
#'
#' @seealso <https://github.com/edwardlavender/dv>
#'
#' @docType package
#' @name dv
NULL
