test_that("check_class() works", {
  expect_invisible(check_class("a", "character"))
  expect_error(check_class("a", numeric))
  expect_error(check_class("a", "numeric"))
})

test_that("check_dir_exists() works", {
  expect_invisible(check_dir_exists(tempdir()))
  expect_error(check_dir_exists(file.path(tempdir(), "blah")))
})

test_that("check_file_exists() works", {
  expect_invisible(check_file_exists(tempdir()))
  expect_error(check_file_exists(file.path(tempdir(), "blah")))
})
