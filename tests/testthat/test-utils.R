test_that("glue_items() works.", {
  expect_equal(as.character(glue_items("A list:", letters[1])),
               "A list: 'a'.\n")

  expect_equal(as.character(glue_items("A list:", letters[1:3])),
               "A list: 'a', 'b', 'c'.\n")
})
