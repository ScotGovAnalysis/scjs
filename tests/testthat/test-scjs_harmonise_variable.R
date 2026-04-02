# Test scjs_harmonise_variable()

test_that("scjs_harmonise_variable() gives an error when the original data is not a list", {
  expect_error(scjs_harmonise_variable(original_data = c("abc", "def"), vars = c("xyz")), "must be a list")
})

test_that("scjs_harmonise_variable() gives an error when the vars to harmonise is not a character", {
  expect_error(scjs_harmonise_variable(original_data = list("abc", "def"), vars = 1), "must be a character")
})

