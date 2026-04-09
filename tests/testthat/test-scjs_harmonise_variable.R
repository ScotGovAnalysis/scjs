# Test scjs_harmonise_variable()

test_that("scjs_harmonise_variable() gives an error when the original data is not a list", {
  expect_error(scjs_harmonise_variable(df_list = c("abc", "def"), vars = c("xyz")), "must be a list")
})

test_that("scjs_harmonise_variable() gives an error when the vars to harmonise is not a character", {
  expect_error(scjs_harmonise_variable(df_list = list("abc", "def"), vars = 1), "must be a character")
})

# Test extract_year_var()
test_that("extract_year_var() returns an error when 'year' is not present in data", {
  expect_error(extract_year_var(data.frame(A=1)), "'year' is not in data frame")
})

test_that("extract_year_var() works on a single dataframe", {
  expect_equal(extract_year_var(data.frame(year=2000)), c(2000))
})

test_that("extract_year_var() returns a vector of years when a list is passed", {
  expect_equal(extract_year_var(list(data.frame(year=2000), data.frame(year=2001))), c(2000, 2001))
})
