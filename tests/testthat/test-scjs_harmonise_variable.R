# Test scjs_harmonise_variable()

test_that("scjs_harmonise_variable() gives an error when the original data is not a list", {
  expect_error(scjs_harmonise_variable(df_list = c("abc", "def"), var_list = c("xyz")), "must be a list")
})

test_that("scjs_harmonise_variable() gives an error when the vars to harmonise is not a character", {
  expect_error(scjs_harmonise_variable(df_list = list("abc", "def"), var_list = 1), "must be a character")
})

test_that("scjs_harmonise_variable() gives an error when the names_from arg is invalid", {
  expect_error(scjs_harmonise_variable(df_list = list(data.frame(year=2000), data.frame(year=2001)), var_list = "abc", names_from="err"), "'names_from' must be one of")
})

test_that("scjs_harmonise_variable() gives an error when keep_all_vars is not TRUE or FALSE", {
  expect_error(scjs_harmonise_variable(df_list = list(data.frame(year=2000), data.frame(year=2001)), var_list = "abc", keep_all_vars = "abc"), "must be TRUE or FALSE")
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

# Test convert_financial_year()
test_that("convert_financial_year() successfully converts valid input years", {
  expect_equal(convert_financial_year(2012), 2012)
  expect_equal(convert_financial_year("2012"), 2012)
  expect_equal(convert_financial_year("2012/13"), 2012)
  expect_equal(convert_financial_year("12/13"), 2012)
})

test_that("convert_financial_year() rejects invalid inpurs ", {
  expect_error(convert_financial_year("abc"), "Could not convert item in year to numeric")
  expect_error(convert_financial_year("123"), "Could not convert item in year to numeric")
  expect_error(convert_financial_year("12345/67"), "Length of extracted year is not of length 2 or 4")
  expect_error(convert_financial_year("12345/678"), "Length of extracted year is not of length 2 or 4")
})

# Test subset_variable_map()
test_that("subset_variable_map()")
