# Test check_years

test_that("check_years() accepts a valid single SCJS year", {
  expect_silent(check_years(2019))
})

test_that("check_years() accepts a valid input of multiple SCJS years", {
  expect_silent(check_years(c(2008,2009,2010)))
  expect_silent(check_years(2010:2020))
})

test_that("check_years() rejects a year where an SCJS did not take place", {
  expect_error(check_years(2011), "not a valid SCJS year")
  expect_error(check_years(2010.1), "not a valid SCJS year")
})

test_that("check_years() rejects in put with a year outside the valid range", {
  expect_error(check_years(2000:2010), "outside the range of valid years")
  expect_error(check_years(2015:max(ukds_lookup[,1]) + 1))
})


# Test fetch_full_path
# Uses the mockery package to create some 'mock' data as we don't have access to the same files when testing
# stub() essentially says that in fetch_full_path(), we manually overwrite the call to list.files() with a func that returns "file1.sav"
# We also create a fake lookup table, needs to have 4 columns as the fetch_full_path() function reads a lookup table with at least 4 cols

test_that("fetch_full_path() returns path when file exists", {
  mock_lookup <- data.frame(a=1, b=2, folder="mock_folder", filename="file1.sav")

  mockery::stub(fetch_full_path, "list.files", function(...) "file1.sav")

  out <- fetch_full_path("base/", mock_lookup, 1)
  expect_equal(out, "base/mock_folder/file1.sav")
})


