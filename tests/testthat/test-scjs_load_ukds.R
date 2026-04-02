# Test check_years ####

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


# Test fetch_full_path ####
# Uses the mockery package to create some 'mock' data as we don't have access to the same files when testing
# stub() essentially says that in fetch_full_path(), we manually overwrite the call to list.files() with a func that returns "file1.sav"
# We also create a fake lookup table, needs to have 4 columns as the fetch_full_path() function reads a lookup table with at least 4 cols
test_that("fetch_full_path() returns path when file exists", {
  mock_lookup <- data.frame(a=1, b=2, folder="mock_folder", filename="file1.sav")

  mockery::stub(fetch_full_path, "list.files", function(...) "file1.sav")

  out <- fetch_full_path("base/", mock_lookup, 1)
  expect_equal(out, "base/mock_folder/file1.sav")
})

test_that("fetch_full_path() returns NA when no file is found", {
  mock_lookup <- data.frame(a=1, b=2, folder="mock_folder", filename="file1.sav")

  mockery::stub(fetch_full_path, "list.files", function(...) character(0))

  out <- fetch_full_path("base/", mock_lookup, 1)
  expect_true(is.na(out))
})


# Test read_sav_data ####
test_that("read_sav_data() loads data when filepath valid", {
  # mock_read_sav <- function(...) data.frame(x=1)
  mockery::stub(read_sav_data, "haven::read_sav", function(...) data.frame(x=1))

  out <- read_sav_data("mock.sav", NULL, "nvf", 2014)
  expect_s3_class(out, "data.frame")
})

test_that("read_sav_data() returns NA when filepath is invalid", {
  out <- read_sav_data(NA, NULL, "nvf", 2014)
  expect_true(is.na(out))
})

# Test scjs_load_ukds() ####
test_that("scjs_load_ukds() rejects invalid dataset types", {
  expect_error(scjs_load_ukds("path", dataset_type="wrong"), "Invalid dataset type")
})


test_that("scjs_load_ukds() loads correct number of datasets", {
  ukds_lookup <- data.frame(
    year = c(2015),
    nvf_folder = c("/f1", "/f2"),
    filename = c("file1", "file2")
  )

  mock_fetch <- function(...) "mock.sav"
  mock_read <- function(...) data.frame(a=1)

  mockery::stub(scjs_load_ukds, "fetch_full_path", mock_fetch)
  mockery::stub(scjs_load_ukds, "read_sav_data", mock_read)

  out <- scjs_load_ukds(".", years_to_load = c(2016, 2017))
  expect_length(out, 2)
  expect_named(out, c("nvf_2016", "nvf_2017"))
})


test_that("scjs_load_ukds() drops NA results", {
  mock_fetch <- function(path, lookup, index) if (index == 1) NA else "mock.sav"
  mock_read  <- function(...) data.frame(a=1)
  mock_result <- list(nvf_2016=NA, nvf_2017=data.frame(a=1))

  mockery::stub(scjs_load_ukds, "fetch_full_path", mock_fetch)
  mockery::stub(scjs_load_ukds, "read_sav_data", mock_read)
  mockery::stub(scjs_load_ukds, "purrr::set_names", mock_result)

  out <- scjs_load_ukds(".", years_to_load = c(2016, 2017))
  expect_length(out, 1)
  expect_named(out, "nvf_2017")
})

test_that("scjs_load_ukds() returns name prefix", {
  mock_fetch <- function(...) "mock.sav"
  mock_read  <- function(...) data.frame(a=1)
  mock_result <- list(test_prefix_2015=NA, test_prefix_2016=data.frame(a=1))

  mockery::stub(scjs_load_ukds, "fetch_full_path", mock_fetch)
  mockery::stub(scjs_load_ukds, "read_sav_data", mock_read)
  mockery::stub(scjs_load_ukds, "purrr::set_names", mock_result)

  out <- scjs_load_ukds(".", years_to_load = c(2015, 2016), name_prefix = "test_prefix")
  expect_length(out, 1)
  expect_named(out, "test_prefix_2016")
})
