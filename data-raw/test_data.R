# Script to import and create a small data set to use for testing functions
# for development purposes. Run this script when

# NOTE: your .Renviron file needs to be up to date, see .Renviron.example

path_2023 <- paste0(Sys.getenv("PATH_SAS_SERVER"), "MAIN3_2023_24.sas7bdat")
path_2024 <- paste0(Sys.getenv("PATH_SAS_SERVER"), "MAIN2_2024_25.sas7bdat")

scjs_vars <- c(
  "PREVPROPERTY", # crime - combined weight
  "PREVVIOLENT", # crime - individually weighted
  "PREVHOUSEBREAK", # crime - household weight
  "QSFDARK", # full sample question - can be aggregated
  "QDCONF_03", # partial sample question
  "SIMD_15MOST", # demographic
  "WGTGINDIV", # individual weight
  "WGTGHHD" # household weight
)

df_test_2023 <- haven::read_sas(path_2023, col_select = all_of(scjs_vars)) |>
  dplyr::mutate(year = 2023, survey_year = "2023/24", .before = 1)
df_test_2024 <- haven::read_sas(path_2024, col_select = all_of(scjs_vars)) |>
  dplyr::filter(!is.na(QSFDARK)) |> #clear up bug where one row had all NA
  dplyr::mutate(year = 2024, survey_year = "2024/25", .before = 1)

scjs_test_data <- dplyr::bind_rows(df_test_2023, df_test_2024) |>
  dplyr::mutate(serial_test = dplyr::row_number(), .before = 1)

# Save as an internal dataset in sysdata.rda
usethis::use_data(df_test_2023, internal = TRUE, overwrite = TRUE)
usethis::use_data(df_test_2024, internal = TRUE, overwrite = TRUE)
usethis::use_data(scjs_test_data, internal = TRUE, overwrite = TRUE)
