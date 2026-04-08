scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)


df_nvf <- purrr::map(scjs_data, ~ dplyr::select(.x, WGTGINDIV)) |> purrr::list_rbind()

scjs_load_ukds(path = Sys.getenv())
