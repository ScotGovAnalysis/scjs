scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)


df_nvf <- purrr::map(scjs_data, ~ dplyr::select(.x, WGTGINDIV)) |> purrr::list_rbind()



subset_variable_map("vm_nvf_overview", c("wgtgindiv", "wgtghhd"), c(2018, 2019), names_from="original", keep_all_vars = F)
subset_variable_map("vm_nvf_overview", c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", keep_all_vars = T)
subset_variable_map("vm_nvf_overview", c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", keep_all_vars = F)
subset_variable_map("vm_nvf_overview", c("weight_indiv"), c(2021), names_from="pipeline")


scjs_harmonise_variable(.data="create", scjs_data, c("wgtgindiv", "wgtghhd"))
scjs_harmonise_variable(scjs_data, c("wgtgindiv", "wgtghhd"), keep_all_vars = TRUE)
scjs_harmonise_variable(scjs_data, c("weight_indiv", "weight_household"), names_from="pipeline")

# test invalid entries in var_list
scjs_harmonise_variable(scjs_data, c("askdjfhaks"), names_from="pipeline")
scjs_harmonise_variable(scjs_data, c("weight_indiv", "askdjfhaks"), names_from="pipeline")


