scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)


df_nvf <- purrr::map(scjs_data, ~ dplyr::select(.x, WGTGINDIV)) |> purrr::list_rbind()



subset_variable_map("abc", c("wgtgindiv", "wgtghhd"), c(2018, 2019), names_from="original", find_all_vars = F)
subset_variable_map(vm_nvf$overview, c(), c(2018, 2019), names_from="original", find_all_vars = F)
subset_variable_map(vm_nvf$overview, c(""), c(2018, 2019), names_from="original", find_all_vars = F)
subset_variable_map(vm_nvf$overview, c("wgtgindiv", "wgtghhd"), c(2018, 2019), names_from="original", find_all_vars = F)
subset_variable_map(vm_nvf$overview, c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", find_all_vars = T)
subset_variable_map(vm_nvf$overview, c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", find_all_vars = F)
subset_variable_map(vm_nvf$overview, c("weight_indiv"), c(2021), names_from="pipeline")
subset_variable_map(vm_nvf$overview, c("weight_indiv"), c(2000), names_from="pipeline") # expect error
subset_variable_map(vm_nvf$overview, c("INVALID"), c(2000), names_from="pipeline") # expect error


scjs_harmonise_variable(.data="create", scjs_data, c("wgtgindiv", "wgtghhd"))
scjs_harmonise_variable(data.frame(), scjs_data, c("wgtgindiv", "wgtghhd"), find_all_vars = TRUE)
scjs_harmonise_variable(data.frame(), scjs_data, c("weight_indiv", "weight_household"), names_from="pipeline")

# test invalid entries in var_list
scjs_harmonise_variable(vm_nvf$overview, scjs_data, c("askdjfhaks"), names_from="pipeline")
scjs_harmonise_variable(data.frame(), scjs_data, c("weight_indiv", "askdjfhaks"), names_from="pipeline")




# base case
df <- scjs_harmonise_variable("create", scjs_data, var_list = c("safety_walkingalone"), names_from = "pipeline")

# easy pipe
df2 <- df |>
  scjs_harmonise_variable(scjs_data, var_list = c("disability"), names_from = "pipeline")

# 2 function calls + pipe
df3 <- scjs_harmonise_variable("create", scjs_data, var_list = c("safety_walkingalone"), names_from = "pipeline") |>
  scjs_harmonise_variable(scjs_data, var_list = c("disability"), names_from = "pipeline")

# input is single dataset - not list
df4 <-
  scjs_harmonise_variable(data = "create", nvf_2018, var_list = c("disability"), names_from = "pipeline")

# check returns nothing
df5 <-
  scjs_harmonise_variable(data = "create", scjs_data, var_list = c("disability"), names_from = "pipeline", harmonise_as = "none")

# check drop_original_cols arg
df6 <-
  scjs_harmonise_variable(data = "create", nvf_2018, var_list = c("disability"), names_from = "pipeline", drop_original_cols = TRUE)

# check names_from = "original"
df7 <-
  scjs_harmonise_variable(data = "create", nvf_2018, var_list = c("qsfdark"), find_all_vars = TRUE)

df8 <- df7 |>
  scjs_harmonise_variable(nvf_2018, var_list = c("qsfdark"), find_all_vars = TRUE, join_method = "append")

# var_list length > 1
df9 <-
  scjs_harmonise_variable(data = "create", nvf_2018, var_list = c("qsfdark", "prevviolent"), find_all_vars = TRUE)

# var_list has variable i know is missing from original data
df10 <-
  scjs_harmonise_variable(data = "create", scjs_data, var_list = c("qsfdark", "prevsurveycrime"), find_all_vars = FALSE)

df11 <-
  scjs_harmonise_variable(data = "create", scjs_data, var_list = c("disability", "prev_traditionalcrime"), names_from = "pipeline")

foo <- subset_variable_map(vm_nvf$overview, c("qcondit and qlimit", "prevsurveycrime", "qsfdark", "wgtgindiv", "wgtghhd"), c(2018, 2019), names_from="original", find_all_vars = F) |>
  dplyr::select(dplyr::starts_with("2"), var_name) |>
  tidyr::pivot_longer(names_to = "year", values_to = c("original_var"), cols = dplyr::starts_with("2"))

subset_variable_map(vm_nvf$overview, c("prevsurveycrime", "qsfdark", "wgtgindiv", "wgtghhd"), c(2018, 2019), names_from="original", find_all_vars = F) |>
  dplyr::select(var_name, dplyr::starts_with("2")) |>
  tidyr::pivot_longer(names_to = "year", values_to = c("var"), cols = dplyr::starts_with("2"))

foo |>
  dplyr::select(dplyr::starts_with("2"), var_name) |>
  tidyr::separate_wider_delim(cols = dplyr::starts_with("2"), delim = stringr::regex("( and )|( or )"), names_sep = c("sep"), too_few = "align_start") |>
  tidyr::pivot_longer(names_to = "year", values_to = c("original_var"), cols = dplyr::starts_with("2")) |>
  dplyr::filter(!is.na(original_var)) |>
  dplyr::mutate(year = substr(year, 1, 4))

stringr::regex("( and )|( or )")
stringr::str_detect("qcondit and qlimit", stringr::regex("( and )|( or )"))

foo2 <- split(foo, foo$year)

bar <- dplyr::pull(foo, var = `2018`)

setdiff(bar, names(nvf_2018))

