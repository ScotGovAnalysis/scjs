scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)


df_nvf <- purrr::map(scjs_data, ~ dplyr::select(.x, WGTGINDIV)) |> purrr::list_rbind()



subset_variable_map(vm_nvf$overview, c("wgtgindiv", "wgtghhd"), c(2018, 2019), names_from="original", keep_all_vars = F)
subset_variable_map(vm_nvf$overview, c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", keep_all_vars = T)
subset_variable_map(vm_nvf$overview, c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", keep_all_vars = F)
subset_variable_map(vm_nvf$overview, c("weight_indiv"), c(2021), names_from="pipeline")


scjs_harmonise_variable(.data="create", scjs_data, c("wgtgindiv", "wgtghhd"))
scjs_harmonise_variable(data.frame(0), scjs_data, c("wgtgindiv", "wgtghhd"), keep_all_vars = TRUE)
scjs_harmonise_variable(data.frame(0), scjs_data, c("weight_indiv", "weight_household"), names_from="pipeline")

# test invalid entries in var_list
scjs_harmonise_variable(data.frame(0), scjs_data, c("askdjfhaks"), names_from="pipeline")
scjs_harmonise_variable(data.frame(0), scjs_data, c("weight_indiv", "askdjfhaks"), names_from="pipeline")




# harmonisation notes

# harmonised var: return could be "data" "label" "both"? - what about vars that don't have a label (i.e. weight?)
  # will have to harmonise based on data
  # create a column of the label
  # have to drop the data column then rename the label column if that is specified


# original vars: keep original = T/F

# when requires recode is FALSE: still might need to get the label

# harmonisation is only based on data and labels is handled separately?
# could reuse the label getting function for outside use - e.g. a separate function call to get labels for original variables


# order of execution

# function sees:
# one data set (for one year)
# the variable map subset
# optional arguments for function (keep originals, keep labels or data)

# subset the original data to the necessary variables (based on what's in that year's column in variable map)
# extract a vector of the pipeline vars and the original vars (for that year) - make it a named vector?

# read in the relevant section of the variable map with the recoding instructions

vm_sub <- subset_variable_map("vm_nvf_overview", c("wgtgindiv", "qsfdark"), c(2018, 2019), names_from="original", keep_all_vars = F)
df_test <- nvf_2019 |> dplyr::select(serial2, wgtgindiv, qsfdark)

harmonise_func <- function(dataset, variable_map, options) {

  # get the variable map sections


}
