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

vm_sub <- subset_variable_map(vm_nvf$overview, c("wgtgindiv", "prevviolent", "qsfdark"), c(2018, 2019), names_from="original", keep_all_vars = F)
df_test <- nvf_2019 |> dplyr::select(serial2, year, wgtgindiv, prevviolent, qsfdark)

harmonise_func <- function(dataset, variable_map, options) {

  # get the variable map sections - necessary if the package can see all of them anyway?
  vm_sections <- dplyr::pull(variable_map, var = "section_or_module")

  # extract the variables from the variable map subset(s)

  # sort the variable maps into groups? - would this speed things up? - probably if have to split variable map into list etc.
  # or join the variable map sub sheets?

  # reduce the dataset to only the necessary variables?

}


vm_split <- split(vm_sub, vm_sub$section_or_module)

vm_split[1]

vm_nvf[["demographics"]] |> dplyr::filter(new_var %in% c("feelings_of_safety_tabulated"))

lookup <- vm_nvf[["demographics"]] |>
  dplyr::filter(new_var %in% c("feelings_of_safety_tabulated")) |>
  (\(data) {
    vars <- split(data, data$new_var)
    purrr::map(vars, \(x) setNames(x$new_val, x$old_val))
  })()

df_test <- df_test |>
  dplyr::mutate(feelings_of_safety_tabulated = qsfdark, .after = qsfdark)

var <- "feelings_of_safety_tabulated"

#works (but need to copy the variable)
df_test <- df_test |>
  (\(d) {
    d[[var]] <- factor(
      lookup[[var]][as.character(d[[var]])], # look at the thing in the column of interest, find the thing with that name in lookup, returning the value
      levels = unique(lookup[[var]]) # don't strictly need levels
    )
    d
  })()

# works with no levels
df_test <- df_test |>
  (\(d) {
    d[[var]] <- factor(
      lookup[[var]][as.character(d[[var]])] # look at the thing in the column of interest, find the thing with that name in lookup, returning the value
    )
    d
  })()

# works but still has explicit references to col names
df_test <- df_test |>
  dplyr::mutate(
    qsfdark = factor(
      lookup[[var]][as.character(qsfdark)],
      levels = unique(lookup[[var]])
    )
  )

# works as across and anonymous function, still uses direct reference to 'pipeline var' name
df_test <- df_test |>
  dplyr::mutate(
    dplyr::across(
      all_of("qsfdark"),
      \(x) factor(
        lookup[[var]][as.character(x)],
        levels = unique(lookup[[var]])
      )
    )
  )
lookup[["feelings_of_safety_tabulated"]][as.character(-2)]

# looks up the element with the name "4" - and returns the corresponding value - double [[]] in second part returns just the value itself
lookup[["feelings_of_safety_tabulated"]]["4"]
lookup[["feelings_of_safety_tabulated"]][["4"]] + 5
