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

vm_sub <- subset_variable_map(vm_nvf$overview, c("wgtgindiv", "prevviolent", "qsfdark", "qcondit and qlimit"), c(2018, 2019), names_from="original", keep_all_vars = F)
df_test <- nvf_2019 |> dplyr::select(serial2, year, wgtgindiv, prevviolent, qsfdark, qcondit, qlimit)

process_harmonise_df <- function(dataset, variable_map, keep_columns_as="labels") {

  # grab the year of the dataset
  year <- dplyr::pull(dataset, var = year)[1]

  # process the variable maps
  vm_combined <- vm_combine_sheets(vm_sub, year) # combine relevant sections from individual sheets
  vm_split <- split(vm_combined, vm_combined[["new_var"]]) # split the combined vm into a list of dfs

  # create the list structure for the combined variable map
  if (!(keep_columns_as %in% c("data", "labels"))) {
    stop("Argument keep_columns_as must be equal to one of: 'data' or 'labels'.")
  }

  # control inputs to vm_split_to_lists() based on value in 'keep_columns_as'
  if (keep_columns_as == "labels") {
    vm_processed_list <- vm_split_to_lists(vm_combined, old_col = "old_val", new_col = "new_lab")
  } else if (keep_columns_as == "data") {
    vm_processed_list <- vm_split_to_lists(vm_combined, old_col = "old_val", new_col = "new_val")
  }


  # preprocess the dataset
  df_preprocess <- purrr::reduce(vm_split, harmonise_preprocess_df, .init = dataset)

  # replace the preprocessed values with the new harmonised variable values
  df_processed <- purrr::reduce2(vm_processed_list, names(vm_processed_list), harmonise_replace_values, .init = df_preprocess)

  return(df_processed)

}

# Filters the relevant variable map sheet to just the variables being processed
get_var_maps <- function(vm, vm_sheet) {

  # not all vars passed will have a recoded version, so return a blank list when that happens
  if (is.null(vm[[names(vm_sheet)]])) return(list())

  # filter to only the relevant info to recode for that year - convert to string and remove any spaces for ease
  vm_int <- vm[[names(vm_sheet)]] |>
    dplyr::filter(new_var %in% names(vm_sheet[[1]]),
                  old_var %in% vm_sheet[[1]]) |>
    dplyr::mutate(across(c(old_val, new_val), ~ as.character(stringr::str_replace(.x, " ", ""))))
}

# Takes the subset variable map and processes it to gather actual data recoding
vm_combine_sheets <- function(vm, year, root_vm=vm_nvf) {

  # group the starting subset vm into sections
  vm_split <- split(vm_sub, vm_sub[["section_or_module"]]) |>
    purrr::map(~ setNames(.[[as.character(year)]], .$var_name))

  # go through each section containing vars to be processed and return a combined variable map
  vm_combined <- purrr::lmap(vm_split, ~ get_var_maps(root_vm, .x)) |>
    purrr::list_rbind()

  return(vm_combined)
}

vm_split_to_lists <- function(vm_split, old_col="old_val", new_col="new_val") {
  vm_processed_lists <- split(vm_split, vm_split[["new_var"]]) |>
    purrr::map(~ setNames(.[[new_col]], .[[old_col]]))
}


harmonise_preprocess_df <- function(dataset, vm_slice) {

  # check if any preprocessing is required
  old_var <- vm_slice$old_var[1]
  new_var <- vm_slice$new_var[1]

  old_var_processed <- dplyr::case_when(stringr::str_detect(old_var, " and ") ~ stringr::str_split_1(old_var, " and | or "),
                                        stringr::str_detect(old_var, " or ") ~ stringr::str_split_1(old_var, " and | or "),
                                        TRUE ~ old_var)
  # length(old_var_processed)

  if (length(old_var_processed) == 1) {
    dataset <- dataset |>
      dplyr::mutate("{new_var}" := dataset[[old_var]], .after = all_of(old_var_processed))
  } else if (length(old_var_processed) > 1) {
    dataset <- dataset |>
      dplyr::mutate(
        "{new_var}" := do.call(
          paste,
          c(dplyr::across(all_of(old_var_processed)), sep = ",")
        ),
        .after = old_var_processed[length(old_var_processed)]
      )
  }
  return(dataset)

}

harmonise_replace_values <- function(dataset, vm_processed_list_slice, var) {


  # reconstruct the list - necessary as the 'metadata' doesn't get passed when calling purrr::reduce()
  vm_processed_list_slice <- list(vm_processed_list_slice)
  names(vm_processed_list_slice) <- var

  dataset[[var]] <- factor(
    vm_processed_list_slice[[var]][as.character(dataset[[var]])] # look at the thing in the column of interest, find the thing with that name in lookup list, and return the value of the thing with that name
  )

}

df_processed <- process_harmonise_df(df_test, vm_sub)





#works (but need to copy the variable)
df_test <- df_test |>
  (\(d) {
    d[[var]] <- factor(
      vm_processed[[var]][as.character(d[[var]])], # look at the thing in the column of interest, find the thing with that name in vm_processed, returning the value
      levels = unique(vm_processed[[var]]) # don't strictly need levels
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


df <- data.frame() |>
  scjs_harmonise_variable(scjs_data, var_list = c("disability"), names_from = "pipeline")
