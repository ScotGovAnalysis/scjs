#' Harmonise variables from SCJS datasets
#'
#' @description
#' Harmonises variables from individual SCJS datasets into a target single multi-year dataset.
#'
#' @param data A dataframe to add the harmonised variables to, can be set to "create" to start from scratch
#' @param df_list A list containing the original SCJS datasets to harmonise from
#' @param var_list A vector of variables to be harmonised and placed into the target data frame
#' @param names_from Indicates the method to look for variables in the variable map
#' @param variable_map Name of the relevant variable map for the data set type
#' @param find_all_vars TRUE/FALSE indicating whether to keep instances where multiple matches are found when using original data names
#' @param harmonise_as Indicate whether to keep any harmonised question as data values or as labelled values
#' @param join_method "join" or "append" - Whether to join datasets together: for adding more variables; or append the data: for adding a new year of data
#' @param drop_original_cols TRUE/FALSE indicating whether to drop original columns after harmonisation
#'
#' @export
scjs_harmonise_variable <- function(
    data="create",
    df_list,
    var_list,
    names_from="original",
    variable_map=vm_nvf$overview,
    find_all_vars=FALSE,
    harmonise_as="labels",
    join_method = "join",
    drop_original_cols=FALSE
) {

  if (missing(data)) {
    stop("`data` must be supplied, either directly or via a pipe.", call. = FALSE)
  }

  if(identical(data, "create")) {
    data <- data.frame()
  }

  if(!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!(join_method %in% c("join", "append"))) {
    stop("Argument 'join_method' must be one of 'join' or 'append'.")
  }

  # Check that original data is in the right format
  if(is.data.frame(df_list)) {
    df_list <- list(df_list) # when a single dataframe is passed
  } else if (is.list(df_list) && all(vapply(df_list, is.data.frame, logical(1)))) {
    # is list of data frames
  } else {
    stop("The original data supplied must be a list of data frames or a single data frame.")
  }

  # Check that var_list is a character
  if(!is.character(var_list)) stop("The list of variables to harmonise must be a character or vector of characters.")

  # check names_from validity
  valid_names_from <- c("pipeline", "original")
  if(!names_from %in% valid_names_from) {
    stop(paste("argument 'names_from' must be one of:", valid_names_from))
  }

  # check find_all_vars validity
  if(!is.logical(find_all_vars)) {
    stop("Argument keep all vars must be TRUE or FALSE.")
  }

  # find our what the years are in the data list
  years_vec <- extract_year_var(df_list)
  # print(years_vec)
  latest_year <- max(years_vec)
  print(paste("latest_year:", latest_year))

  # subset the variable map to the relevant parts
  vm_sub <- subset_variable_map(variable_map, var_list, years_vec, names_from, find_all_vars)
  # unique_var_list <- dplyr::pull(vm_sub, var="var_name")

  # pre-harmonisation check - make sure all necessary variables are in data
  # first get the variable map in list format with just the original variables for each year
  vm_sub_pre <- vm_sub |>
    dplyr::select(dplyr::starts_with("2"), var_name) |>
    tidyr::separate_wider_delim(cols = dplyr::starts_with("2"), delim = stringr::regex("( and )|( or )"), names_sep = c("sep"), too_few = "align_start") |>
    tidyr::pivot_longer(names_to = "year", values_to = c("original_var"), cols = dplyr::starts_with("2")) |>
    dplyr::filter(!is.na(original_var)) |>
    dplyr::mutate(year = substr(year, 1, 4))
  vm_sub_list <- split(vm_sub_pre, vm_sub_pre$year)
  missing_variables <- purrr::map(df_list, ~ pre_harmonisation_check(.x, vm_sub_list)) |>
    purrr::list_rbind()
  if (nrow(missing_variables) > 0) {
    missing_variables <- missing_variables |> dplyr::relocate(year, .before = 0)
    stop(paste(c("Some original variables missing from requested harmonisation - unable to proceed. The missing variables are:",
                 capture.output(print(missing_variables))), collapse = "\n"))
  }
  # print(missing_variables)

  # perform the transformations for harmonisation
  df_harmonised <- df_list |>
    purrr::map(
      ~ process_harmonise_df(.x, vm_sub, harmonise_as = harmonise_as)
    ) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE, ignore.attr = TRUE)

  # join the new harmonised dataset to the original data set

  if (all(dim(data)) == 0) {
    data <- df_harmonised
  } else if (join_method == "join") {
    data <- do_dt_join(data, df_harmonised)
  } else if (join_method == "append") {

    # check for overlap in years - raise error if overlap
    starting_data_years <- data |>
      dplyr::pull(var = "year") |>
      unique()
    years_intersect <- intersect(starting_data_years, years_vec)
    if(length(years_intersect > 0)) {
      stop(
        paste(
          "Overlap in years detected when trying to append data. Append should only be used when adding a new year to an existing dataset.",
          "Overlapping years:",
          years_intersect
        )
      )
    }
    data <- data.table::rbindlist(list(data, df_harmonised), use.names = TRUE, fill = TRUE, ignore.attr = TRUE)
  }

  # drop original variables
  if (drop_original_cols == TRUE) {
    # find which columns to drop
    if (names_from == "original") {
      cols_to_drop <- var_list
    } else if (names_from == "pipeline") {
      cols_to_drop <- get_all_original_vars(vm_sub)
    }
    data <- data |> dplyr::select(-dplyr::all_of(unlist(cols_to_drop)))
  }

  return(data)

  # print(vm_sub)
  # print(unique_var_list)

}



extract_year_var <- function(df) {
  if(is.data.frame(df)) {
    if(!("year" %in% names(df))) {
      stop(paste("variable 'year' is not in data frame:", df))
    } else {
      year <- df$year[1]
      if(!is.numeric(year)) {
        year <- convert_financial_year(year)
      }
    }
  } else {
    year <- purrr::map_vec(df, ~extract_year_var(.x))
  }
  return(year)
}

# function to coerce financial year to numeric
convert_financial_year <- function(year) {
  # regex for financial year
  finyr_regex <- stringr::str_detect(year, "\\b\\d{2,4}/?\\d{2}\\b")
  if(finyr_regex){
    year_as_num <- stringr::str_extract(year, "\\b\\d{2,}")
    if(stringr::str_length(year_as_num) == 2) {
      year_as_num <- 2000 + as.numeric(year_as_num)
    } else if (stringr::str_length(year_as_num) == 4) {
      year_as_num <- as.numeric(year_as_num)
    } else {
      stop("Length of extracted year is not of length 2 or 4.")
    }
  } else {
    stop("Could not convert item in year to numeric.")
  }
  return(year_as_num)
}


# reduces the overview sheet of a variable map to just the rows being harmonised and just the years passed in as data
subset_variable_map <- function(variable_map, var_list, years_vec, names_from, find_all_vars) {

  if (!is.data.frame(variable_map)) {
    stop("Supplied variable map must be a data frame.")
  }

  if ((!is.vector(var_list) || length(var_list) == 0) || all(var_list == "")) {
    stop("Invalid var_list supplied, must be a vector or string containing variable names.")
  }

  if(!is.numeric(years_vec)) {
    stop("The extracted years is not a numeric vector.")
  }

  latest_year <- max(years_vec)

  if(names_from == "original") {
    vm_init <- variable_map |>
      dplyr::filter(.data[[as.character(latest_year)]] %in% var_list)

    vm_check_list <- vm_init |>
      dplyr::select(dplyr::all_of(as.character(latest_year))) |>
      dplyr::pull()

    # check for multiple matches
    vm_grp <- vm_init |>
      dplyr::group_by(.data[[as.character(latest_year)]]) |>
      dplyr::mutate(count = dplyr::n())

    vm_grp_multi <- vm_grp |>
      dplyr::filter(count > 1)

    vm_vars_multi <- vm_grp_multi |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(as.character(latest_year))) |>
      dplyr::pull()

    if(find_all_vars == FALSE) {
      vm_grp <- vm_grp |>
        dplyr::filter(dplyr::row_number() == 1)

      vm_dup_vars <- vm_grp |> dplyr::select(dplyr::all_of(as.character(latest_year))) |> dplyr::pull()
      vm_dup_pipeline_vars <-  vm_grp |> dplyr::select(var_name) |> dplyr::pull()
      warning(paste(
        "Found multiple results for some original variables, scjs_harmonise_variable() will only take the first result by default.",
        "To keep all matches to the original variable name, use find_all_vars=TRUE in scjs_harmonise_variable().",
        "\n",
        "The duplicated variables are:",
        paste(vm_dup_vars, collapse = ", "),
        "\n",
        "The unique 'pipeline' variables kept are:",
        paste(vm_dup_pipeline_vars, collapse=", ")
      ))

      vm_sub_final <- vm_grp
    } else if (find_all_vars == TRUE & max(vm_grp$count) > 1) {
      vm_pairs_multi <- vm_grp_multi |>
        dplyr::ungroup() |>
        dplyr::select(var_name, dplyr::all_of(as.character(latest_year)))

      message(paste(
        "Found multiple results for some original variables, and kept all instances.",
        "\n",
        "The duplicated variables and corresponding unique 'pipeline' name are:",
        "\n"),
        paste(vm_pairs_multi[1],"\n", vm_pairs_multi[2], sep="")
      )
      vm_sub_final <- vm_grp
    }

    # print(vm_grp)
  }

  if(names_from == "pipeline") {
    vm_init <- variable_map |>
      dplyr::filter(var_name %in% var_list)
    vm_check_list <- vm_init |>
      dplyr::select(var_name) |>
      dplyr::pull()
    vm_sub_final <- vm_init
    # print(vm_sub_final)
  }

  if(nrow(vm_sub_final) == 0) {
    stop(paste("Unable to find any variables with the name(s) supplied in the specified list of data sets."))
  }

  # Check result list against supplied var_list
  vars_not_found <- setdiff(var_list, vm_check_list)
  if(length(vars_not_found) != 0) {
    warning(paste("Unable to match all variables requested for harmonisation.", "\n", "Could not find:", paste(vars_not_found, collapse=", ")))
  }

  vm_sub_final <- vm_sub_final |>
    dplyr::select(section_or_module, var_name, var_type, dplyr::all_of(as.character(years_vec)), requires_recoding)

  return(vm_sub_final)

  # input list of dataframes, input subset of variable map
}

pre_harmonisation_check <- function(dataset, vm_sub_list) {
  cur_year <- dplyr::pull(dataset, var = year)[1]
  cur_list <- vm_sub_list[[as.character(cur_year)]]
  missing_vars <- setdiff(cur_list$original_var, names(dataset))
  if(length(missing_vars) == 0) {
    return(NULL)
  }
  data_missing <- cur_list |>
    dplyr::filter(original_var %in% missing_vars)
  return(data_missing)

}

# Functions to process the variable map / dataset and perform the harmonisation based on the input variable map
process_harmonise_df <- function(dataset, variable_map, harmonise_as="labels") {

  # grab the year of the dataset
  year <- dplyr::pull(dataset, var = year)[1]

  # process the variable maps
  vm_combined <- vm_combine_sheets(variable_map, year) # combine relevant sections from individual sheets

  # need to handle cases where no recoding is required but still need them to go into a single united column in harmonised df
  variable_map_no_recoding <- variable_map |>
    dplyr::filter(requires_recoding == "no") |>
    dplyr::select(var_name, dplyr::starts_with("2"))
  variable_map_no_recoding <- variable_map_no_recoding |>
    tidyr::pivot_longer(names_to = "year", values_to = "old_var", cols = -var_name) |>
    dplyr::rename(new_var = var_name) |>
    dplyr::select(-year)

  vm_combined <- vm_combined |>
    dplyr::bind_rows(variable_map_no_recoding)

  vm_split <- split(vm_combined, vm_combined[["new_var"]]) # split the combined vm into a list of dfs



  # create the list structure for the combined variable map
  if (!(harmonise_as %in% c("data", "labels", "none"))) {
    stop("Argument harmonise_as must be equal to one of: 'data' or 'labels'.")
  }

  # control inputs to vm_split_to_lists() based on value in 'harmonise_as'
  if (harmonise_as == "labels") {
    vm_processed_list <- vm_split_to_lists(vm_combined, old_col = "old_val", new_col = "new_lab")
  } else if (harmonise_as == "data") {
    vm_processed_list <- vm_split_to_lists(vm_combined, old_col = "old_val", new_col = "new_val")
  } else if (harmonise_as == "none") {
    return(dataset)
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
    dplyr::mutate(dplyr::across(c(old_val, new_val), ~ as.character(stringr::str_replace(.x, " ", ""))))
}

get_all_original_vars <- function(vm) {
  unique_original_vars <- vm |>
    dplyr::select(dplyr::all_of(dplyr::starts_with("2"))) |>
    unlist() |>
    unique()

  split_combo_vars <- function(var) {
    var_processed <- dplyr::case_when(stringr::str_detect(var, " and ") ~ stringr::str_split_1(var, " and | or "),
                                                       stringr::str_detect(var, " or ") ~ stringr::str_split_1(var, " and | or "),
                                                       TRUE ~ var)
  }

  unique_original_vars_processed <- purrr::map(unique_original_vars, ~ split_combo_vars(.x))

  return(unique_original_vars_processed)
}

# Takes the subset variable map and processes it to gather actual data recoding
vm_combine_sheets <- function(vm, year, root_vm=vm_nvf) {

  # group the starting subset vm into sections
  vm_split <- split(vm, vm[["section_or_module"]]) |>
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
      dplyr::mutate("{new_var}" := dataset[[old_var]], .after = dplyr::all_of(old_var_processed))
  } else if (length(old_var_processed) > 1) {
    dataset <- dataset |>
      dplyr::mutate(
        "{new_var}" := do.call(
          paste,
          c(dplyr::across(dplyr::all_of(old_var_processed)), sep = ",")
        ),
        .after = old_var_processed[length(old_var_processed)]
      )
  }
  return(dataset)

}

harmonise_replace_values <- function(dataset, vm_processed_list_slice, var) {

  # exit early if no recoding is being done (given by the vm_processed_list_slice being NA)
  if(all(is.na(vm_processed_list_slice))) {
    return(dataset)
  }

  # reconstruct the list - necessary as the 'metadata' doesn't get passed when calling purrr::reduce()
  vm_processed_list_slice <- list(vm_processed_list_slice)
  names(vm_processed_list_slice) <- var

  dataset[[var]] <- factor(
    vm_processed_list_slice[[var]][as.character(dataset[[var]])] # look at the thing in the column of interest, find the thing with that name in lookup list, and return the value of the thing with that name
  )

  return(dataset)
}


do_dt_join <- function(lhs, rhs, join_keys = c("year", "serial2")) {
  lhs <- data.table::as.data.table(lhs)
  rhs <- data.table::as.data.table(rhs)

  different_cols <- setdiff(names(rhs), names(lhs))
  diff_col_locs <- purrr::map(different_cols, ~ which(names(rhs) == .x))
  names(diff_col_locs) <- different_cols
  different_cols_rev <- rev(diff_col_locs)

  cols_to_keep <- c(join_keys, different_cols)
  rhs_join <- rhs[, cols_to_keep, with = FALSE]

  df_join <- merge(
    lhs,
    rhs_join,
    by = join_keys,
    all.x = TRUE,
    sort = FALSE
  )

  # repair column order (need to reverse list to avoid altering index during runtime)
  dataset_reordered <- purrr::reduce2(different_cols_rev, names(different_cols_rev), repair_column_order, .init = df_join)

  return(dataset_reordered)
}

repair_column_order <- function(dataset, relocate_list, relocate_names) {
  dataset <- dataset |>
    dplyr::relocate(relocate_names, .before = relocate_list)
  return(dataset)
}
