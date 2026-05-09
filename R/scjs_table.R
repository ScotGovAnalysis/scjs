#' Create a table from SCJS data
#'
#' @description
#' Takes an individual or harmonised set of SCJS data and builds a table using standard methods.
#'
#' @param dataset Input dataset to construct the table from
#' @param table_type Specify what format the output table should take (time series / subgroup comparison etc.)
#' @param var Single variable or vector specifying multiple variables to be presented in the resulting table
#' @param crossbreak Single variable or vector specifying multiple variables to act as cross breaks - usually a demographic variable, can be "total" for all respondents. NOTE: for intersectional split, crossbreak MUST be a list() with the desired intersection combination grouped by c() or list()
#' @param time_period Used to restrict the years analysed in the table
#' @param time_grouping Used to specify how years can be grouped together for pooled analysis - default is just survey year. Argument needs to be name of existing column in dataset.
#' @param result_type Used to specify whether data in the table is presented as a proportion or a volume.
#' @param weighting_var Weight value to use in calculations - default is the harmonised individual weight from the non-victim form. Needs to be existing column in dataset.
#'
#' @export
#'
scjs_table <- function(
    dataset,
    table_type,
    var,
    crossbreak=NULL,
    time_period=NULL,
    time_grouping="year",
    result_type="proportion",
    weighting_var="weight_indiv"
) {

  # pre checks
  validate_table_inputs(dataset, table_type, var, crossbreak, time_grouping, weighting_var)

  # data pre processing
  df_reduce <- reduce_input_dataset(dataset, var, crossbreak, time_period, time_grouping, weighting_var)

  # do aggregations?

  # add design factor
  joining_cols <- intersect(names(scjs_design_factor), names(df_reduce))
  df_prep <- df_reduce |>
    dplyr::left_join(scjs_design_factor, by = joining_cols) |>
    dplyr::relocate(dplyr::all_of(c("survey_year", "design_factor")), .after = .data[[time_grouping]])

  # generate all combinations of var + crossbreak
  var_crossbreak_combinations <- tidyr::crossing(var, crossbreak)

  if (is.null(crossbreak)) {
    var_crossbreak_combinations[["crossbreak"]] <- "total"
  }


  table_base <- purrr::map2(
    var_crossbreak_combinations$var,
    var_crossbreak_combinations$crossbreak,
    ~ base_summary_table(
      dataset = df_prep,
      var = .x,
      crossbreak = .y,
      time_grouping = time_grouping,
      result_type = result_type,
      weighting_var = weighting_var)
  ) |>
    purrr::list_rbind()



  return(table_base)
}

# Helper functions ####
validate_table_inputs <- function(
    dataset,
    table_type,
    var,
    crossbreak,
    time_grouping,
    weighting_var
) {

  # 'total' from crossbreak
  crossbreak <- crossbreak[which(crossbreak != "total")]

  # check that a data.frame object is supplied as the dataset argument
  if (!is.data.frame(dataset)) {
    stop("Argument 'dataset' must be supplied as a data.frame object.")
  }

  valid_table_types <- c("time series", "subgroup")
  if (!table_type %in% valid_table_types) {
    stop(paste("Invalid table_type argument, must be one of:", paste(valid_table_types, collapse = ", ")))
  }

  # check var argument is of correct type
  if ((!is.vector(var) || length(var) == 0) || all(var == "")) {
    stop("Invalid var supplied, must be a string of variable names or a vector containing multiple names.")
  }

  # check crossbreak argument is of correct type
  if (!is.null(crossbreak) && (!is.vector(crossbreak) || length(crossbreak) == 0 || all(crossbreak == ""))) {
    stop("Invalid crossbreak supplied, must be a string of variable names or a vector containing multiple names.")
  }

  # check that there is a weighting variable in the dataset
  if (!weighting_var %in% names(dataset)) {
    stop("Unable to find weighting var in dataset. Default is harmonised version 'weight_indiv', specify if other weighting variable desired.")
  }

  # check variables requested are in the dataset
  if (!all(c(var, crossbreak, time_grouping) %in% names(dataset))) {
    missing_vars <- setdiff(c(var, crossbreak, time_grouping), names(dataset))
    stop(paste("Source dataset has some missing variables - unable to proceed. Missing vars are:", paste(missing_vars, collapse = ", ")))
  }
}

reduce_input_dataset <- function(dataset, var, crossbreak, time_period, time_grouping, weighting_var) {
  # reduce the number of columns
  df_reduce <- dataset |>
    dplyr::select(any_of(c(time_grouping, var, crossbreak, weighting_var)))

  if (!is.null(time_period)) {
    df_reduce <- df_reduce |>
      dplyr::filter(.data[[time_grouping]] %in% time_period)
  }

  return(df_reduce)

}

# Table functions ####
base_summary_table <- function(dataset, var, crossbreak, time_grouping, result_type, weighting_var) {

  dataset_strip <- dataset |>
    dplyr::filter(!is.na(.data[[var]]))

  table_base <- dataset_strip |>
    dplyr::group_by(across(any_of(c(time_grouping, var, unlist(crossbreak))))) |>
    dplyr::summarise(sum_weight = sum(.data[[weighting_var]]),
                     base = dplyr::n(),
                     design_factor = max(design_factor)) |> # change max() to alter how design_factor is selected for combined years
    dplyr::ungroup()

  if (result_type == "proportion") {
    table_base <- table_base |>
      dplyr::group_by(across(dplyr::any_of(c(time_grouping, crossbreak)))) |>
      dplyr::mutate(base_total = sum(base), .after = "base",,
                    proportion = sum_weight / sum(sum_weight) * 100,
                    se = sqrt((proportion) * (100 - (proportion)) / base),
                    ci_95 = se * qnorm(0.975) * design_factor) |>
      dplyr::ungroup()
  }

  # reformat the table to have more generic names to allow for easier combining
  table_format <- table_base |>
    dplyr::mutate("{crossbreak}" :=  if (crossbreak == "total") {"all respondents"} else {as.character(.data[[crossbreak]])}) |>
    dplyr::mutate("{var}" := as.character(.data[[var]]))

  table_format <- table_format |>
    dplyr::mutate(variable_name = var,
                  crossbreak = crossbreak)

  table_format <- table_format |>
    dplyr::rename(response = .data[[var]],
                  subgroup = .data[[crossbreak]])

  table_format <- table_format |>
    dplyr::relocate(c(variable_name, crossbreak, subgroup), .after = time_grouping)

  table_format <- table_format |>
    dplyr::arrange(.data[[time_grouping]], variable_name, crossbreak, subgroup)

  return(table_format)

}



# Statistical significance functions ####
#' @importFrom janitor round_half_up
significance_nstep <- function(proportion, ci, lag=1, rounding_method="table", format="prop") {

  not_significant_symbol <- if (rounding_method == "table") "[ns]" else "."
  rounding <- if (rounding_method == "table") {
    if (sum(c(proportion, lag(proportion, n = lag) < 10), na.rm = TRUE) == 2) 1 else 0
  } else 2

  lag_prop <- dplyr::lag(proportion, n = lag)
  lag_ci <- dplyr::lag(ci, n = lag)

  diff <- proportion - lag_prop
  threshold <- sqrt(ci^2 + lag_ci^2)

  significant <- abs(diff) > threshold

  if (format == "prop") {
    change_value <- abs(diff)
    change_symbol <- "pp"
  } else {
    pct_change <- dplyr::if_else(
      lag_prop == 0 | is.na(lag_prop),
      NA_real_,
      (diff / lag_prop) * 100
    )
    change_symbol <- "%"
    change_value <- abs(pct_change)
  }

  dplyr::case_when(
    is.na(lag_prop) ~ NA_character_,
    significant & diff > 0 ~ paste0("up by ", janitor::round_half_up(change_value, rounding), change_symbol),
    significant & diff < 0 ~ paste0("down by ", janitor::round_half_up(change_value, rounding), change_symbol),
    TRUE ~ not_significant_symbol
  )
}

#' @importFrom janitor round_half_up
significance_fromstart <- function(proportion, ci, rounding_method = "table", format = "prop") {

  # --- formatting options ---
  not_significant_symbol <- if (rounding_method == "table") "[ns]" else "."

  rounding <- if (rounding_method == "table") {
    if (sum(c(proportion, lag(proportion, n = lag) < 10), na.rm = TRUE) == 2) 1 else 0
  } else 2

  # --- find first non-NA baseline ---
  first_idx <- which(!is.na(proportion))[1]

  # if no valid baseline or only 1 value, return all NA
  if (is.na(first_idx)) {
    return(rep(NA_character_, length(proportion)))
  }
  if (first_idx == length(proportion)) {
    return(c(rep(NA_character_, length(proportion) - 1), "[z]")) # return string with [z] for not applicable at end
  }
  # could handle cases where there are only 2 observations? as this comparison is covered by the onestep back function

  base_prop <- proportion[first_idx]
  base_ci   <- ci[first_idx]

  diff <- proportion - base_prop
  threshold <- sqrt(ci^2 + base_ci^2)

  significant <- abs(diff) > threshold

  if (format == "prop") {
    change_value <- abs(diff)
    change_symbol <- "pp"
  } else {
    pct_change <- dplyr::if_else(
      base_prop == 0 | is.na(base_prop),
      NA_real_,
      (diff / base_prop) * 100
    )
    change_value <- abs(pct_change)
    change_symbol <- "%"
  }

  dplyr::case_when(
    is.na(proportion) ~ NA_character_,
    seq_along(proportion) == first_idx ~ NA_character_,
    significant & diff > 0 ~ paste0("up by ", janitor::round_half_up(change_value, rounding), change_symbol),
    significant & diff < 0 ~ paste0("down by ", janitor::round_half_up(change_value, rounding), change_symbol),
    TRUE ~ not_significant_symbol
  )
}


significance_mostrecent <- function(proportion, ci, time_variable) {

  n <- length(proportion)
  result <- rep(NA, n)

  for (i in rev(seq_along(proportion))) {

    # skip first element
    if (i == 1) next

    # skip invalid current values
    if (is.na(proportion[i]) || is.na(ci[i]) || proportion[i] > 100) next

    # look backwards
    for (j in (i - 1):1) {

      # skip invalid comparison values
      if (is.na(proportion[j]) || is.na(ci[j])) next

      # significance test
      if (abs(proportion[i] - proportion[j]) > sqrt(ci[i]^2 + ci[j]^2)) {
        result[i] <- time_variable[j]
        break
      }
    }
  }

  return(result)
}
