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
#' @param rounding_method Specifies how change in significance columns are presented, default value 'table' will round to 0dp when numbers are >10 and 1dp when below 10 - change value to round to 2dp.
#' @param ts_significance_steps_back For time series tables, specifies how many time periods back to produce a significance column for, e.g. by default gives a result compared to 1 and 2 years prior.
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
    rounding_method="table",
    ts_significance_steps_back=2,
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


  table_base_summary <- purrr::map2(
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

  # add the requisite significance columns for that table type
  table_base_sig <- add_significance_cols(
    dataset = table_base_summary,
    table_type = table_type,
    var = var,
    crossbreak = crossbreak,
    time_grouping = time_grouping,
    result_type = result_type,
    rounding_method = rounding_method,
    ts_significance_steps_back = ts_significance_steps_back
  )




  return(table_base_sig)
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

  # should add check that time grouping is either year or is a factor (so that ordering can be set correctly) - raise warning if so
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

## Table functions ####
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

add_significance_cols <- function(dataset, table_type, var, crossbreak, time_grouping, result_type=result_type, rounding_method="table",ts_significance_steps_back) {

  #TODO configure way to input custom significance requests - e.g. none or for time series only steps back

  # establish what the 'format' value should be for changes shown in significance cols
  if (result_type == "proportion") {
    format <- "prop"
  } else {
    format <- "volume"
  }

  if (result_type == "volume") {
    # current implementation does not allow volume significance tests, but if added it would be handled here
    return(dataset)
  }

  if (table_type == "time series") {

    steps_back <- 1:ts_significance_steps_back

    # group the dataset
    # grouping_cols <- c(variable_name, crossbreak, subgroup, response)
    dataset <- dataset |>
      dplyr::group_by(variable_name, crossbreak, subgroup, response)

    data_sig <- purrr::reduce(
      steps_back,
      ~ significance_nstep_wrapper(.x, .y, rounding_method = rounding_method, format = format),
      .init = dataset
    )

    data_sig <- data_sig |>
      dplyr::mutate(sig_fromstart = significance_fromstart(proportion, ci_95)) |>
      dplyr::mutate(sig_mostrecent = significance_mostrecent(proportion, ci_95, .data[["year"]])) |>
      dplyr::ungroup()

    return(data_sig)
  }

  if (table_type == "subgroup") {
    dataset <- dataset |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c(time_grouping))), variable_name, crossbreak, response)

    data_sig <- dataset |>
      dplyr::mutate(sig_allcombo = significance_allcombo(proportion, ci_95, context_var=subgroup)) |>
      dplyr::ungroup()

    return(data_sig)
  }

}


# Statistical significance functions ####
#' @importFrom janitor round_half_up
significance_nstep <- function(proportion, ci, lag=1, rounding_method="table", format="prop") {

  not_significant_symbol <- if (rounding_method == "table") "[ns]" else "."
  rounding <- if (rounding_method == "table") {
    if (sum(proportion > 10, na.rm = TRUE) >= 2) 1 else 0
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
    seq_along(proportion) <= lag ~ "[z]",
    is.na(lag_prop) ~ NA_character_,
    significant & diff > 0 ~ paste0("up by ", janitor::round_half_up(change_value, rounding), change_symbol),
    significant & diff < 0 ~ paste0("down by ", janitor::round_half_up(change_value, rounding), change_symbol),
    TRUE ~ not_significant_symbol
  )
}

significance_nstep_wrapper <- function(dataset, steps_back, rounding_method="table", format="prop") {
  col_name <- paste0("significance_", steps_back, "_step", if(steps_back > 1) "s")
  dataset <- dataset |>
    dplyr::mutate("{col_name}" := significance_nstep(proportion, ci_95, lag = steps_back, rounding_method=rounding_method, format=format))
}

#' @importFrom janitor round_half_up
significance_fromstart <- function(proportion, ci, rounding_method = "table", format = "prop") {

  # --- formatting options ---
  not_significant_symbol <- if (rounding_method == "table") "[ns]" else "."

  rounding <- if (rounding_method == "table") {
    if (sum(proportion > 10, na.rm = TRUE) >= 2) 1 else 0
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

significance_allcombo <- function(proportion, ci, context_var, matrix_result = FALSE) {

  # define expected output
  full_results <- rep(NA, length(proportion))

  # set up format of output based on specification
  if (matrix_result) {
    invalid_res <- "."
    positive_res <- rep("yes", length(proportion))
    negative_res <- "no"
  } else {
    invalid_res <- NA
    positive_res <- context_var
    negative_res <- NA
  }

  for (i in 1:length(proportion)) {
    indiv_result_list <- rep(NA, length(proportion))
    for (j in 1:length(proportion)) {
      if (i == j){
        # indiv_result_list <- append(indiv_result_list, invalid_res)
        indiv_result_list[j] <- invalid_res
      } else {
        if (abs(proportion[i] - proportion[j]) > sqrt(ci[i]^2 + ci[j]^2)) {
          # indiv_result_list <- append(indiv_result_list, positive_res[j])
          indiv_result_list[j] <- positive_res[j]
          # steps <- append(steps, outcome_var[j])
          # steps <- steps[!is.na(steps)]
        } else {
          indiv_result_list[j] <- negative_res
        }
      }
      if (matrix_result) {
        full_results[i] <- paste(indiv_result_list, collapse = ", ")
      } else {
        strip_results <- indiv_result_list[which(!(indiv_result_list %in% c(invalid_res, negative_res)))]
        full_results[i] <- paste(strip_results, collapse = ", ")
      }

    }
  }
  return(full_results)
}
