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
