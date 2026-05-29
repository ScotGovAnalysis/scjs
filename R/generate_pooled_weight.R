#' Create a table from SCJS data
#'
#' @description Adds a pooled individual or household weight to a combined SCJS
#' dataset. Implements the same methodology outlined in Appendix B of the
#' published Scottish Government paper [Ethnicity in the Justice
#' System](https://www.gov.scot/publications/ethnicity-justice-system/pages/8/).
#'
#' @param dataset Input dataset to add the pooled weights to.
#' @param weighting_var Variable to base the pooled weights on (nominally the
#'   name of the variable containing gross individual or household weights).
#' @param output_suffix The suffix of the name of the new pooled weight
#'   variable. This is '`_pooled`' by default.
#' @param keep_all_columns Boolean variable, `FALSE` by default, specifying
#'   whether to keep the columns used to calculate the pooled weights if `TRUE`, or if
#'   `FALSE` keeps just the resulting weight column.
#'
#' @returns Returns a data frame object of the same dimensions of the input
#'   dataset with an added column for the pooled weight, as well as additional
#'   columns used for the calculation if `keep_all_columns=TRUE`.
#'
#' @export
generate_pooled_weight <- function(dataset, weighting_var, output_suffix="_pooled", keep_all_columns=FALSE) {
  # check that year is in the dataset
  if (!("year" %in% names(dataset))) {
    stop("Variable 'year' not found in dataset which is necessary to generate pooled weights.")
  }

  # add design factor
  joining_cols <- intersect(names(scjs_design_factor), names(dataset))
  dataset <- dataset |>
    dplyr::left_join(scjs_design_factor, by = joining_cols) |>
    dplyr::relocate(dplyr::all_of(c("survey_year", "design_factor")), .after = year)

  sample_totals <- dataset |>
    dplyr::group_by(year) |>
    dplyr::summarise(sample_size = dplyr::n(),
                     population_total = sum(.data[[weighting_var]]),
                     design_factor = dplyr::first(design_factor)) |>
    dplyr::ungroup() |>
    dplyr::mutate(design_effect = design_factor ^ 2,
                  effective_sample = sample_size / design_effect,
                  total_effective_sample = sum(effective_sample),
                  pooled_effective_sample = effective_sample / total_effective_sample)

  new_columns <- setdiff(names(sample_totals), names(dataset))
  new_columns <- c("survey_year", "design_factor", new_columns)

  sample_joining_columns <- intersect(names(sample_totals), names(dataset))
  dataset_join <- dataset |>
    dplyr::left_join(sample_totals, by = sample_joining_columns)

  new_name <- paste0(weighting_var, output_suffix)

  dataset_join <- dataset_join |>
    dplyr::mutate("{new_name}" := .data[[weighting_var]] * pooled_effective_sample)

  if (!keep_all_columns) {
    dataset_join <- dataset_join |>
      dplyr::select(-dplyr::any_of(new_columns))
  }

  return(dataset_join)
}
