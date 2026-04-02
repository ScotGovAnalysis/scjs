#' Harmonise variables from SCJS datasets
#'
#' @description
#' Harmonises variables from individual SCJS datasets into a target single multi-year dataset.
#'
#'
#' @param original_data A list containing the original SCJS datasets to harmonise from
#'
#' @param vars A vector of variables to be harmonised and placed into the target data frame
#'
#' @export
scjs_harmonise_variable <- function(original_data, vars) {
  # Check that original data is a list
  if(!is.list(original_data)) stop("The original data supplied must be a list.")

  # Check that vars is a character
  if(!is.character(vars)) stop("The list of variables to harmonise must be a character or vector of characters.")
}

# scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)
