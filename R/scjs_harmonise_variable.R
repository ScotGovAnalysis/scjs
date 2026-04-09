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

  # find our what the years in the data list
  years_vec <- extract_year_var(df_list)
  print(years_vec)
  latest_year <- max(years_vec)
  print(paste("latest_year:", latest_year))

  # check names_from validity
  valid_names_from <- c("pipeline", "original")
  if(!names_from %in% valid_names_from) {
    stop(paste("argument 'names_from' must be one of:", valid_names_from))
  }

  # when names_from is 'pipeline'
  if(names_from == "pipeline") {
    vm_sub <- subset_variable_map(variable_map, var)
    if(nrow(vm_sub) == 0) {
      stop(paste("Unable to find match in variable map for", var))
    }
  }

  if(names_from == "original") {
    vm_sub <- subset_variable_map(variable_map, var)

    # if(nrow(vm_sub) > 1) {
    #   vm_sub <- vm_sub |>
    #     dplyr::slice(1)
    #   first_result <- pull(vm_sub, var = "var_name")
    #   warning(paste("Found more than one variable with match to", var, ". Using result from", first_result))
    # } else if(nrow(vm_sub) == 0) {
    #   stop(paste("Unable to find match in variable map for", var))
    # }
  }

  print(vm_sub)
  purrr::imap(var, ~ print(paste("var", .y, .x)))
}



extract_year_var <- function(df) {
  if(is.data.frame(df)) {
    if(!("year" %in% names(df))) {
      stop(paste("variable 'year' is not in data frame:", df))
    } else {
      year <- df$year[1]
    }
  } else {
    year <- purrr::map_vec(df, ~extract_year_var(.x))
  }
}
