#' Easily find all columns to load from SCJS data
#'
#' @description
#' Creates a list containing the original column names for all sections or harmonised variables you want to load from the raw data.
#'
#' @param dataset_type A short string specifying what type of SCJS dataset to look for columns in. Must be one of c("nvf").
#' @param variable_list A single variable or vector of variables of 'pipeline' names from the 'var_name' column in the variable map.
#' @param sections A single string or vector of a section in the 'section_or_module' column in the variable map. Used to import entire sections.
#' @param years_to_load A single year or range of years to look for the original variables in.
#'
#' @export
column_select_helper <- function(dataset_type, variable_list=NULL, sections=NULL, years_to_load=NULL) {

  # check that dataset_type is valid
  valid_dataset_types <- c("nvf") # expand to include other types in future
  if(!(tolower(dataset_type) %in% valid_dataset_types)) stop(paste("Invalid dataset type, must be one of:", valid_dataset_types))

  # check that years are valid
  if(!is.null(years_to_load)) {
    if(!is.numeric(years_to_load)) stop("Argument 'years_to_load' must be a numeric value or vector")
    check_years(years_to_load) # Check it was SCJS year and in valid range
  }

  # set up the variable map with the corresponding dataset type
  if (dataset_type == "nvf") {
    variable_map <- vm_nvf$overview
  }

  # restrict the variable map just to the specified years
  year_columns <- dplyr::pull(ukds_lookup, var="year")
  if (!is.null(years_to_load)) {
    # find the overlap between specified year range and all valid possible SCJS years
    year_columns <- intersect(years_to_load, dplyr::pull(ukds_lookup, var="year"))
  }

  variable_map_subset <- variable_map |>
    dplyr::select(section_or_module, var_name, dplyr::any_of(as.character(year_columns)))

  # filter the subset VM to just the specified variables

  # handle sections first
  if (!is.null(sections)) {
    # logic to check if provided sections exist
    existing_sections <- unique(dplyr::pull(variable_map, var = "section_or_module"))
    mismatched_sections <- setdiff(sections, existing_sections)
    if (length(mismatched_sections) > 0) {
      warning(paste(
        "Not all supplied sections were found in the relevant variable map, no match was found for the following sections:",
        paste0(mismatched_sections, collapse = ", ")
      ))
    }

  }

  # handle variable_list
  if (!is.null(variable_list)) {
    # logic to check if provided sections exist
    existing_vars <- unique(dplyr::pull(variable_map, var = "var_name"))
    mismatched_vars <- setdiff(variable_list, existing_vars)
    if (length(mismatched_vars) > 0) {
      warning(paste(
        "Not all supplied variables were found in the relevant variable map, no match was found for the following variables:",
        paste0(mismatched_vars, collapse = ", ")
      ))
    }

  }
  variable_map_filter <- variable_map_subset |>
    dplyr::filter(section_or_module %in% sections | var_name %in% variable_list) # do this filter later at the same time as the var list?

  variable_map_original <- variable_map_filter |>
    dplyr::select(dplyr::starts_with("2"))

  variable_unique_list <- unique(unlist(variable_map_original))

  return(variable_unique_list)
}
