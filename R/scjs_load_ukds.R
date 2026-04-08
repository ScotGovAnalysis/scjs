#' Import data downloaded from the UKDS
#'
#' @param path A file path to where files downloaded from the UKDS are.
#' @param dataset_type A short string specifying what type of SCJS dataset to load. Must be one of c("nvf").
#' @param years_to_load An option to specify what years to try and load in. Must be numeric, can be a single year, vector or range.
#' @param columns Columns to specify to load in from the call to haven::read_sav()
#' @param lowercase_cols TRUE/FALSE whether columns loaded in are forced to lower case.
#' @param name_prefix If you want to specify a name convention for the loaded data, defaults to dataset type plus year e.g. 'nvf_2024'.
#'
#' @export
scjs_load_ukds <- function(path, dataset_type="nvf", years_to_load=NULL, columns=NULL, lowercase_cols=TRUE, name_prefix=NULL) {
  # Catch invalid years
  if(!is.null(years_to_load)) {
    if(!is.numeric(years_to_load)) stop("Argument 'years_to_load' must be a numeric value or vector")
    check_years(years_to_load) # Check it was SCJS year and in valid range
  }

  # Reduce lookup just to relevant years
  if(!is.null(years_to_load)) {
    ukds_lookup <- ukds_lookup |>
      dplyr::filter(year %in% years_to_load)
  }

  # Catch invalid dataset type
  valid_dataset_types <- c("nvf") # expand to include other types in future
  if(!(tolower(dataset_type) %in% valid_dataset_types)) stop(paste("Invalid dataset type, must be one of:", valid_dataset_types))

  # Get full file paths of data files
  target_folders <- dplyr::pull(ukds_lookup, var = "nvf_folder")
  full_file_paths <- purrr::imap_chr(target_folders, ~fetch_full_path(path=path, lookup=ukds_lookup, index=.y))

  # Read spss .sav files found above
  scjs_data <- purrr::map2(full_file_paths, dplyr::pull(ukds_lookup, var = "year"), ~read_sav_data(filepath=.x, columns=columns, dataset_type=dataset_type, year=.y))

  # Add a variable for year to the loaded datasets
  scjs_data <- purrr::map2(scjs_data, dplyr::pull(ukds_lookup, var = "year"), ~ add_year_var(.x, .y))

  # Convert column names to lowercase if specified (TRUE by default)
  if(lowercase_cols) {
    scjs_data <- purrr::map(scjs_data, ~ dplyr::rename_with(.x, tolower))
  }

  # Name the output datasets and filter for only sucessful results
  if(!is.null(name_prefix)) {
    scjs_data <- purrr::set_names(scjs_data, paste(name_prefix, dplyr::pull(ukds_lookup, var = "year"), sep="_"))
  } else {
    scjs_data <- purrr::set_names(scjs_data, paste(dataset_type, dplyr::pull(ukds_lookup, var = "year"), sep="_"))
  }
  scjs_data <- scjs_data[!is.na(scjs_data)]

  message(paste("Successfully loaded", length(scjs_data), dplyr::if_else(length(scjs_data) > 1, "datasets", "dataset"), "into R inside a list."))
  message("You can unpack the list into individual datasets with 'list2env(scjs_data, envir=.GlobalEnv)'.")

  return(scjs_data)
}

check_years <- function(years) {
  valid_years <- dplyr::pull(ukds_lookup, var="year") # for years of length 1 (has to be year an SCJS took place)
  valid_range <- 2008:ukds_lookup[[nrow(ukds_lookup),1]] # year column from lookup

  if(length(years) == 1 && !(years %in% valid_years)) {
    stop("Specified year is not a valid SCJS year.")
  }

  if(!all(years %in% valid_range)) {
    stop("Years specified fall outside the range of valid years.")
  }
}

fetch_full_path <- function(path, lookup, index) {
  ukds_folder_path <- paste0(path, lookup[[index,3]])
  target_file_name <- lookup[[index,4]]

  if(!is.na(target_file_name)) {
    fetch_file_path <- list.files(ukds_folder_path, pattern = target_file_name, recursive = TRUE)

    if(length(fetch_file_path) != 0) {
      path_to_data <- paste0(
        ukds_folder_path,
        "/",
        list.files(ukds_folder_path, pattern = target_file_name, recursive = TRUE)
      )
    } else {
      path_to_data <- NA_character_
    }

  } else {
    path_to_data <- NA_character_
  }

  return(path_to_data)
}

read_sav_data <- function(filepath, columns, dataset_type, year) {
  if(!is.na(filepath)) {
    if(!is.null(columns)) {
      data <- haven::read_sav(filepath, col_select = dplyr::all_of(columns))
    } else {
      data <- haven::read_sav(filepath)
    }
    print(paste("Successfully loaded data for", year))
  } else {
    print(paste("Was not able to load data for", year))
    data <- NA
  }
  return(data)
}

add_year_var <- function(df, year) {
  if("year" %in% names(df)) {
    return(df)
  } else {
    df <- dplyr::mutate(df, year = year)
  }
}
