scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)


df_nvf <- purrr::map(scjs_data, ~ dplyr::select(.x, WGTGINDIV)) |> purrr::list_rbind()


var <- "wgtgindiv"


test <- function(df_list, var, names_from="original", variable_map="vm_nvf_overview") {


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



test_full <- test(nvf_2018, c("weight_indiv", "weight_household"), names_from="pipeline")
test_full <- test(scjs_data, c("weight_indiv", "weight_household"), names_from="pipeline")


# walks through the list containing data frames, finds highest value of variable 'year'
# assumes that list is probably ordered, so infers that highest value is at the start or end to avoid going through whole list
find_latest_year <- function(data_list) {

  if(!is.data.frame(data_list)) {
    if(!("year" %in% names(data_list[[1]]))) {
      stop("variable 'year' is not in data")
    } else {
      latest_year <- data_list[[length(data_list)]]$year[1]
      for(df in data_list) {
        if(df$year[1] < latest_year) {
          break
        } else if (df$year[1] > latest_year) {
          latest_year <- df$year[1]
        }
      }
    }
  } else if(is.data.frame(data_list)) {
    if(!("year" %in% names(data_list))) {
      stop("variable 'year' is not in data")
    } else {
      latest_year <- data_list$year[1]
  }
  }
  return(latest_year)

}

find_latest_year(scjs_data)
find_latest_year(nvf_2018)
"year" %in% names(nvf_2019)

subset_variable_map <- function(df, var) {
  data <- get(df) |>
    dplyr::filter(var_name %in% var) |>
    dplyr::select(var_name, starts_with("2"))
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

