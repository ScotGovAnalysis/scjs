scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)


df_nvf <- purrr::map(scjs_data, ~ dplyr::select(.x, WGTGINDIV)) |> purrr::list_rbind()


var <- "wgtgindiv"


test <- function(data, vars, names_from="original", variable_map="vm_nvf_overview") {

  # vm_sub <- vm_nvf_overview |> dplyr::filter(var_name == vars[1]) |>
  #   dplyr::select(var_name, starts_with("2"))

  # find our what the most recent year in the data list is
  latest_year <- find_latest_year(data)
  print(paste("latest_year:", latest_year))

  # check names_from validity
  valid_names_from <- c("pipeline", "original")
  if(!names_from %in% valid_names_from) {
    stop(paste("argument 'names_from' must be one of:", valid_names_from))
  }

  # when names_from is 'pipeline'
  if(names_from == "pipeline") {
    vm_sub <- get(variable_map) |>
      dplyr::filter(var_name == vars)
  }

  purrr::imap(vars, ~ print(paste("var", .y, .x)))

}

test_full <- test(scjs_data, c("weight_indiv", "weight_household"))


# walks through the list containing data frames, finds highest value of variable 'year'
# assumes that list is probably ordered, so infers that highest value is at the start or end to avoid going through whole list
find_latest_year <- function(data_list) {

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

  return(latest_year)

}
