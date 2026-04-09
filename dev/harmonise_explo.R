scjs_data <- list(nvf_2023=df_test_2023, nvf_2024=df_test_2024)


df_nvf <- purrr::map(scjs_data, ~ dplyr::select(.x, WGTGINDIV)) |> purrr::list_rbind()


var <- "wgtgindiv"


test <- function(df_list, var_list, names_from="original", variable_map="vm_nvf_overview", keep_all_vars=FALSE) {


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

  # subset the variable map to the relevant parts
  vm_sub <- subset(variable_map, var_list, years_vec, names_from, keep_all_vars)
  unique_var_list <- dplyr::pull(vm_sub, var="var_name")

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

subset_variable_map <- function(variable_map, var_list, years_vec, names_from, keep_all_vars) {

  latest_year <- max(years_vec)

  if(names_from == "original") {
    vm_init <- get(variable_map) |>
      dplyr::filter(.data[[as.character(latest_year)]] %in% var_list)

    # check for multiple matches
    vm_grp <- vm_init |>
      dplyr::group_by(.data[[as.character(latest_year)]]) |>
      dplyr::mutate(count = dplyr::n())

    vm_grp_multi <- vm_grp |>
      dplyr::filter(count > 1)

    vm_vars_multi <- vm_grp_multi |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(as.character(latest_year))) |>
      dplyr::pull()

    if(keep_all_vars == FALSE) {
      vm_grp <- vm_grp |>
        dplyr::filter(dplyr::row_number() == 1)

      vm_dup_vars <- vm_grp |> dplyr::select(dplyr::all_of(as.character(latest_year))) |> dplyr::pull()
      vm_dup_pipeline_vars <-  vm_grp |> dplyr::select(var_name) |> dplyr::pull()
      warning(paste(
        "Found multiple results for some original variables, scjs_harmonise_variable() will only take the first result by default.",
        "To keep all matches to the original variable name, use keep_all_vars=TRUE in scjs_harmonise_variable().",
        "\n",
        "The duplicated variables are:",
        paste(vm_dup_vars, collapse = ", "),
        "\n",
        "The unique 'pipeline' variables kept are:",
        paste(vm_dup_pipeline_vars, collapse=", ")
      ))

      vm_sub_final <- vm_grp
    } else if (keep_all_vars == TRUE & max(vm_grp$count) > 1) {
      vm_pairs_multi <- vm_grp_multi |>
        dplyr::ungroup() |>
        dplyr::select(var_name, dplyr::all_of(as.character(latest_year)))

      message(paste(
        "Found multiple results for some original variables, and kept all instances.",
        "\n",
        "The duplicated variables and corresponding unique 'pipeline' name are:",
        "\n"),
        paste(vm_pairs_multi[1],"\n", vm_pairs_multi[2], sep="")
      )
      vm_sub_final <- vm_grp
    }

    print(vm_grp)
  }

  if(names_from == "pipeline") {
    vm_init <- get(variable_map) |>
      dplyr::filter(var_name %in% var_list)
    vm_sub_final <- vm_init
    print(vm)
  }


}

subset_variable_map("vm_nvf_overview", c("wgtgindiv", "wgtghhd"), c(2018, 2019), names_from="original", keep_all_vars = F)
subset_variable_map("vm_nvf_overview", c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", keep_all_vars = T)
subset_variable_map("vm_nvf_overview", c("wgtgindiv", "wgtghhd"), c(2021), names_from="original", keep_all_vars = F)
subset_variable_map("vm_nvf_overview", c("weight_indiv"), c(2021), names_from="pipeline")


scjs_harmonise_variable(scjs_data, c("wgtgindiv", "wgtghhd"))
scjs_harmonise_variable(scjs_data, c("weight_indiv", "weight_household"), names_from="pipeline")

# test invalid entries in var_list
scjs_harmonise_variable(scjs_data, c("askdjfhaks"), names_from="pipeline")
scjs_harmonise_variable(scjs_data, c("weight_indiv", "askdjfhaks"), names_from="pipeline")

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

