dataset <- scjs_synthetic_data

# implementation function
qsf_aggregation <- function(variable) {
  aggregated_variable <- dplyr::case_when(variable %in% list(3, 4, "very unsafe", "a bit unsafe") ~ "net: unsafe",
                                          variable %in% list(1, 2, "fairly safe", "very safe") ~ "net: safe",
                                          TRUE ~ NA_character_)
  return(aggregated_variable)
}


# implementation via anonymous function and lookup

aggregate_var <- function(variable, aggregate_function=NULL) {

  # extract var_lookup outside of this function?
  var_lookup <- list(
    qsf_aggregation = c("safety_walkingalone", "safety_athomealone")
  )


  # some code to find name - can specify the name
  if (!is.null(aggregate_function)) {
    function_name <- aggregate_function
  } else {
    function_name <- "qsf_aggregation" # replace with logic to look through var_lookup
  }


  aggregated_variable <- do.call(function_name, args = list(variable))
  return(aggregated_variable)
}


dataset <- dataset |>
  dplyr::mutate(qsf_agg2 = aggregate_var(safety_walkingalone),
                qsf_agg3 = aggregate_var(qsfdark), # can use the numeric version but should this get separated out in the actual aggregation function?
                .after = "safety_walkingalone")
