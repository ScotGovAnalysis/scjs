
df_dev <-   scjs_harmonise_variable(
  data = "create",
  scjs_data,
  var_list = c("safety_walkingalone", "sex", "weight_indiv"),
  names_from = "pipeline"
)

df_dev <- df_dev |>
  scjs_harmonise_variable(
    scjs_data,
    var_list = c("prev_violent", "prev_property", "simd_15_most"),
    names_from = "pipeline"
)


table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"))
table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), pivot=F)
# table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), ts_significance_steps_back = 1)
table2 <- scjs_table(df_dev, "time series", "safety_walkingalone", "sex")

table3 <- scjs_table(df_dev, "subgroup", "safety_walkingalone", "sex")

table3 <- scjs_table(df_dev, "time series", "safety_walkingalone", "qlimit", time_period = 2018)
# table <- scjs_table(df_dev, "time series", "safety_walkingalone", c("total", "qlimit"), time_period = 2018)



# pivoting a table
wide <- pivot_ts_table(table, "year", "volume")
wide2 <- pivot_ts_table(table2, "year", "proportion")
wide2 <- pivot_ts_table(table2, "year", "volume")
wide3 <- pivot_ts_table(table3, "year", "proportion")



time_grouping <- "year"
result_type <- "proportion"

pvt <- table |>
  dplyr::select(.data[[time_grouping]], proportion, variable_name, crossbreak, subgroup, response) |>
  tidyr::pivot_wider(names_from = time_grouping, values_from = c(result_type)) |>
  dplyr::mutate(type = result_type, .after = "response")

pvt_base <- table |>
  dplyr::select(.data[[time_grouping]], base, variable_name, crossbreak, subgroup, response) |>
  tidyr::pivot_wider(names_from = time_grouping, values_from = c("base")) |>
  dplyr::mutate(type = "unweighted base", .after = "response")

pvt_base_total <- table |>
  dplyr::select(.data[[time_grouping]], base_total, variable_name, crossbreak, subgroup, response) |>
  tidyr::pivot_wider(names_from = time_grouping, values_from = c("base_total")) |>
  dplyr::mutate(type = "unweighted base", .after = "response") |>
  dplyr::mutate(response = "total") |>
  dplyr::group_by(subgroup) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

sig <- table |>
  dplyr::filter(.data[[time_grouping]] == dplyr::last(.data[[time_grouping]])) |>
  dplyr::select(dplyr::starts_with("sig"))

pvt_sig <- dplyr::bind_cols(pvt, sig)

pvt_bind <- dplyr::bind_rows(pvt_sig, pvt_base, pvt_base_total)
