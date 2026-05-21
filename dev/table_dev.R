
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
table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), ts_significance_steps_back = 1)
table2 <- scjs_table(df_dev, "time series", "safety_walkingalone", "sex")

table3 <- scjs_table(df_dev, "subgroup", "safety_walkingalone", "sex")

table3 <- scjs_table(df_dev, "time series", "safety_walkingalone", "qlimit", time_period = 2018)
table <- scjs_table(df_dev, "time series", "safety_walkingalone", c("total", "qlimit"), time_period = 2018)



# pivoting a table
wide <- pivot_ts_table(table2, "year", "proportion")
