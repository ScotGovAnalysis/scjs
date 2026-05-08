
df_dev <-   scjs_harmonise_variable(
  data = "create",
  scjs_data,
  var_list = c("safety_walkingalone", "weight_indiv"),
  names_from = "pipeline"
)




table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone", "qlimit"))
table2 <- scjs_table(df_dev, "time series", "safety_walkingalone", "qlimit")
table3 <- scjs_table(df_dev, "time series", "safety_walkingalone", "qlimit", time_period = 2018)
table <- scjs_table(df_dev, "time series", "safety_walkingalone", c("total", "qlimit"), time_period = 2018)

