
df_dev <-   scjs_harmonise_variable(
  data = "create",
  scjs_data,
  var_list = c("safety_walkingalone", "sex", "weight_indiv"),
  names_from = "pipeline"
)

df_dev <- df_dev |>
  scjs_harmonise_variable(
    scjs_data,
    var_list = c("prev_violent", "prev_property", "simd_15_most", "disability"),
    names_from = "pipeline"
)


table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"))
table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), pivot=F)
# table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), ts_significance_steps_back = 1)
table2 <- scjs_table(df_dev, "time series", "safety_walkingalone", "sex")

table3 <- scjs_table(df_dev, "subgroup", "safety_walkingalone", "sex")
table3 <- scjs_table(df_dev, "subgroup", "safety_walkingalone", "simd_15_most")

table3 <- scjs_table(df_dev, "time series", "safety_walkingalone", "qlimit", time_period = 2018)
table4 <- scjs_table(df_dev, "time series", "safety_walkingalone", crossbreak=list("total", "sex", c("sex", "simd_15_most"))) # intersection
table5 <- scjs_table(df_dev, "subgroup", "safety_walkingalone", crossbreak=list(c("sex", "simd_15_most")))
# table <- scjs_table(df_dev, "time series", "safety_walkingalone", c("total", "qlimit"), time_period = 2018)

#TODO

# aggregation

# is this the best way to do aggregation? it is more explicit but does it need to be?
# makes it easier to have it show only the aggregations, e.g. make the aggregation then only pass that to the table function
# seems like any attempt to make it part of the function would end up just having to make a new column anyway

df_dev <- df_dev |>
  dplyr::mutate(safety_aggregated = dplyr::case_when(safety_walkingalone %in% c("very safe", "fairly safe") ~ "net: safe",
                                                     safety_walkingalone %in% c("a bit unsafe", "vey unsafe") ~ "net: unsafe",
                                                     TRUE ~ NA_character_))

table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone", "safety_aggregated"))
table2 <- scjs_table(df_dev, "time series", "safety_aggregated", "sex")


# test concept of pooled analysis (haven't made the weights)
df_dev <- df_dev |>
  dplyr::mutate(all_scjs_years = "all scjs years",
                split2 = dplyr::case_when(year %in% c(2016, 2017) ~ "2016/17 and 2017/18",
                                          year %in% c(2018, 2019) ~ "2018/19 and 2019/20"))


table_pool <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), time_grouping = "all_scjs_years")
table_pool <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), time_grouping = "split2")
table_pool <- scjs_table(df_dev, "subgroup", var=c("safety_walkingalone"), time_grouping = "split2")
