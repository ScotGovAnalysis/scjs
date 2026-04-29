vars_to_load <- c(
  # "SERIAL2",
  "PREVPROPERTY", # crime - combined weight
  "PREVVIOLENT", # crime - individually weighted
  "PREVHOUSEBREAK", # crime - household weight
  "QSFDARK", # full sample question - can be aggregated
  "QDCONF_03", # partial sample question
  "SIMD_15MOST", # demographic
  "QCONDIT",
  "QLIMIT",
  "WGTGINDIV", # individual weight
  "WGTGHHD" # household weight
)


scjs_data <- scjs_load_ukds(path = Sys.getenv("USER_DATA_PATH"), years_to_load = 2018:2019, columns = vars_to_load)

list2env(scjs_data, envir=.GlobalEnv)

"year" %in% names(scjs_data[[1]])
"year" %in% names(scjs_data[[2]])
