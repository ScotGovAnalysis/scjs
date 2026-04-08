vars_to_load <- c(
    "SERIAL2",
    "PREVPROPERTY", # crime - combined weight
    "PREVVIOLENT", # crime - individually weighted
    "PREVHOUSEBREAK", # crime - household weight
    "QSFDARK", # full sample question - can be aggregated
    "QDCONF_03", # partial sample question
    "SIMD_15MOST", # demographic
    "WGTGINDIV", # individual weight
    "WGTGHHD" # household weight
)


trial_load <- scjs_load_ukds(path = Sys.getenv("USER_DATA_PATH"), years_to_load = 2018:2019, columns = vars_to_load)

list2env(trial_load, envir=.GlobalEnv)

"year" %in% names(trial_load[[1]])
"year" %in% names(trial_load[[2]])
