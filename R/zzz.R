# Declare some global variable
# Done to satisfy notes during R CMD check
# These datasets / variables are referenced without binding '<-' so this solves that
utils::globalVariables(c("ukds_lookup", "year"))
