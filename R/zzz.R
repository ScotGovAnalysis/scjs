# Declare some global variable
# Done to satisfy notes during R CMD check
# These datasets / variables are referenced without binding '<-' so this solves that
# utils::globalVariables(c("ukds_lookup", "year"))

# check_years()
utils::globalVariables(c("ukds_lookup"))

# get_var_maps()
utils::globalVariables(c("new_var", "new_val", "old_var", "old_val"))

# scjs_harmonise_variable()
utils::globalVariables(c("vm_nvf"))

# scjs_load_ukds()
utils::globalVariables(c("year"))

# subset_variable_map()
utils::globalVariables(c(".data", "count", "var_name", "section_or_module", "var_type", "requires_recoding"))

# vm_combine_sheets()
utils::globalVariables(c("vm_nvf"))
