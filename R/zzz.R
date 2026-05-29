# Declare some global variable
# Done to satisfy notes during R CMD check
# These datasets / variables are referenced without binding '<-' so this satisfies that note
# utils::globalVariables(c("ukds_lookup", "year"))

# add_significance_cols()
utils::globalVariables(c("ci_95", "proportion", "response", "subgroup", "variable_name"))

# base_summary_table()
utils::globalVariables(c("base", "base_total", "design_factor", "proportion", "se", "subgroup", "united_col", "variable_name", "volume"))

# check_years()
utils::globalVariables(c("ukds_lookup"))

# format_subgroup()
utils::globalVariables(c("base", "base_total", "crossbreak", "proportion", "response", "sig_allcombo", "subgroup", "variable_name"))

# generate_pooled_weight()
utils::globalVariables(c("design_effect", "design_factor", "effective_sample", "pooled_effective_sample", "sample_size", "scjs_design_factor", "total_effective_sample"))

# get_var_maps()
utils::globalVariables(c("new_var", "new_val", "old_var", "old_val"))

# pivot_ts_table()
utils::globalVariables(c("base", "base_total", "crossbreak", "response", "subgroup", "variable_name"))

# pre_harmonisation_check()
utils::globalVariables(c("original_var"))

# scjs_harmonise_variable()
utils::globalVariables(c("original_var", "vm_nvf"))

# scjs_load_ukds()
utils::globalVariables(c("year"))

# scjs_table()
utils::globalVariables(c("scjs_design_factor"))

# significance_nstep_wrapper()
utils::globalVariables(c("ci_95", "proportion"))

# subset_variable_map()
utils::globalVariables(c(".data", "count", "var_name", "section_or_module", "var_type", "requires_recoding"))

# vm_combine_sheets()
utils::globalVariables(c("vm_nvf"))
