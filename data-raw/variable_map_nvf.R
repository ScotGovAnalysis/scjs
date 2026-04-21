# Script to load in version of the NVF variable map

# Get the names of all the sheets
vm_nvf_sheet_list <- readxl::excel_sheets(path = "./inst/extdata/variable_map_nvf.xlsx")

# Read the individual sheets in as a df within a list
vm_nvf <- purrr::map(vm_nvf_sheet_list, ~ readxl::read_xlsx("./inst/extdata/variable_map_nvf.xlsx", sheet = .x))

# Set the names of the dfs in the list
names(vm_nvf) <- vm_nvf_sheet_list

# call usethis::use_data() to add the datasets to the package
usethis::use_data(list = vm_nvf, overwrite = TRUE)

# Each sheet is now within the object 'vm_nvf', so the demographics sheet can be accessed like:
# vm_nvf$demographics, vm_nvf[["demographics"]] etc.
