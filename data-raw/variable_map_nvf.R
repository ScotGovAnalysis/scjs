# Script to load in version of the NVF variable map

vm_nvf_overview <- readxl::read_xlsx("./inst/extdata/variable_map_nvf.xlsx", sheet = "overview")

usethis::use_data(vm_nvf_overview, overwrite = TRUE)
