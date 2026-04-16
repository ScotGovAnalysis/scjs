# A script to document the datasets used in the package

#' SCJS Design Factor Information
#'
#' A small data set containing the values of the generic design factor for each
#' single SCJS year
#'
#' @format ## `scjs_design_factor`
#' A data frame with 12 rows and 3 columns:
#' \describe{
#'   \item{year}{Numeric year when most of the fieldwork occurred}
#'   \item{survey_year}{Formatted value for fieldwork year}
#'   \item{design_factor}{Generic design factor associated with that survey}
#'   ...
#' }
"scjs_design_factor"


#' UKDS SCJS Name Lookup Table
#'
#' A lookup table containing the names of folders and files as they are when
#' downloaded from the UKDS.
#'
#' @format ## `ukds_lookup`
#' A data frame with 12 rows and 4 columns:
#' \describe{
#'   \item{year}{Numeric year when most of the fieldwork occurred}
#'   \item{survey_year}{Formatted value for fieldwork year}
#'   \item{nvf_folder}{Folder containing non-victim form (NVF) files for a given year}
#'   \item{nvf_file}{Name of the SPSS file containing non-victim form (NVF data for a given year)}
#'   ...
#' }
"ukds_lookup"


#' SCJS NVF Variable Map Overview
#'
#' An overview of how variables have changed over time in SCJS data.
#'
#'
#' @format ## `vm_nvf_overview`
#' A data frame with 463 rows and 21 columns:
#' \describe{
#'   \item{section_or_module}{Name of the section where the sheet containing the exact recoding is given}
#'   \item{var_name}{Name of the variable as it appears in the original SG SCJS data pipeline code}
#'   ...
#' }
"vm_nvf_overview"
