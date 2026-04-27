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


#' SCJS NVF Variable Map
#'
#' A data frame list containing the variable map for the SCJS non-victim form.
#' Each element contains a data frame. The first is the overview which describes
#' which variables from individual years are used to produce the harmonised variables.
#' The others describe exactly how the data recoding is done in each specific instance.
#'
#'
#' @format ## `vm_nvf`
#' A list containing data frames:
#' \describe{
#'   \item{overview}{Overview of what variables from individual years make up harmonised variables}
#'   \item{demographics}{Personal characteristics of respondent e.g. sex, disability, religion etc.}
#'   \item{socio_economic}{Characteristics of respondent or respondent's household e.g. deprivation, household income etc.}
#'   \item{geography}{Information about respondent's location e.g. local authority, urban/rural etc.}
#'   \item{victimisation}{Summary variables of respondent's victim status for each crime type - includes incident count}
#'   \item{cjs_confidence}{Responses to questions around confidence in the criminal justice system}
#'   \item{police_confidence}{Responses to questions on the topic of policing e.g. confidence in police, police visibility, police contact}
#'   \item{crime_and_safety}{Attitudinal questions on changes in crime rate, feelings of safety etc.}
#'   \item{copfs}{Responses to questions relating to the Crown Office and Procurator Fiscal Service - COPFS}
#'   \item{other}{Currently unused - Questions not falling under any other category}
#'   ...
#' }
"vm_nvf"
