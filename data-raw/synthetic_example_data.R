# This script makes some synthetic example data to be used in tests and examples
# Some of the figures are roughly based on actual SCJS findings, but all data is completely synthetic

# Assumptions for this data set:
# - the split of sex is 50:50, non-binary or other responses are not included in this synthetic dataset
# - sex is independent of year and deprivation status
# - victimisation on violent crime is independent of victimisation or property crime
# These assumptions are made purely to make the creation of synthetic data less complex

# The initial data set will include variables with the real name for that year
# this is to allow the `scjs_harmonise()` function to be demonstrated.
# The synthetic dataset will also include the harmonised versions to be used when demonstrating tables.

# Create the initial data frame
years <- c(2016, 2017, 2018, 2019)
sample_size <- c(5570, 5480, 5540, 5570)
set.seed(42)

n <- sum(sample_size)

scjs_synth_data <- data.frame(
  serial2 = 1:n,
  year = rep(years, sample_size)
) |>
  dplyr::mutate(
    qdgen = sample(c(1, 2), n, replace = TRUE, prob = c(0.5, 0.5)),
    simd_top = ifelse(year %in% c(2016, 2017), sample(c(1,2), sum(sample_size[1], sample_size[2]), replace = TRUE, prob = c(0.15, 0.85)), NA),
    simd_15most = ifelse(year %in% c(2018, 2019), sample(c(1,2), sum(sample_size[3], sample_size[4]), replace = TRUE, prob = c(0.15, 0.85)), NA)
  )


# To create the synthetic disability variable we will use figures obtained from real SCJS data
# this is to reflect that disability is not evenly distributed between sex and SIMD status.

# QCONDIT
scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(
    u = runif(dplyr::n()),  # one random number per row
    qcondit = dplyr::case_when(
      qdgen == 2 & simd_top == 1 ~
        ifelse(u < 0.48, 1, 2),
      qdgen == 2 & simd_top == 2 ~
        ifelse(u < 0.37, 1, 2),
      qdgen == 1 & simd_top == 1 ~
        ifelse(u < 0.43, 1, 2),
      qdgen == 1 & simd_top == 2 ~
        ifelse(u < 0.33, 1, 2),
      qdgen == 2 & simd_15most == 1 ~
        ifelse(u < 0.48, 1, 2),
      qdgen == 2 & simd_15most == 2 ~
        ifelse(u < 0.37, 1, 2),
      qdgen == 1 & simd_15most == 1 ~
        ifelse(u < 0.43, 1, 2),
      qdgen == 1 & simd_15most == 2 ~
        ifelse(u < 0.33, 1, 2)
    )
  ) |>
  dplyr::select(-u)  # clean up

# QLIMIT
scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(
    u = runif(dplyr::n()),  # one random number per row
    qlimit = dplyr::case_when(
      qdgen == 2 & simd_top == 1 & qcondit == 1 ~
        dplyr::case_when(u < 0.44 ~ 1,
                         u < 0.81 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 2 & simd_top == 1 & qcondit == 2 ~ 4,

      qdgen == 2 & simd_top == 2 & qcondit == 1 ~
        dplyr::case_when(u < 0.33 ~ 1,
                         u < 0.70 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 2 & simd_top == 2 & qcondit == 2 ~ 4,

      qdgen == 1 & simd_top == 1 & qcondit == 1 ~
        dplyr::case_when(u < 0.50 ~ 1,
                         u < 0.80 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 1 & simd_top == 1 & qcondit == 2 ~ 4,

      qdgen == 1 & simd_top == 2 & qcondit == 1 ~
        dplyr::case_when(u < 0.31 ~ 1,
                         u < 0.65 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 1 & simd_top == 2 & qcondit == 2 ~ 4,


      qdgen == 2 & simd_15most == 1 & qcondit == 1 ~
        dplyr::case_when(u < 0.44 ~ 1,
                         u < 0.81 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 2 & simd_15most == 1 & qcondit == 2 ~ 4,

      qdgen == 2 & simd_15most == 2 & qcondit == 1 ~
        dplyr::case_when(u < 0.33 ~ 1,
                         u < 0.70 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 2 & simd_15most == 2 & qcondit == 2 ~ 4,

      qdgen == 1 & simd_15most == 1 & qcondit == 1 ~
        dplyr::case_when(u < 0.50 ~ 1,
                         u < 0.80 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 1 & simd_15most == 1 & qcondit == 2 ~ 4,

      qdgen == 1 & simd_15most == 2 & qcondit == 1 ~
        dplyr::case_when(u < 0.31 ~ 1,
                         u < 0.65 ~ 2,
                         u <= 1 ~ 3),
      qdgen == 1 & simd_15most == 2 & qcondit == 2 ~ 4,

    )
  ) |>
  dplyr::select(-u)


# Create the harmonised versions of the demographic variables

scjs_synth_data_list <- split(scjs_synth_data, scjs_synth_data$year)

scjs_synth_data <- scjs_synth_data |>
  scjs::scjs_harmonise_variable(
    df_list = scjs_synth_data_list,
    var_list = c("sex", "simd_15_most", "disability"),
    names_from = "pipeline")

# Creating the victimisation variables
# Again basing this on findings from real survey results
# Adjusting the rate based on year, sex, simd, disability
# Property crime and household crime are dependent variables - e.g. all household victims are property victims

# Here essentially i am avoiding writing hundreds of lines of logic by creating a lookup table for each demographic
# The lookup table is used to form the thresholds for the random selection and is joined on to the main data set
# Then i can just write logic once to construct the variables using the thresholds rather than repeating every case of demographic combination

# These vectors are the output of the summary table using the actual data and extracting just the proportion column
# It could be roughly replicated by grouping year, sex, simd and disability and preserving in alphabetical order within groups
# e.g. first figure is 2016, female, 15% most deprived areas, disabled - non-victim, then victim etc.
# a replica of the labels part of the table can be recreated using the `build_data_lookup()` function below
violent_pct = c(
  0.97,
  0.03,
  0.967,
  0.033,
  0.97,
  0.03,
  0.981,
  0.019,
  0.907,
  0.093,
  0.952,
  0.048,
  0.958,
  0.042,
  0.973,
  0.027,
  0.977,
  0.023,
  0.969,
  0.031,
  0.979,
  0.021,
  0.981,
  0.019,
  0.958,
  0.042,
  0.951,
  0.049,
  0.975,
  0.025,
  0.979,
  0.021,
  0.951,
  0.049,
  0.984,
  0.016,
  0.974,
  0.026,
  0.984,
  0.016,
  0.92,
  0.08,
  0.982,
  0.018,
  0.978,
  0.022,
  0.979,
  0.021,
  0.923,
  0.077,
  0.971,
  0.029,
  0.977,
  0.023,
  0.986,
  0.014,
  0.945,
  0.055,
  0.938,
  0.062,
  0.963,
  0.037,
  0.977,
  0.023
)

property_pct <- c(
  0.843,
  0.157,
  0.845,
  0.155,
  0.878,
  0.122,
  0.899,
  0.101,
  0.73,
  0.27,
  0.876,
  0.124,
  0.875,
  0.125,
  0.897,
  0.103,
  0.834,
  0.166,
  0.841,
  0.159,
  0.86,
  0.14,
  0.916,
  0.084,
  0.833,
  0.167,
  0.856,
  0.144,
  0.894,
  0.106,
  0.898,
  0.102,
  0.853,
  0.147,
  0.85,
  0.15,
  0.863,
  0.137,
  0.89,
  0.11,
  0.899,
  0.101,
  0.865,
  0.135,
  0.895,
  0.105,
  0.909,
  0.091,
  0.861,
  0.139,
  0.892,
  0.108,
  0.875,
  0.125,
  0.902,
  0.098,
  0.855,
  0.145,
  0.87,
  0.13,
  0.896,
  0.104,
  0.919,
  0.081
)

housebreak_pct <- c(
  0.994,
  0.006,
  1,
  0,
  0.99,
  0.01,
  0.994,
  0.006,
  0.948,
  0.052,
  0.981,
  0.019,
  0.99,
  0.01,
  0.992,
  0.008,
  0.997,
  0.003,
  0.981,
  0.019,
  0.987,
  0.013,
  0.997,
  0.003,
  1,
  0,
  0.986,
  0.014,
  0.992,
  0.008,
  0.99,
  0.01,
  0.971,
  0.029,
  0.994,
  0.006,
  0.991,
  0.009,
  0.994,
  0.006,
  0.981,
  0.019,
  0.985,
  0.015,
  0.987,
  0.013,
  0.993,
  0.007,
  0.987,
  0.013,
  1,
  0,
  0.984,
  0.016,
  0.994,
  0.006,
  0.972,
  0.028,
  0.988,
  0.012,
  0.992,
  0.008,
  0.994,
  0.006
)

qsfdark_pct <- c(
  0.191,
  0.224,
  0.218,
  0.367,
  0.167,
  0.435,
  0.231,
  0.167,
  0.244,
  0.335,
  0.245,
  0.175,
  0.336,
  0.406,
  0.182,
  0.076,
  0.314,
  0.279,
  0.261,
  0.147,
  0.383,
  0.452,
  0.128,
  0.038,
  0.51,
  0.347,
  0.089,
  0.055,
  0.616,
  0.313,
  0.061,
  0.01,
  0.111,
  0.348,
  0.289,
  0.252,
  0.15,
  0.387,
  0.268,
  0.195,
  0.278,
  0.316,
  0.214,
  0.192,
  0.373,
  0.346,
  0.204,
  0.077,
  0.341,
  0.33,
  0.212,
  0.117,
  0.372,
  0.426,
  0.132,
  0.069,
  0.516,
  0.325,
  0.108,
  0.051,
  0.614,
  0.322,
  0.055,
  0.009,
  0.125,
  0.263,
  0.252,
  0.361,
  0.189,
  0.38,
  0.284,
  0.147,
  0.254,
  0.32,
  0.238,
  0.188,
  0.348,
  0.39,
  0.196,
  0.066,
  0.363,
  0.375,
  0.175,
  0.087,
  0.435,
  0.373,
  0.151,
  0.041,
  0.451,
  0.343,
  0.132,
  0.074,
  0.624,
  0.313,
  0.055,
  0.009,
  0.122,
  0.244,
  0.317,
  0.317,
  0.154,
  0.383,
  0.291,
  0.172,
  0.242,
  0.355,
  0.234,
  0.169,
  0.33,
  0.389,
  0.214,
  0.067,
  0.358,
  0.35,
  0.162,
  0.129,
  0.437,
  0.418,
  0.092,
  0.053,
  0.506,
  0.313,
  0.128,
  0.053,
  0.599,
  0.346,
  0.045,
  0.01
)

qdconf_pct <- c(
  0.186,
  0.581,
  0.183,
  0.05,
  0.222,
  0.607,
  0.144,
  0.027,
  0.153,
  0.649,
  0.154,
  0.044,
  0.181,
  0.667,
  0.126,
  0.025,
  0.273,
  0.42,
  0.202,
  0.105,
  0.25,
  0.625,
  0.097,
  0.028,
  0.176,
  0.547,
  0.215,
  0.061,
  0.223,
  0.622,
  0.125,
  0.03,
  0.233,
  0.541,
  0.185,
  0.041,
  0.157,
  0.642,
  0.147,
  0.054,
  0.166,
  0.598,
  0.18,
  0.056,
  0.183,
  0.651,
  0.141,
  0.025,
  0.248,
  0.443,
  0.223,
  0.087,
  0.223,
  0.612,
  0.14,
  0.026,
  0.14,
  0.607,
  0.204,
  0.049,
  0.206,
  0.64,
  0.128,
  0.026,
  0.162,
  0.628,
  0.158,
  0.052,
  0.202,
  0.578,
  0.191,
  0.029,
  0.16,
  0.643,
  0.165,
  0.031,
  0.166,
  0.669,
  0.139,
  0.025,
  0.231,
  0.537,
  0.129,
  0.103,
  0.272,
  0.58,
  0.116,
  0.032,
  0.176,
  0.611,
  0.162,
  0.052,
  0.213,
  0.63,
  0.127,
  0.029,
  0.139,
  0.615,
  0.173,
  0.073,
  0.179,
  0.6,
  0.163,
  0.058,
  0.139,
  0.664,
  0.144,
  0.053,
  0.155,
  0.672,
  0.154,
  0.019,
  0.194,
  0.508,
  0.253,
  0.046,
  0.233,
  0.581,
  0.148,
  0.038,
  0.161,
  0.607,
  0.174,
  0.057,
  0.186,
  0.671,
  0.122,
  0.021
)

build_data_lookup <- function(name, options, pct_vector) {
  years <- c(2016, 2017, 2018, 2019)
  sexes <- c("female", "male")
  simd_status <- c("15% most deprived", "remainder")
  disability_status <- c("disabled", "non-disabled")

  n_demographic_combinations <- length(years) * length(sexes) * length(simd_status) * length(disability_status)
  total_rows <- n_demographic_combinations * length(options)

  demographic_combinations <- data.frame(
    year = rep(years, each = total_rows/4), # 4 * 16
    sex = rep(sexes, 4, each = total_rows/8), # 2 * 4 * 8
    simd_15_most = rep(simd_status, 8, each = total_rows/16), # 2 * 8 * 4
    disability = rep(disability_status, 16, each = total_rows/32) # 2 * 16 * 2
  )

  demographic_combinations <- demographic_combinations |>
    dplyr::mutate("{name}" := rep(options, total_rows/(1 * length(options))))

  if(length(pct_vector) != nrow(demographic_combinations)) {
    stop("length of pct_vector not equal to number of rows in demographic combinations data frame")
  }
  lookup_with_pct <- dplyr::bind_cols(demographic_combinations, pct_vector = pct_vector)

  # create cumulative percentage column
  lookup_with_cumulative_pct <- lookup_with_pct |>
    dplyr::group_by(year, sex, simd_15_most, disability) |>
    dplyr::mutate(cumulative_pct = cumsum(pct_vector),
                  cumulative_pct = ifelse(is.na(cumulative_pct), pct_vector, cumulative_pct)) |>
    dplyr::select(-pct_vector)

  # pivot the dataframe wider to get one column per response option
  lookup_wider <- lookup_with_cumulative_pct |>
    tidyr::pivot_wider(names_from = .data[[name]], values_from = cumulative_pct, names_prefix = "threshold_")

  # force the final column to be 1 as the figures are based on rounded figures so might bve slightly out
  lookup_wider[[ncol(lookup_wider)]] <- 1
  return(lookup_wider)
}

# VICTIM OF VIOLENT CRIME
violent_framework <- build_data_lookup("prevviolent", c("non-victim", "victim"), violent_pct)

scjs_synth_data <- dplyr::left_join(scjs_synth_data, violent_framework)

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(u_vio = runif(dplyr::n()))

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(prevviolent = dplyr::case_when(
    u_vio < `threshold_non-victim` ~ 2,
    u_vio <= threshold_victim ~ 1
  ))

violent_summ_compar <- scjs_synth_data |>
  dplyr::group_by(year, sex, simd_15_most, disability, prevviolent) |>
  dplyr::summarise(count = dplyr::n()) |>
  dplyr::mutate(pct = round(count / sum(count), 3)) |>
  dplyr::ungroup()

scjs_synth_data <- scjs_synth_data |>
  dplyr::select(-dplyr::starts_with(c("u", "threshold")))


# VICTIM OF PROPERTY CRIME
property_framework <- build_data_lookup("prevproperty", c("non-victim", "victim"), property_pct)

scjs_synth_data <- dplyr::left_join(scjs_synth_data, property_framework)

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(u_prop = runif(dplyr::n()))

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(prevproperty = dplyr::case_when(
    u_prop < `threshold_non-victim` ~ 2,
    u_prop <= threshold_victim ~ 1
  ))

prop_summ_compar <- scjs_synth_data |>
  dplyr::group_by(year, sex, simd_15_most, disability, prevproperty) |>
  dplyr::summarise(count = dplyr::n()) |>
  dplyr::mutate(pct = round(count / sum(count), 3)) |>
  dplyr::ungroup()

scjs_synth_data <- scjs_synth_data |>
  dplyr::select(-dplyr::starts_with(c("threshold")))

# VICTIM OF HOUSEHOLD CRIME (HHD WEIGHT)
# DEPENDENT VARIABLE FROM PROPERTY CRIME SO WILL REUSE SAME U VALUE
# E.G. ALL HOUSEBREAK VICTIMS ARE PROPERTY VICTIMS (BUT NOT VICE VERSA)
housebreak_framework <- build_data_lookup("prevhousebreak", c("non-victim", "victim"), housebreak_pct)

scjs_synth_data <- dplyr::left_join(scjs_synth_data, housebreak_framework)

# not recreating the u-value as reusing u_prop due to dependence

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(prevhousebreak = dplyr::case_when(
    u_prop < `threshold_non-victim` ~ 2,
    u_prop <= threshold_victim ~ 1
  ))

housebreak_summ_compar <- scjs_synth_data |>
  dplyr::group_by(year, sex, simd_15_most, disability, prevhousebreak) |>
  dplyr::summarise(count = dplyr::n()) |>
  dplyr::mutate(pct = round(count / sum(count), 3)) |>
  dplyr::ungroup() |>
  tidyr::complete(year, sex, simd_15_most, disability, prevhousebreak) |>
  dplyr::mutate(pct = ifelse(is.na(pct), 0, pct))

scjs_synth_data <- scjs_synth_data |>
  dplyr::select(-dplyr::starts_with(c("u", "threshold")))

# QSFDARK
qsfdark_framework <- build_data_lookup("qsfdark", c(1, 2, 3, 4), qsfdark_pct)

scjs_synth_data <- dplyr::left_join(scjs_synth_data, qsfdark_framework)

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(u_qsf = runif(dplyr::n()))

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(qsfdark = dplyr::case_when(
    u_qsf < threshold_1 ~ 1,
    u_qsf < threshold_2 ~ 2,
    u_qsf < threshold_3 ~ 3,
    u_qsf <= threshold_4 ~ 4
  ))

qsfdark_summ_compar <- scjs_synth_data |>
  dplyr::group_by(year, sex, simd_15_most, disability, qsfdark) |>
  dplyr::summarise(count = dplyr::n()) |>
  dplyr::mutate(pct = round(count / sum(count), 3)) |>
  dplyr::ungroup()

scjs_synth_data <- scjs_synth_data |>
  dplyr::select(-dplyr::starts_with(c("u", "threshold")))


# QDCONF_03
# This is a quarter sample question, I am just going to create a value for all rows and then randomly assign 75% back to NA
qdconf_framework <- build_data_lookup("qdconf_03", c(1, 2, 3, 4), qdconf_pct)

scjs_synth_data <- dplyr::left_join(scjs_synth_data, qdconf_framework)

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(u_qd = runif(dplyr::n()))

scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(qdconf_03 = dplyr::case_when(
    u_qd < threshold_1 ~ 1,
    u_qd < threshold_2 ~ 2,
    u_qd < threshold_3 ~ 3,
    u_qd <= threshold_4 ~ 4
  ))

# assign 75% of responses to NA
scjs_synth_data <- scjs_synth_data |>
  dplyr::mutate(u_quartersample = runif(dplyr::n()),
                qdconf_03 = ifelse(u_quartersample < 0.75, NA, qdconf_03))

qdconf_summ_compar <- scjs_synth_data |>
  dplyr::filter(!is.na(qdconf_03)) |>
  dplyr::group_by(year, sex, simd_15_most, disability, qdconf_03) |>
  dplyr::summarise(count = dplyr::n()) |>
  dplyr::mutate(pct = round(count / sum(count), 3)) |>
  dplyr::ungroup()

scjs_synth_data <- scjs_synth_data |>
  dplyr::select(-dplyr::starts_with(c("u", "threshold")))


# Create the harmonised versions of the victimisation and attitudinal variables
scjs_synth_data_list <- split(scjs_synth_data, scjs_synth_data$year)

scjs_synth_data <- scjs_synth_data |>
  scjs::scjs_harmonise_variable(
    df_list = scjs_synth_data_list,
    var_list = c("prev_violent", "prev_property", "prev_housebreaking", "safety_walkingalone", "qdconf_access"),
    names_from = "pipeline")


# create synthetic weights
# While it would be possible to create a synthetic distribution of the weights it doesn't really make sense here
# The random assignment to the variables was done to reflect the real weighted data, so adding a random weight would
# cause the data to stray from the real thing.
# We need something to be a weight value, as it needs to be passed to certain function, but we can just put the same value
# for each row.
# To get something resembling reality, we can make the weight sum up to the real totals for each year.
# Our sample size does not exactly match up with the real data, so it will be out by a few hundred each time
# (i also am just using the rounded mean weight)


weight_df <- data.frame(
  year = c(2016, 2017, 2018, 2019),
  weight_indiv = c(806, 823, 816, 816),
  weight_household = c(440, 450, 447, 445)
)

scjs_synth_data <- scjs_synth_data |>
  dplyr::left_join(weight_df)

# FINALISE DATA SET
scjs_synthetic_data <- scjs_synth_data |>
  dplyr::relocate(prev_property, .after = prevproperty) |>
  dplyr::relocate(prev_housebreaking, .after = prevhousebreak) |>
  dplyr::relocate(safety_walkingalone, .after = qsfdark) |>
  dplyr::relocate(qdconf_access, .after = qdconf_03)

usethis::use_data(scjs_synthetic_data, overwrite = TRUE)
