
df_dev <-   scjs_harmonise_variable(
  data = "create",
  scjs_data,
  var_list = c("safety_walkingalone", "weight_indiv"),
  names_from = "pipeline"
)




table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"))
table <- scjs_table(df_dev, "time series", var=c("safety_walkingalone"), ts_significance_steps_back = 1)
table2 <- scjs_table(df_dev, "time series", "safety_walkingalone", "qlimit")

table3 <- scjs_table(df_dev, "subgroup", "safety_walkingalone", "qlimit")

table3 <- scjs_table(df_dev, "time series", "safety_walkingalone", "qlimit", time_period = 2018)
table <- scjs_table(df_dev, "time series", "safety_walkingalone", c("total", "qlimit"), time_period = 2018)


foo <- table |>
  dplyr::group_by(variable_name, crossbreak, subgroup, response) |>
  # dplyr::arrange(year) |>
  dplyr::mutate(sig_onestep = significance_nstep(proportion, ci_95)) |>
  dplyr::mutate(sig_twostep = significance_nstep(proportion, ci_95, lag=2)) |>
  dplyr::mutate(sig_fromstart = significance_fromstart(proportion, ci_95)) |>
  dplyr::mutate(sig_mostrecent = significance_mostrecent(proportion, ci_95, .data[["year"]])) |>
  dplyr::mutate(sig_allcombo = significance_allcombo(proportion, ci_95, .data[["year"]], matrix_result = T))



# time series table basically group everything together and only different years are left to test
# - test n steps back
# - test from start
# - test most recent
# - test all combinations

# subgroup is the same but swap subgroup for time_grouping
# - test all combinations

# matrix
# - test all combinations (depends what need to hold constant)

# cross question
# - test all combinations (also depends what needed held constant)

significance_allcombo <- function(proportion, ci, context_var, matrix_result = FALSE) {

  # define expected output
  full_results <- rep(NA, length(proportion))

  # set up format of output based on specification
  if (matrix_result) {
    invalid_res <- "."
    positive_res <- rep("yes", length(proportion))
    negative_res <- "no"
  } else {
    invalid_res <- NA
    positive_res <- context_var
    negative_res <- NA
  }

  for (i in 1:length(proportion)) {
    indiv_result_list <- rep(NA, length(proportion))
    for (j in 1:length(proportion)) {
      if (i == j){
        # indiv_result_list <- append(indiv_result_list, invalid_res)
        indiv_result_list[j] <- invalid_res
      } else {
        if (abs(proportion[i] - proportion[j]) > sqrt(ci[i]^2 + ci[j]^2)) {
          # indiv_result_list <- append(indiv_result_list, positive_res[j])
          indiv_result_list[j] <- positive_res[j]
          # steps <- append(steps, outcome_var[j])
          # steps <- steps[!is.na(steps)]
        } else {
          indiv_result_list[j] <- negative_res
        }
      }
      if (matrix_result) {
        full_results[i] <- paste(indiv_result_list, collapse = ", ")
      } else {
        strip_results <- indiv_result_list[which(!(indiv_result_list %in% c(invalid_res, negative_res)))]
        full_results[i] <- paste(strip_results, collapse = ", ")
      }

    }
  }
  return(full_results)
}
