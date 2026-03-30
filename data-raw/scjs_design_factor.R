# A small data set containing information on the design factor of SCJS survey sweeps

scjs_design_factor <- dplyr::tribble(
  ~year, ~survey_year, ~design_factor,
  2008, "2008/09", 1.5,
  2009, "2009/10", 1.5,
  2010, "2010/11", 1.5,
  2012, "2012/13", 1.3,
  2014, "2014/15", 1.2,
  2016, "2016/17", 1.34,
  2017, "2017/18", 1.22,
  2018, "2018/19", 1.17,
  2019, "2019/20", 1.21,
  2021, "2021/22", 1.19,
  2023, "2023/24", 1.44,
  2024, "2024/25", 1.67
)

usethis::use_data(scjs_design_factor)
