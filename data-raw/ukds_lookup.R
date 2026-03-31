# A lookup table of UKDS dataset names

ukds_lookup <- dplyr::tribble(
  ~year, ~survey_year, ~nvf_folder, ~nvf_file,
  2008, "2008/09", "UKDA-6362-spss", "scjs_s2_rf_091214.sav",
  2009, "2009/10", "UKDA-6685-spss", "scjs_s3_rf_ukda_110120.sav",
  2010, "2010/11", "UKDA-7229-spss", "scjs_s4_2010-11-rf_ukda_130115.sav",
  2012, "2012/13", "UKDA-7543-spss", "scjs_main_2012_13_5.sav",
  2014, "2014/15", "UKDA-8141-spss", "new_main_2014_15_2_main_dataset.sav",
  2016, "2016/17", "UKDA-8365-spss", "scjs1617_nvf-main_y1_eul.sav",
  2017, "2017/18", "UKDA-8498-spss", "scjs1718__nvf-main_y2_eul_20190508.sav",
  2018, "2018/19", "UKDA-8795-spss", "scjs1819_nvf-main_y3_eul-safeguarded_20210316_nvf.sav",
  2019, "2019/20", "UKDA-8799-spss", "scjs1920_nvf-main_y4_eul-safeguarded_20210322_nvf.sav",
  2021, "2021/22", "UKDA-9290-spss", "9290_p16472_scjs_y6_nvf_ukds_240314.sav",
  2023, "2023/24", NA, NA,
  2024, "2024/25", NA, NA
)

usethis::use_data(ukds_lookup, overwrite = TRUE)
