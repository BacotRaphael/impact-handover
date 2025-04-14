packages = c(
  "tibble",
  "rio",
  "purrr",
  "readr",
  "humind",
  "impactR.utils",
  "dplyr",
  "tidyr",
  "stringr",
  "data.table",
  "DataExplorer"
)

lapply(packages, function(p) library(p, character.only = T)) %>% invisible()


## get datasets from the _targets/objects folder, filtering to keep only files bigger than 700ko
big.file <- file.size(paste0("_targets/objects/", all)) > 700 * 1024
all <- list.files("_targets/objects")
all.b <- all[big.file]
all.c <- all.b[!grepl("pcode", all.b)]

## read all.c in _targets/objects
for (f in all.c){
  assign(gsub("\\.rds", "", f), readRDS(paste0("_targets/objects/", f)))
}

## rename "Ménage" with "main"
names(CAR1_dat) <- gsub("Ménage", "main", names(CAR1_dat))
names(DRC1_dat) <- gsub("hh data", "main", names(DRC1_dat))
names(HTI1_dat) <- gsub("Clean Data", "main", names(HTI1_dat))
names(HTI2_dat) <- gsub("Clean Data", "main", names(HTI2_dat))
names(KEN1_dat) <- gsub("main_cleaned_data", "main", names(KEN1_dat))
names(KEN2_dat) <- gsub("main_cleaned_data", "main", names(KEN2_dat))
names(MLI1_dat) <- gsub("Ménages", "main", names(MLI1_dat))
names(NER1_dat) <- gsub("raw_data_clean", "main", names(NER1_dat))

main <- data.frame()
for (d in all.c){
  main <- bind_rows(main, get(d)$main %>%
                      mutate(
                        country=gsub("\\d", "", d),
                        across(any_of(
                          c(
                            "today",
                            "start",
                            "end",
                            "submission_time",
                            "_submission_time",
                            "dis_arrival_date"
                          )
                        ), \(x) as.Date(x, format = "%Y-%m-%d")
                        ),
                        across(any_of(
                          c(
                            "_id",
                            "_index",
                            "enum_age",
                            "hoh_age",
                            "ind_age_0_1_n",
                            "ind_age_0_4_n",
                            "ind_age_0_5_n",
                            "ind_age_2_11_n",
                            "ind_age_5_17_n",
                            "ind_m_age_5_17_n",
                            "ind_f_age_5_17_n",
                            "ind_f_age_above18_n",
                            "ind_m_age_above18_n",
                            "ind_f_n",
                            "ind_m_n",
                            "ind_potentially_hoh_n",
                            "ind_age_schooling_n",
                            "fsl_fcs_condiments",
                            "health_facility_time",
                            "wash_drinking_water_time_int",
                            "weight",
                            "weights",
                            "dependency_ratio",
                            "resp_age",
                            "hoh_age_final",
                            "hh_size",
                            "ind_age_above18_n",
                            "wash_sanitation_facility_sharing_n",
                            "hhs_score",
                            "rcsi_score",
                            "ind_age_below18_n",
                            "fsl_fc_cell"
                            )
                        ), \(x) as.numeric(x)),
                        across(
                          c(
                            starts_with("cm_income_source."),
                            starts_with("aap_priority_support_ngo."),
                            starts_with("snfi_fds_cooking_issue."),
                            starts_with("snfi_fds_sleeping_issue."),
                            starts_with("snfi_fds_storing_issue."),
                            starts_with("snfi_fds_personal_hygiene_issue."),
                            starts_with("prot_child_sep_reason."),
                            starts_with("fsl_fcs_weight"),
                            starts_with("fsl_lcsi_other_reason."),
                            starts_with("fsl_hhs_"),
                            starts_with("fsl_lcsi_"),
                            starts_with("fsl_fcs_"),
                            starts_with("cm_expenditure_frequent_"),
                            starts_with("cm_expenditure_infrequent_"),
                            matches("^cm_income_source_.*_n$"),
                            matches("^rcsi_.*_weighted$"),
                            starts_with("fsl_rcsi_"),
                            starts_with("fcs_weight_"),
                            ends_with("_other"),
                            starts_with("wash_hygiene_menstrual_issue."),
                            starts_with("fsl_source_food."),
                            starts_with("aap_priority_challenge."),
                            starts_with("aap_preferred_modality."),
                            matches(".*\\..*")
                            ),
                          \(x) as.numeric(x)
                        ),
                        across(
                          any_of(
                            c(
                              "enum_id"
                            )
                          ),
                          \(x) as.character(x)
                        )
                      )
                    )
}

# main <- lapply(all.c, \(x) get(x)$main %>% select(-any_of("today"))) %>% plyr::rbind.fill()




