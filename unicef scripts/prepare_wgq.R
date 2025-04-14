rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Load necessary packages
library(tibble)
library(rio)
library(purrr)
library(readr)
library(humind)
library(impactR.utils)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(DataExplorer)

path <- list.files("R/", recursive = T)
for (i in path) {source(paste0("R/",i))}

# Define the named vector of country ISOs
cty_iso <- c(
  AFG1 = "AFG",
  BFA1 = "BFA",
  CAR1 = "CAR",
  DRC1 = "DRC",
  ETH1 = "ETH",
  HTI1 = "HTI",
  HTI2 = "HTI",
  KEN1 = "KEN",
  KEN2 = "KEN",
  MLI1 = "MLI",
  MMR1 = "MMR",
  NER1 = "NER",
  SOM1 = "SOM",
  SYR1 = "SYR",
  UGA1 = "UGA",
  UKR1 = "UKR",
  UKR2 = "UKR"
)
cty_iso_names <- "HTI1"

# STEP 1 and 2: Mapping of choices and survey
# Extract mapping files
msp <- list.files("data/mapping/mapping_survey/", full.names = TRUE)
mcp <- list.files("data/mapping/mapping_choices/", full.names = TRUE)
cmp <- list.files("data/mapping/check_mapping/", full.names = TRUE)

# Check, get, and pivot mappings
m <- mapping.get_mapping(msp, mcp, cmp)
pm <- mapping.pivot_mapping(m)

# STEP 3: Load and prepare datasets
# Metadata and variable name files
metadata_path <- "data/metadata.csv"
vn_path <- "data/vn.csv"
vn_df <- readr::read_csv(vn_path)
metadata_df <- readr::read_csv(metadata_path)

# Sample files (example for SOM1 and KEN1)

## Pcodes files
SOM1_pcode_admin1_path <- "data/sample_design/SOM1_pcode_admin1.csv"
SOM1_pcode_admin1_df = rio::import(SOM1_pcode_admin1_path)
SOM1_pcode_admin2_path <- "data/sample_design/SOM1_pcode_admin2.csv"
SOM1_pcode_admin2_df = rio::import(SOM1_pcode_admin2_path)
KEN1_pcode_admin1_path <- "data/sample_design/KEN1_pcode_admin1.csv"
KEN1_pcode_admin1_df = rio::import(KEN1_pcode_admin1_path)
KEN1_pcode_admin2_path <- "data/sample_design/KEN1_pcode_admin2.csv"
KEN1_pcode_admin2_df = rio::import(KEN1_pcode_admin2_path)
KEN2_pcode_admin1_path <- "data/sample_design/KEN2_pcode_admin1.csv"
KEN2_pcode_admin1_df = rio::import(KEN2_pcode_admin1_path)
KEN2_pcode_admin2_path <- "data/sample_design/KEN2_pcode_admin2.csv"
KEN2_pcode_admin2_df = rio::import(KEN2_pcode_admin2_path)

## Population files
UKR2_pop_path <- "data/sample_design/UKR2_pop.csv"
UKR2_pop = rio::import(UKR2_pop_path)
# URLs
url_df = metadata_df |> dplyr::filter(!is.na(url))

#------ Check url changes and download data
BFA1_url<- get_url_from_metadata(url_df, "BFA1")
BFA1_dat = rio::import_list(BFA1_url, guess_max = 21474836)
CAR1_url<- get_url_from_metadata(url_df, "CAR1")
CAR1_dat = rio::import_list(CAR1_url, guess_max = 21474836)
DRC1_url<- get_url_from_metadata(url_df, "DRC1")
DRC1_dat = rio::import_list(DRC1_url, guess_max = 21474836)
ETH1_url<- get_url_from_metadata(url_df, "ETH1")
ETH1_dat = rio::import_list(ETH1_url, guess_max = 21474836)
HTI1_url<- get_url_from_metadata(url_df, "HTI1")
HTI1_dat = rio::import_list(HTI1_url, guess_max = 21474836)
HTI2_url<- get_url_from_metadata(url_df, "HTI2")
HTI2_dat = rio::import_list(HTI2_url, guess_max = 21474836)
KEN1_url<- get_url_from_metadata(url_df, "KEN1")
KEN1_dat = rio::import_list(KEN1_url, guess_max = 21474836)
KEN2_url<- get_url_from_metadata(url_df, "KEN2")
KEN2_dat = rio::import_list(KEN2_url, guess_max = 21474836)
MLI1_url<- get_url_from_metadata(url_df, "MLI1")
MLI1_dat = rio::import_list(MLI1_url, guess_max = 21474836)
NER1_url<- get_url_from_metadata(url_df, "NER1")
NER1_dat = rio::import_list(NER1_url, guess_max = 21474836)
SOM1_url<- get_url_from_metadata(url_df, "SOM1")
SOM1_dat = lapply(SOM1_url, \(x) rio::import_list(x, guess_max = 21474836)) |> Reduce(append, x = _)
UGA1_url<- get_url_from_metadata(url_df, "UGA1")
UGA1_dat = rio::import_list(UGA1_url, guess_max = 21474836)
UKR1_url<- get_url_from_metadata(url_df, "UKR1")
UKR1_dat = rio::import_list(UKR1_url, guess_max = 21474836)
UKR2_url<- get_url_from_metadata(url_df, "UKR2")
UKR2_dat = rio::import_list(UKR2_url, guess_max = 21474836)

#------ Get online sheets

## Join the loops and rename vn (and custom import functions if needed)
BFA1 = get_joined_loops(BFA1_dat, metadata_df, vn_df, "BFA1")
CAR1 = get_joined_loops(CAR1_dat, metadata_df, vn_df, "CAR1")
DRC1 = get_joined_loops(DRC1_dat, metadata_df, vn_df, "DRC1")
# Archived data---tbd what to do
# ETH1 = get_joined_loops(ETH1_dat, metadata_df, vn_df, "ETH1")
HTI1 = get_joined_loops(HTI1_dat, metadata_df, vn_df, "HTI1")
HTI2 = get_joined_loops(HTI2_dat, metadata_df, vn_df, "HTI2")
KEN1 = get_joined_loops(KEN1_dat, metadata_df, vn_df, "KEN1")
KEN2 = get_joined_loops(KEN2_dat, metadata_df, vn_df, "KEN2")
MLI1 = get_joined_loops(MLI1_dat, metadata_df, vn_df, "MLI1") |>
  # MLI review uuid in loop
  MLI1.import()
NER1 = get_joined_loops(NER1_dat, metadata_df, vn_df, "NER1") |>
  # NER 1 had NER instead of NE for pcodes
  NER1.import()
SOM1 = get_joined_loops(SOM1_dat, metadata_df, vn_df, "SOM1")
UGA1 = get_joined_loops(UGA1_dat, metadata_df, vn_df, "UGA1") |>
  # UGA1 needs a stratum column
  UGA1.import()
UKR1 = get_joined_loops(UKR1_dat, metadata_df, vn_df, "UKR1")
UKR2 = get_joined_loops(UKR2_dat, metadata_df, vn_df, "UKR2")

#------ Get offline data
AFG1 = get_offline_data(metadata_df, vn_df, "AFG1") |>
  # AFG1 used UNOCHA AFG specific admin 1
  AFG1.import()
ETH1 = get_offline_data(metadata_df, vn_df, "ETH1")
MMR1 = get_offline_data(metadata_df, vn_df, "MMR1")
SYR1 = get_offline_data(metadata_df, vn_df, "SYR1") |>
  ## SYR 1 does not have a stratum column
  SYR1.import()

#------ Get MOZ dataset
MOZ1 <- rio::import_list("data/offline_dataset/MOZ1.xlsx")
names(MOZ1)[which(names(MOZ1) %in% c("Household_data", "Individual_data"))] <- c("main", "loop")
MOZ1$main <- MOZ1$main |> rename_with(~ unlist(unname(as.vector(MOZ1$main[1, ])))) |> slice(-1) |> type_convert()
MOZ1$loop <- MOZ1$loop |> rename_with(~ unlist(unname(as.vector(MOZ1$loop[1, ])))) |> slice(-1) |> type_convert()

rename_main <- c(
  "population_group"="population_group",
  "admin1"="admin1",
  "admin2"="admin2",
  "admin3"="admin3",
  "resp_age"="resp_age",
  "resp_gender"="resp_gender",
  "hoh_age"="hoh_age",
  "hoh_gender"="hoh_gender",
  "uuid"="uuid",
  "weights"="weight"
)
MOZ1$main <- MOZ1$main |> rename(any_of(rename_main))

MOZ1$loop <- MOZ1$loop |>
  rename_with(~str_replace_all(., "^ind_", ""), matches("wgq")) |>
  mutate(across(matches("wgq"),
                ~str_replace_all(.,
                                 c("No difficulty"="no_difficulty",
                                   "Some difficulty"="some_difficulty",
                                   "A lot of difficulty"="lot_of_difficulty",
                                   "Cannot do at all"="cannot_do",
                                   "Don't know"="dnk",
                                   "Prefer not to answer"="pnta")
                )))

pcode_path <- "data/external/global_pcodes.csv"
pcode_df = rio::import(pcode_path)

# write all environment to load it directly faster
save.image("data/data_loaded_march_25.RData")

# load all environment to skip step 1-2-3
load("data/data_loaded_march_25.RData")

# Step 4: Recoding of WGQ

# Recoding class preparation
rcp <- list.files("data/mapping/recoding_class/", full.names = TRUE)
rc <- rio::import_list(rcp, guess_max = 21474836) |> bind_rows() |>
  dplyr::mutate(
    class = ifelse(is.na(class) | class == "", choice, class),
    class_label = ifelse(is.na(class) | class == "", choice_label, class_label)
  )

rc_non_na <- rc |> filter(!country_choice %in% c("", NA))

## rename with m when !usability %in% c("not_usable", "")

msp <- list.files("data/mapping/mapping_survey/", full.names = TRUE)
m <- map(msp, ~rio::import(., guess_max=2146936) |> mutate(across(any_of("row"), as.numeric))) |> bind_rows()
name_key <- m |>
  filter(column=="name",
         !standard_value %in% c(NA, ""),
         !country_value %in% c(NA, ""),
         !usability %in% c("not_usable", ""))


## for all main dataset, extract uuid and weight
name_wgq <- name_key |> filter(grepl("^wgq_", name))
## add MOZ

country.list <- unique(name_wgq$country) |> append("MOZ1")

## check that for all country.list loop dataset, there is a weight column
lapply(country.list,
       \(c) print(paste0("country ", c, " - ", paste0(str_subset(names(get(c)[["loop"]]), "ind_gender$"), collapse = "; ")))) |>  invisible()

# for ETH1 and SOM1, join the weights
ETH1$loop <- ETH1$loop |> select(-any_of("weight")) |> left_join(ETH1$main |> select(uuid, weight), by=c("uuid.x"="uuid"))
SOM1$loop <- SOM1$loop |> select(-any_of("weight")) |> left_join(SOM1$main |> select(instance_name, weight), by=c("parent_instance_name"="instance_name"))

## rename the weights in the loops
names(KEN1$loop) <- gsub("^weights$", "weight", names(KEN1$loop))
names(KEN2$loop) <- gsub("^weights$", "weight", names(KEN2$loop))
names(MLI1$loop) <- gsub("^weights\\.x$", "weight", names(MLI1$loop))
names(MMR1$loop) <- gsub("^weight_final$", "weight", names(MMR1$loop))
names(UGA1$loop) <- gsub("^weights$", "weight", names(UGA1$loop))
names(MOZ1$loop) <- gsub("^weights$", "weight", names(MOZ1$loop))

## rename ind_age and ind_gender
names(SYR1$loop) <- gsub("^Q5_2_ind_age$", "ind_age", names(SYR1$loop))
names(SYR1$loop) <- gsub("^Q5_1_ind_gender$", "ind_gender", names(SYR1$loop))

## rename hoh_gender and hoh_age?? for all country.list
SYR1$main <- SYR1$main |>
  mutate(resp_hoh_yn=Q3_3_hoh, hoh_gender=Q4_4_hoh_gender, hoh_age=Q4_5_hoh_age, resp_gender=Q3_1_respondent_gender, resp_age=Q3_2_respondent_age)

## check unique values from resp_hoh_yn in all datasets
for(c in country.list) {
  print(paste0("For country ", c, ", unique values of resp_hoh_yn: ", paste0(unique(get(c)$main$resp_hoh_yn), collapse=", ")))
}


## for all, recode if resp_hoh_yn == "yes", then take resp_age and resp_gender for hoh
val.yes <- c("yes", "Yes", "oui")
val.no <- c("no", "No", "non")
for (c in country.list){
  list.df <- get(c)
  list.df$main <- list.df$main |>
    mutate(hoh_age = ifelse(resp_hoh_yn %in% val.yes, resp_age, hoh_age),
           hoh_gender = ifelse(resp_hoh_yn %in% val.yes, resp_gender, hoh_gender))
  assign(c, list.df, .GlobalEnv)
}


## unselect any_of select(-any_of(c("resp_age", "resp_gender"))) in loop dataset of country.list
map(country.list %>% keep(!. %in% "MOZ1"), \(c) {
  list.df <- get(c)
  list.df$loop <- list.df$loop |> select(-any_of(c("resp_age", "resp_gender", "hoh_age", "hoh_gender")))
  assign(c, list.df, .GlobalEnv)
}) |>  invisible()


## for SYR, left_join with main dataset to recover hoh age and gender
SYR1$loop <- SYR1$loop |>
  left_join(SYR1$main |> select(uuid, hoh_gender, hoh_age, resp_gender, resp_age)) %>%
  mutate(ind_age=coalesce(ind_age, hoh_age, resp_age),
         ind_gender=coalesce(ind_gender, hoh_gender, resp_gender))

## for MLI rename uuid.x column as uuid
ETH1$loop <- ETH1$loop |> mutate(uuid.z=uuid, uuid=uuid.x)
MLI1$loop <- MLI1$loop |> rename(uuid=uuid.x)
MMR1$main <- MMR1$main |> rename(uuid=`_uuid`)
MMR1$loop <- MMR1$loop |> rename(uuid=`_uuid`)
SOM1$main <- SOM1$main |> mutate(uuid=instance_name)
SOM1$loop <- SOM1$loop |> mutate(uuid=parent_instance_name)

## check if resp_gender and resp_age hoh_gender and hoh_age are present and less than 10% NA in all main datasets
for (c in country.list %>% keep(!. %in% "MOZ1")){
  # c <- country.list[6]
  list.df <- get(paste0(c))
  if (!all(c("resp_gender", "resp_age", "hoh_age", "hoh_gender") %in% colnames(list.df$main))) print(paste0("not all columns in ", c," dataset"))
  print(paste0(c, "- resp_gender - ", sum(is.na(list.df$main[["resp_gender"]])),"NA // resp_age - ", sum(is.na(list.df$main[["resp_age"]])), " NA"))
  print(paste0(c, "- hoh_gender - ", sum(is.na(list.df$main[["hoh_gender"]])), "NA // hoh_age - ", sum(is.na(list.df$main[["hoh_age"]])), " NA"))
  if (all(c("resp_gender", "resp_age", "hoh_age", "hoh_gender") %in% colnames(list.df$main))) {
    ## if resp_age and resp_gender are not in loop, join them to loop dataset
    if (!all(c("resp_gender", "resp_age", "hoh_age", "hoh_gender") %in% colnames(list.df$loop))) {
      ## print # non matchin uuid in loop with no match in main
      print(paste0("uuid in loop with no match in main: ", sum(!list.df$loop$uuid %in% list.df$main$uuid)))
      list.df$loop <- list.df$loop |>
        left_join(list.df$main |> select(uuid, resp_age, resp_gender, hoh_age, hoh_gender), by="uuid") |>  mutate(across(matches("(resp|hoh)_age"), as.numeric))
      print(paste0("resp_age and resp_gender joined to ", c, " dataset."))
      assign(c, list.df, .GlobalEnv)
    } else (print(paste0("No joining done for country ", c, " as columns present")))
  }

}

## rename wgq and align values
for (c in country.list %>% keep(!. %in% "MOZ1")){
  # c <- country.list[1]
  list.df <- get(c)
  ## align column names
  name_wgq_c <- name_wgq |> filter(country==c)
  names(list.df[["loop"]]) <- str_replace_all(names(list.df[["loop"]]), setNames(name_wgq_c$standard_value, paste0("^", name_wgq_c$country_value, "$")))
  ## align wgq values
  rc_c_wgq <- rc |> filter(country==c, grepl("^wgq_", var_code)) |> select(var_code, choice, country_choice)
  for (var in unique(rc_c_wgq$var_code)){
    # var <- unique(rc_c_wgq$var_code)[1]
    rep_df <- rc_c_wgq %>% filter(var_code==var, !country_choice %in% "")
    rep_pattern <- setNames(rep_df$choice, paste0("\\b", rep_df$country_choice, "\\b"))
    list.df[["loop"]][[var]] <- str_replace_all(list.df[["loop"]][[var]], rep_pattern)
  }
  list.df[["loop"]]$country <- c
  assign(paste0(c,"_new"), list.df)
  rm(list.df)
}
MOZ1_new <- MOZ1
MOZ1_new$main$country <- "MOZ1"
MOZ1_new$loop$country <- "MOZ1"

## get all df with wgq data, right uuid, weight and stratum
col.keep <- unique(name_wgq$name)
col.meta <- c("country", "admin1", "admin2") |> append(vn_df |> select(var, loop) |> unlist() |> unique() |> na.omit()) |>
  append(c("ind_age", "ind_gender", "resp_age", "resp_gender", "hoh_gender", "hoh_age"))

## check str() of resp_age for all country.list
for (c in country.list) {
  str(get(c)$loop$hoh_gender)
  print(paste0(c, " - ", sum(is.na(get(c)$loop$hoh_gender))))
}

## rename for MOZ the additionnal indicator with wgq_ prefix to have it in main dataset at the end
MOZ1_new$loop <- MOZ1_new$loop %>%
  rename_with(~str_replace_all(., c("^ind_dis_challenges"="wgq_ind_dis_challenges")), contains("ind_dis_challenges"))

df_wgq_24 <- map(country.list, ~{
  get(paste0(., "_new"))$loop |>
    select(any_of(c(col.meta, col.keep)), starts_with("wgq_ind_dis_challenges")) |>
    mutate(weight=as.numeric(weight),
           across(contains("age"), as.numeric),
           across(matches("id"), as.character))}) |>
  bind_rows() |>
  mutate(year=2024, .before=1)

## filter non matched uuid / empty weights
# check <- df_wgq_24 |> filter(is.na(weight))
# check %>% count(country)
df_wgq_24_filter <- df_wgq_24 |> filter(!is.na(weight))

## use humind() to compute wgs thresholds

## test the updated add_loop_wgss() from local forked repo with this path for source function:
## C:/Users/raphael.bacot/OneDrive - ACTED/git/humind/R/add_loop_wgq_ss.R
source("C:/Users/raphael.bacot/OneDrive - ACTED/git/humind/R/add_loop_wgq_ss.R")
df_wgq_24_filter <- df_wgq_24_filter %>% add_loop_wgq_ss()
# df_wgq_24_filter <- df_wgq_24_filter %>% humind::add_loop_wgq_ss()

## fix issue with na.rm=T for _n indicators
df_wgq_24_filter <- df_wgq_24_filter %>%
  mutate(
    across(
      all_of(c("wgq_cannot_do_n",
               "wgq_lot_of_difficulty_n",
               "wgq_some_difficulty_n",
               "wgq_no_difficulty_n")),
      ~ case_when(
        rowSums(
          across(
            all_of(c("wgq_vision",
                     "wgq_hearing",
                     "wgq_mobility",
                     "wgq_cognition",
                     "wgq_self_care",
                     "wgq_communication")),
            ~is.na(.)
          )
        ) > 0 ~ NA_real_,
        T ~ .
      )
    )
  )

## align ind_gender categories to have male and female only
## first summarise non aligned genders
check_gender <- df_wgq_24_filter %>% count(country, hoh_gender)

df_wgq_24_final <- df_wgq_24_filter %>%
  mutate(
    across(all_of(c("resp_gender", "ind_gender", "hoh_gender")),
           ~ifelse(
             . %in% c("other", "pnta", "autre", "pnpr", "ind_other", "ind_pna"),
             "undefined",
             str_replace_all(.,
                             c(
                               "^resp_male$"="male",
                               "^resp_female"="female",
                               "^masculin$"="male",
                               "^feminin$"="female",
                               "^Female$"="female",
                               "^Male$"="male",
                               "^ind_male$"="male",
                               "^ind_female$"="female",
                               "^Female / woman$"="female",
                               "^Male / man$"="male",
                               "^Other$"="undefined",
                               "^Prefer not to answer$"="undefined"
                             ))
           )
    )
  )

## save df_wgq_24 dataset
dir.create("output", showWarnings = F)
dir.create("output/data", showWarnings = F)
saveRDS(df_wgq_24_final, file="output/data/df_wgq_24.rds")




### side analysis by context

# ## MOZ
# data_moz <- import("data/offline_dataset/MOZ1.xlsx", sheet=2, skip=1)
#
# ## BGD 23
# dir <- "C:/Users/raphael.bacot/OneDrive - ACTED/x-crisis-23/data/"
# data_bgd_host <- import(paste0(dir, "REACH_MSNA_2023_BGD_hosts_output_dataset_host.csv"))
# data_bgd_refugee <- import(paste0(dir, "REACH_MSNA_2023_BGD_refugees_output_dataset_refugees.csv"))
#



## ind_dis_challenges => services not accessed because of disability => to calculate and disaggregate
## Has member of age: ${ind_age} and gender: ${ind_gender} ever experienced challenges in accessing any of the following services due to his/her impairment?
# Medical care tailored to the specific needs of persons with disabilities.
# Rehabilitation and Physical Therapy Services
# Assistive devices and specialized medical equipment
# Mental Health and Psychosocial Support (MHPSS)
# Lifesaving information in accessible formats (braille, sign language, easy to read materials)
# Protection and GBV services
# Inclusive education
# Evacuation
# Accessible emergency shelters
# Accessible toilets
# Non-food items
# Legal aid services (e.g. Civil documentation, HLP rights)
# None of the above
# Don't know
# Prefer not to answer


