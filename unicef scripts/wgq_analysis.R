# rm(list=ls())

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
library(patchwork)
library(ggplot2)

source("custom raph/utils.R")

## load data
df_wgq_24 <- readRDS("output/data/df_wgq_24.rds")
df_wgq_23 <- readRDS("output/data/df_wgq_23.rds") %>% rename(hoh_gender=hoh_gender_cat)
df_wgq_22 <- readRDS("output/data/df_wgq_22.rds") %>% mutate(year=2022)

## align country names
df_wgq_24 <- df_wgq_24 %>% mutate(country=gsub("\\d", "", country))
df_wgq_23 <- df_wgq_23 %>%
  mutate(pop_group = case_when(country == "LBN-PRL" ~ "PRL",
                               country == "LBN-migrants" ~ "migrants",
                               country == "LBN-lebanese" ~ "hosts",
                               country == "UKR-CCCM" ~ "camp",
                               country == "BGD-hosts" ~ "hosts",
                               country == "BGD-refugees" ~ "refugees",
                               T ~ pop_group_cat),
         country = str_replace_all(country, "-.*", ""))

# bind all datasets
df_wgq <- bind_rows(
  df_wgq_24,
  df_wgq_23,
  df_wgq_22
)

## recalculate with add_loop_wgss for all at once
source("C:/Users/raphael.bacot/OneDrive - ACTED/git/humind/R/add_loop_wgq_ss.R")
source("C:/Users/raphael.bacot/OneDrive - ACTED/git/humind/R/internals.R")

vars <- c("wgq_vision", "wgq_hearing", "wgq_mobility", "wgq_cognition", "wgq_self_care", "wgq_communication")
df_wgq <- df_wgq %>% add_loop_wgq_ss(undefined = c("dnk", "pnta", "undefined"))

## calculate age_cat
df_wgq <- df_wgq %>%
  humind::add_age_cat(age_col = "ind_age", breaks = c(-1, 4, 10, 17, 25, seq.int(30, 90, 10), max(df_wgq[["ind_age"]], na.rm=T))) %>%
  # humind::add_age_cat(age_col = "ind_age", new_colname = "ind_age_cat_wide", breaks = c(0, 4, 17, 34, 64, 120)) %>%
  humind::add_age_cat(age_col ="ind_age", new_colname = "ind_age_cat_wide", breaks = c(-1, 4, 17, 34, 59, 120)) %>%
  humind::add_age_cat(age_col ="ind_age", new_colname = "age_detail", breaks = c(-1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,120)) %>%
  humind::add_age_cat(age_col ="ind_age", new_colname = "age_mds", breaks = c(-1, 17, 25, 35, 45, 55, 120))

# df_wgq$ind_age_cat_wide %>% unique

## Check NAs in wgq vars
df_wgq_na <- df_wgq %>% filter(if_any(any_of(vars), is.na)) %>% filter(ind_age>17)
test <- df_wgq_na %>% count(country, year, ind_age_cat)
# df_wgq <- df_wgq %>% filter(!if_any(any_of(vars), is.na))

## load labels
wgq_lab_2 <- import("custom raph/wgq_labels.xlsx", sheet="recoding_indicator")
wgq_lab <- import("custom raph/wgq_labels.xlsx", sheet="recoding_class") %>% rename(`final id`=`global id`) %>%
  mutate(indicator=`final id`, `final id`=paste0(`final id`, "_", class)) %>%
  left_join(wgq_lab_2 %>% select(indicator=`final id`, indicator_label, `question label`)) %>% filter(indicator %in% vars)
wgq_lab_2 <- wgq_lab_2 %>% filter(!`final id` %in% vars) %>% bind_rows(wgq_lab %>% mutate(choice_label=class_label)) %>%
  select(`final id`, indicator, indicator_label, choice_label)

## check disagregation subset in dataset
# check <- df_wgq %>%
#   mutate(ind_age_check = ifelse(is.na(ind_age), "missing", "valid"),
#          ind_gender_check = ifelse(is.na(ind_gender), "missing", "valid")) %>%
#   count(year, country, ind_age_check, ind_gender_check)

## filter out surveys with either ind_gender or ind_age missing
df_wgq <- df_wgq %>% filter(!is.na(ind_age), !is.na(ind_gender), !ind_gender %in% c("undefined"))

## quickly explore undefined records across vars
df_wgq_undefined <- df_wgq %>%
  group_by(country, year) %>%
  summarise(across(all_of(vars), list(mean = ~weighted.mean(. %in% "undefined", w=weight, na.rm=T),
                                      count = ~sum(. %in% "undefined"))))

## expand child columns from vars
df_wgq <- df_wgq %>%
  select(-matches("\\.")) %>%
  ## get rid of undefined as very few observations only
  mutate(across(all_of(vars), ~ifelse(.=="undefined", NA, .))) %>%
  expand.select.multiple.vec(x = vars)

## create resp_gender_hoh_gender combined var
df_wgq <- df_wgq |>
  mutate(resp_hoh_gender=paste0("resp_", resp_gender, "_hoh_", hoh_gender),
         resp_gender = ifelse(str_detect(resp_gender, "(fe|)male"), paste0(resp_gender, " respondent"), NA))

## combine two proxy resp questions
df_wgq <- df_wgq %>%
  mutate(
    wgq_resp_profile= case_when(
      wgq_proxy_resp=="yes" & wgq_caregiver_proxy %in% c(NA, "no") ~ "self respondent",
      wgq_proxy_resp=="yes_with_assistance" ~ "self respondent assisted",
      wgq_proxy_resp %in% c("no", NA) & wgq_caregiver_proxy %in% c("no") ~ "proxy respondent",
      wgq_proxy_resp %in% c("no") & wgq_caregiver_proxy %in% c(NA) ~ "proxy respondent",
      wgq_proxy_resp %in% c("no", NA) & wgq_caregiver_proxy=="yes" ~ "caregiver respondent",
      wgq_proxy_resp=="yes" & wgq_caregiver_proxy=="yes" ~ "caregiver respondent",
      wgq_proxy_resp %in% c(NA) & wgq_caregiver_proxy %in% c(NA) ~ "NA",
      wgq_proxy_resp %in% c(NA) & wgq_caregiver_proxy %in% c("pnta") ~ "NA",
      wgq_proxy_resp %in% c("no") & wgq_caregiver_proxy %in% c("dnk", "pnta") ~ "proxy respondent",
      T ~ paste0("self resp: ", wgq_proxy_resp, " - caregiver resp: ", wgq_caregiver_proxy)
    )
  )

## write df_wgq data
data.table::fwrite(df_wgq, "output/data/df_wgq_prepared.csv")

## check that no NA hoh_gender
test <- df_wgq |> count(country, year, hoh_gender)

## quick weighted analysis
source("custom raph/utils.R")
vars_analyse <- c("wgq_dis_1", "wgq_dis_2", "wgq_dis_3", "wgq_dis_4", names(df_wgq)[grepl("^wgq_", names(df_wgq))]) %>% unique
var_char <- names(df_wgq)[sapply(df_wgq, is.character)]
var_n <- names(df_wgq)[grepl("_n$", names(df_wgq))]
var_an <- vars_analyse[!vars_analyse %in% c(var_char, "year", "wgq_chronic_disease")]

## list of group var
group <- list(
  c("country", "year"),
  c("country", "year", "ind_gender"),
  c("country", "year", "ind_age_cat_wide"),
  c("country", "year", "ind_age_cat"),
  c("country", "year", "ind_gender", "ind_age_cat_wide"),
  c("country", "year", "ind_gender", "ind_age_cat"),
  c("country", "year", "resp_gender"),
  c("country", "year", "hoh_gender"),
  c("country", "year", "ind_gender", "resp_gender"),
  c("country", "year", "resp_hoh_gender"),
  c("country", "year", "ind_gender", "resp_hoh_gender")
)

## additionnal analysis [respondent type]
group <- list(c("country", "year", "wgq_proxy_resp"),
              c("country", "year", "wgq_caregiver_proxy"),
              c("country", "year", "wgq_resp_profile"))

group_other <- group %>% unlist %>% unique %>% keep(!. %in% c("country", "year"))

res <- list()

exclude <- c("NA", "other", "undefined", "other", "gender_non_conforming") %>% paste0(., collapse="|")

for (g in group){
  res[[paste(g, collapse = "_")]] <- analyse(
    df = df_wgq |> filter(if_all(all_of(g), ~!str_detect(., exclude))),
    group_var = g,
    var = var_an,
    col_weight = "weight"
  )
}

get_other_var <- function(var) {group_other %>% keep(!. %in% c(var))}
res$country_year_ind_gender_resp_gender <- res$country_year_ind_gender_resp_gender %>% filter(!is.na(resp_gender))
res_all <- map(res, \(x) x %>% filter(n!=0)) %>% bind_rows
result_lab <- res_all %>% mutate(question = ifelse(!is.na(choice), paste0(question, "_", choice), question)) %>%
  left_join(wgq_lab_2 %>% select(question=`final id`, indicator, indicator_label, choice_label)) %>%
  relocate(indicator, indicator_label, choice_label, .before="mean")
result_lab_country_year <- result_lab %>% filter(if_all(all_of(get_other_var("")), is.na)) %>%
  relocate(indicator, indicator_label, choice_label, .before="mean")
result_lab_country_year_gender <- result_lab %>% filter(if_all(all_of(get_other_var("ind_gender")), is.na), !is.na(ind_gender)) %>%
  relocate(indicator, indicator_label, choice_label, ind_gender, .before="mean")
result_lab_country_year_ind_age_cat_wide <- result_lab %>% filter(if_all(all_of(get_other_var("ind_age_cat_wide")), is.na), !is.na(ind_age_cat_wide)) %>%
  relocate(indicator, indicator_label, choice_label, ind_age_cat_wide, .before="mean")
result_lab_country_year_ind_age_cat <- result_lab %>% filter(if_all(all_of(get_other_var("ind_age_cat")), is.na), !is.na(ind_age_cat)) %>%
  relocate(indicator, indicator_label, choice_label, ind_age_cat, .before="mean")
result_lab_country_year_ind_gender_age <- result_lab %>% filter(if_all(all_of(get_other_var(c("ind_gender", "ind_age_cat_wide"))), is.na), !is.na(ind_gender), !is.na(ind_age_cat_wide)) %>%
  relocate(indicator, indicator_label, choice_label, ind_gender, ind_age_cat_wide, .before="mean")
result_lab_country_year_ind_gender_age_det <- result_lab %>% filter(if_all(all_of(get_other_var(c("ind_gender", "ind_age_cat"))), is.na), !is.na(ind_gender), !is.na(ind_age_cat)) %>%
  relocate(indicator, indicator_label, choice_label, ind_gender, ind_age_cat, .before="mean")
result_lab_country_year_resp_gender <- result_lab %>% filter(if_all(all_of(get_other_var("resp_gender")),is.na),!is.na(resp_gender)) %>%
  relocate(indicator, indicator_label, choice_label, resp_gender, .before="mean")
result_lab_country_year_ind_resp_gender <- result_lab %>% filter(if_all(all_of(get_other_var(c("ind_gender", "resp_gender"))), is.na), !is.na(ind_gender), !is.na(resp_gender)) %>%
  relocate(indicator, indicator_label, choice_label, resp_gender, ind_gender, .before="mean")
result_lab_country_year_resp_hoh_gender <- result_lab %>% filter(if_all(all_of(get_other_var("resp_hoh_gender")),is.na), !is.na(resp_hoh_gender)) %>%
  relocate(indicator, indicator_label, choice_label, resp_hoh_gender, .before="mean")
result_lab_country_year_ind_gender_hoh_gender <- result_lab %>% filter(if_all(all_of(get_other_var(c("ind_gender", "resp_hoh_gender"))), is.na), !is.na(ind_gender), !is.na(resp_hoh_gender)) %>%
  relocate(indicator, indicator_label, choice_label, ind_gender, resp_hoh_gender, .before="mean")
result_lab_country_year_hoh_gender <- result_lab %>% filter(if_all(all_of(get_other_var("hoh_gender")), is.na), !is.na(hoh_gender)) %>%
  relocate(indicator, indicator_label, choice_label, hoh_gender, .before="mean") |> filter(!hoh_gender %in% c("gender_non_conforming"))

## only for proxy resp disag
# res_proxy_resp <- res$country_year_wgq_proxy_resp %>% mutate(question = ifelse(!is.na(choice), paste0(question, "_", choice), question)) %>%
#   left_join(wgq_lab_2 %>% select(question=`final id`, indicator, indicator_label, choice_label))
# res_caregiver_resp <- res$country_year_wgq_caregiver_proxy %>% mutate(question = ifelse(!is.na(choice), paste0(question, "_", choice), question)) %>%
#   left_join(wgq_lab_2 %>% select(question=`final id`, indicator, indicator_label, choice_label))
# res_resp_profile <- res$country_year_wgq_resp_profile %>% mutate(question = ifelse(!is.na(choice), paste0(question, "_", choice), question)) %>%
#   left_join(wgq_lab_2 %>% select(question=`final id`, indicator, indicator_label, choice_label))

#
# ## write the proxy and caregiver
# write_xlsx(list(
#   res_proxy_resp = res_proxy_resp,
#   res_caregiver_resp = res_caregiver_resp,
#   res_resp_profile = res_resp_profile
#   ), "output/result/result_wgq_proxy_resp_caregiver.xlsx")

## list dataframe
list.res.name <- ls(pattern = "result_lab_country_year")
list.res <- lapply(list.res.name, \(x) get(x) %>% ungroup) %>% setNames(str_replace_all(list.res.name, "result_lab_country_year_", ""))

## write results
dir.create("output/result", showWarnings = F)
writexl::write_xlsx(list.res, "output/result/result_wgq_all.xlsx")

################################################################################
## read all results [if you don't want to rerun all]
list.res <- rio::import_list("output/result/result_wgq_all.xlsx")
names(list.res)[-1] <- paste0("result_lab_country_year_", names(list.res)[-1])
list2env(list.res, envir = .GlobalEnv)

# ## import list.proxy.res
# list.proxy.res <- rio::import_list("output/result/result_wgq_proxy_resp_caregiver.xlsx")
# list2env(list.proxy.res, envir = .GlobalEnv)
#
# ## write all together (append to elements to the list)
# str(list.res)
# str(list.proxy.res)
#
# combined_list <- c(list.res, list.proxy.res)
# names(combined_list) <- str_replace_all(names(combined_list), c("res_"="result_lab_country_"))
# names(combined_list) <- str_replace_all(names(combined_list), c("result_lab_country_"=""))

## write this updated version
# writexl::write_xlsx(combined_list, "output/result/result_wgq_all_proxy.xlsx")

## LEBANON and MALI case studies
# source("custom raph/wgq_mli_lbn.R")

## plot pyramid age
source("C:/Users/raphael.bacot/OneDrive - ACTED/git/aap_etc/archive humind/plot_age_pyramid.R")


## get prevalence by country year for all wgq_dis
prev_wgq <- list.res$result_lab_country_year %>% filter(str_detect(question, "wgq_dis_")) %>%
  mutate(text=paste0(country, " - ", year, "\nprevalence: ", scales::percent(mean, accuracy = 0.1), "\n count:", count, " - N:", n)) %>%
  select(country, year, ind=question, text)

## aggregate age and gender
df_wgq_dis_1 <- df_wgq %>% filter(wgq_dis_1==1)
df_wgq_dis_2 <- df_wgq %>% filter(wgq_dis_2==1)
df_wgq_dis_3 <- df_wgq %>% filter(wgq_dis_3==1)
df_wgq_dis_4 <- df_wgq %>% filter(wgq_dis_4==1)

agreggate_age_def <- function(df,...) {df %>%
  aggregate_age(var_age_cat = "ind_age_cat", var_gender = "ind_gender", group_var = c("country", "year"), col_weight = "weight")}

age_gender_table <- df_wgq %>% agreggate_age_def %>% left_join(prev_wgq %>% filter(ind=="wgq_dis_3") %>% mutate(text=str_replace_all(text, "prevalence", "prevalence wg-ss 3")))
age_gender_table_dis_4 <- df_wgq_dis_4 %>% agreggate_age_def %>% mutate(ind="wgq_dis_4") %>% left_join(prev_wgq)
age_gender_table_dis_3 <- df_wgq_dis_3 %>% agreggate_age_def %>% mutate(ind="wgq_dis_3") %>% left_join(prev_wgq)
age_gender_table_dis_2 <- df_wgq_dis_2 %>% agreggate_age_def %>% mutate(ind="wgq_dis_2") %>% left_join(prev_wgq)
age_gender_table_dis_1 <- df_wgq_dis_1 %>% agreggate_age_def %>% mutate(ind="wgq_dis_1") %>% left_join(prev_wgq)

## plot age gender pyramid
## redefine plot_age_pyramid default argument values
require(pacman)
p_load(ggplot2)
plot_age_pyramid_def <- function(df, ...) {
  df %>% plot_age_pyramid(var_gender = "ind_gender", var_age_cat = "ind_age_cat", col_stat = "prop", col_n_unw = "n",
                          unit = "Individual", save=F, ...)
}

plot.dis.p <- age_gender_table %>% plot_age_pyramid_def(title="Individual age gender pyramid\nwhole population", group_var = c("text"), n_col=5)
plot.dis.p.clean <- age_gender_table %>% plot_age_pyramid_def(title="Individual age gender pyramid\nwhole population", group_var = c("country", "year"), n_col=5)
plot.dis.1 <- age_gender_table_dis_1 %>% plot_age_pyramid_def(title="Individual age gender pyramid\nPopulation with WG-SS 1", group_var = c("text"), n_col=5)
plot.dis.2 <- age_gender_table_dis_2 %>% plot_age_pyramid_def(title="Individual age gender pyramid\nPopulation with WG-SS 2", group_var = c("text"), n_col=5)
plot.dis.3 <- age_gender_table_dis_3 %>% plot_age_pyramid_def(title="Individual age gender pyramid\nPopulation with WG-SS 3", group_var = c("text"), n_col=5)
plot.dis.4 <- age_gender_table_dis_4 %>% plot_age_pyramid_def(title="Individual age gender pyramid\nPopulation with WG-SS 4", group_var = c("text"), n_col=5)

## save all graphs in output/graph
dir.create("output/graph_wgq", showWarnings = F)
ggsave("output/graph_wgq/plot_dis_p_ppt.png", plot.dis.p.clean, width=12, height=10, bg="white")
ggsave("output/graph_wgq/plot_dis_p_det_ppt.png", plot.dis.p, width=13, height=12, bg="white")
ggsave("output/graph_wgq/plot_dis_1_ppt.png", plot.dis.1, width=13, height=12, bg="white")
ggsave("output/graph_wgq/plot_dis_2_ppt.png", plot.dis.2, width=13, height=12, bg="white")
ggsave("output/graph_wgq/plot_dis_3_ppt.png", plot.dis.3, width=13, height=12, bg="white")
ggsave("output/graph_wgq/plot_dis_4_ppt.png", plot.dis.4, width=13, height=12, bg="white")

## Only MLI
plot.dis.p.mli <- age_gender_table %>% filter(country=="MLI") %>% plot_age_pyramid_def(title="Whole population individual age gender pyramid", group_var = c("country", "year"), n_col=3)
plot.dis.3.mli <- age_gender_table_dis_3 %>% filter(country=="MLI") %>% plot_age_pyramid_def(title="Population with severity WG-SS 3", group_var = c("text"), n_col=3)
plot.mli.final <- plot.dis.p.mli / plot.dis.3.mli + plot_annotation(title = "Case study - Washingtown group questions\nMALI",
                                                                    theme = theme(plot.title = element_text(hjust = 0.513, size=25)))
plot.mli.final
ggsave("output/graph_wgq/plot_mli_final.png", plot.mli.final, width=12, height=12, bg="white")
ggsave("output/graph_wgq/plot_dis_3_mli.png", plot.dis.3.mli, width=8, height=4, bg="white")
ggsave("output/graph_wgq/plot_dis_p_mli.png", plot.dis.p.mli, width=8, height=4, bg="white")

## only CAR
plot.dis.p.car <- age_gender_table %>% filter(country=="CAR") %>% plot_age_pyramid_def(title="Whole population individual age gender pyramid", group_var = c("country", "year"), n_col=3)
plot.dis.3.car <- age_gender_table_dis_3 %>% filter(country=="CAR") %>% plot_age_pyramid_def(title="Population with severity WG-SS 3", group_var = c("text"), n_col=3)
plot.car.final <- plot.dis.p.car + theme(legend.position = "none") + plot.dis.3.car +
  plot_annotation(title = "", theme = theme(plot.title = element_text(hjust = 0.513, size=25)))
## delete a folder
ggsave("output/graph_wgq/case studies/plot_car_final.png", plot.car.final, width=10, height=4, bg="white")

### Prevalence plotted by different sev/dim/disag var
## Generate graph for all
dir.create("output/graph_wgq/detail", showWarnings = F)

## only for resp
# all.countries <- res_caregiver_resp$country %>% unique
# all.ind <- res_caregiver_resp$indicator %>% unique %>% str_subset("_(n|d)$", negate=T)

all.ind <- result_lab_country_year$indicator %>% unique %>% str_subset("_(n|d)$", negate=T)
all.countries <- result_lab_country_year$country %>% unique

for (ind in all.ind[1]){
  # p <- plot.c(data = result_lab_country_year %>% only_diff, var=ind)
  # ggsave(paste0("output/graph_wgq/detail/", ind, ".png"), p, height=10, width=15, bg = "white")
  # p <- plot.c.disag(data = result_lab_country_year_gender %>% only_diff, var=ind, col.disag="ind_gender", sub_lab="Disaggregated by gender")
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_gender", ".png"), p, height=12, width=15, bg = "white")
  # p <- plot.c.disag(data = result_lab_country_year_ind_age_cat_wide %>% only_diff,
  #                   var=ind, col.disag="ind_age_cat_wide", sub_lab="Disaggregated by age")
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_age", ".png"), p, height=15, width=15, bg = "white")
  # p <- plot.c.disag(data = result_lab_country_year_ind_resp_gender %>% only_diff,
  #                   var=ind, col.disag=c("ind_gender", "resp_gender"), sub_lab="Disaggregated by gender", n.col = 6)
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_gender_resp", ".png"), p, height=16, width=17, bg = "white")
  #
  # p_1 <- plot.c.disag(data = result_lab_country_year_ind_gender_hoh_gender %>% only_diff %>% filter(country %in% all.countries[1:8]),
  #                     var=ind, col.disag=c("ind_gender", "resp_hoh_gender"), sub_lab="Disaggregated by gender resp/HoH", n.col = 8)
  # p_2 <- plot.c.disag(data = result_lab_country_year_ind_gender_hoh_gender %>% only_diff %>% filter(country %in% all.countries[9:15]),
  #                     var=ind, col.disag=c("ind_gender", "resp_hoh_gender"), sub_lab="Disaggregated by gender resp/HoH", n.col = 8)
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_gender_resp_hoh_1", ".png"), p_1, height=16, width=18, bg = "white")
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_gender_resp_hoh_2", ".png"), p_2, height=16, width=18, bg = "white")
  # p <- plot.c.disag(data = result_lab_country_year_resp_gender %>% only_diff, var=ind, col.disag=c("resp_gender"),sub_lab="Disaggregated by respondent gender", n.col=6)
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_resp_gender", ".png"), p, height=16, width=17, bg = "white")
  #  # same with result_lab_country_year_hoh_gender
  # p <- plot.c.disag(data = result_lab_country_year_hoh_gender %>% only_diff, var=ind, col.disag=c("hoh_gender"), sub_lab = "Disaggregated by HoH gender", n.col=6)
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_hoh_gender",".png"), p, height=16, width=17, bg = "white")
  # p <- plot.c.disag(data = res_caregiver_resp %>% only_diff %>% mutate(caregiver_respondent = wgq_caregiver_proxy) %>%
  #                     filter(str_detect(country, "BGD|CAR|DRC")),
  #                   var=ind, col.disag=c("caregiver_respondent"), sub_lab="Disaggregated by caregiver proxy", n.col=3)
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_caregiver_proxy", ".png"), p, height=5, width=10, bg = "white")
  # p <- plot.c.disag(data = res_proxy_resp %>% only_diff %>% mutate(self_respondent=wgq_proxy_resp) %>%
  #                     filter(!str_detect(country, "MLI")),
  #                   var=ind, col.disag=c("self_respondent"), sub_lab="Disaggregated by self vs proxy respondent", n.col=3)
  # ggsave(paste0("output/graph_wgq/detail/", ind, "_proxy_resp", ".png"), p, height=7, width=10, bg = "white")
  p <- plot.c.disag(data = res_resp_profile %>% only_diff %>% mutate(respondent_profile=wgq_resp_profile) %>%
                      filter(!str_detect(country, "MLI")),
                    var=ind, col.disag=c("respondent_profile"), sub_lab="Disaggregated by WG respondent profile", n.col=3)
  ggsave(paste0("output/graph_wgq/detail/", ind, "_resp_profile", ".png"), p, height=7, width=10, bg = "white")
}

## very quickly get graph of % proxy vs ind
exclude.c <- c("MLI")

df_wgq.exp <- df_wgq %>% filter(!country %in% exclude.c) %>%
  # mutate(across(c("wgq_caregiver_proxy", "wgq_proxy_resp"), ~ifelse(is.na(.), "NA", .))) %>%
  expand.select.multiple.vec(c("wgq_caregiver_proxy", "wgq_proxy_resp"))
var.proxy <- colnames(df_wgq.exp) %>% str_subset("proxy") %>% str_subset("\\.")
sum.proxy <- df_wgq.exp %>%
  filter(ind_age>=5) %>%
  group_by(country, year) %>% filter(!(length(unique(wgq_caregiver_proxy))==1 | length(unique(wgq_proxy_resp))==1)) %>% ungroup %>%
  analyse(group_var = c("country", "year"),
          var = var.proxy,
          col_weight = "weight",
          col_strata = NULL) %>%
  filter(mean!=0) %>% filter(!(choice=="NA" & mean==1)) %>%
  mutate(
    question_label=case_when(question=="wgq_caregiver_proxy" ~ "Caregiver reporting",
                             question=="wgq_proxy_resp" ~ "Self-reporting"),
    choice_label=str_replace_all(choice, c("_"=" "))
  )

sum.proxy.cross <- df_wgq %>% filter(!country %in% exclude.c) %>%
  group_by(country, year) %>%
  filter(!(length(unique(wgq_caregiver_proxy))==1 | length(unique(wgq_proxy_resp))==1)) %>%
  filter(ind_age>=5) %>%
  count(wgq_resp_profile, wt = weight) %>%
  mutate(prop=n/sum(n)) %>% ungroup %>%
  mutate(country_year=paste0(country,"\n", year))

plots.resp <- map(unique(sum.proxy.cross$country), \(x){
      ggplot(sum.proxy.cross %>% filter(country==x),
             aes(x=reorder(wgq_resp_profile, prop), y=prop, fill=wgq_resp_profile)) +
        geom_bar(position = "dodge", stat = "identity", fill="#0067A9") +
        geom_text(aes(label=paste0(round(100*prop, 0), " %")), hjust=-.2, size=2.8) +
        scale_y_continuous(labels=scales::percent_format(), limits=c(0,1.09)) +
        theme_minimal() + theme(plot.title=element_text(hjust=0.5)) +
        labs(title=x, x="", y="") +
        coord_flip()
    }) %>%
  patchwork::wrap_plots(ncol=2) +
  patchwork::plot_annotation(title="Washingtown group - Respondents' profile",
                             subtitle = "Combined questions on WG-SS respondent type",
                             theme=theme(plot.title=element_text(hjust=0.5),
                                         plot.subtitle = element_text(hjust=0.5),
                                         plot.caption = element_text(hjust=0)),
                             caption=str_wrap("Note: Questions pertaining to respondent profile have been only asked to the following countries in 2023 MSNAs. Data has been filtered to keep only individuals aged 5 and above for which Washington group questions has been asked. When the respondent profile is reported as NA, it is due to entries dropped during data cleaning.", 140))

ggsave("output/graph_wgq/resp_profile_wgq.png", plots.resp, width=8, height=6, units="in")

plot.sep <- map(c("wgq_proxy_resp", "wgq_caregiver_proxy"),
  \(x){
    lab <- if (x=="wgq_proxy_resp") "Among all adults (except BGD & UKR)" else "Among children or when individual not present"
    plot.c(data = sum.proxy %>%
             mutate(label=lab, choice_label=factor(choice_label, levels=rev(c("yes", "yes with assistance", "no", "NA", "dnk", "pnta")),ordered = T)),
           custom_margin = 0.15,
           col_question = "question", col_label = "question_label", flip = T, factor=T, col_choice_lab = "choice_label", n.col = 2, longitudinal = T,
           var = x) +
      theme(plot.subtitle = element_text(hjust=0.5)) +
      scale_y_continuous(labels=scales::percent_format(), limits=c(0,1.2), breaks =  seq(0, 1, by = 0.25))
  }
    ) %>%
  wrap_plots() +
  patchwork::plot_annotation(title="Washingtown group - Respondents' profile", subtitle = "",
                             theme=theme(plot.title=element_text(hjust=0.5, size=17),
                                         plot.subtitle = element_text(hjust=0.5), plot.caption = element_text(hjust=0)),
                             caption=str_wrap("Note: Questions pertaining to respondent profile have been only asked to the following countries in 2023 MSNAs. Data has been filtered to keep only individuals aged 5 and above for which Washington group questions has been asked. In the case of BGD and UKR, self-reporting question was mistakenly asked to all individual aged 5 and above. No percentages are shown for the choice 'yes with assistance' for countries where the response choice was not available", 155))

ggsave("output/graph_wgq/resp_profile_wgq_sep.png", plot.sep, width=9, height=6, units="in")

## plot general prevalence WG-SS 1/2/3/4
prevalence <- result_lab_country_year %>% filter(str_detect(question, "wgq_dis")) %>%
  mutate(lab_graph=paste0(indicator_label, "\n", choice_label), facet=paste0(country, "\n", year), country_col=country) %>%
  group_by(country, year) %>% arrange(desc(question)) %>%
  mutate(mean_diff = mean - lag(mean, default = 0), .after="mean") %>% ## do define height of the next bar only take the change in prevalence from one severity to the next one
  ungroup


# iterate over wgq_dis_1/2/3/4
for (ind in c("wgq_dis_1", "wgq_dis_2", "wgq_dis_3", "wgq_dis_4")){
  if (ind =="wgq_dis_4") custom_marg <- 0.001 else if (ind =="wgq_dis_3") custom_marg <- 0.01 else if (ind=="wgq_dis_2") custom_marg <- 0.02 else custom_marg <- 0.04
  p <- plot.c(var=ind, data = prevalence, col_question="question", col_choice_lab = "country", col_label = "lab_graph", facet="year", flip=T, custom_margin = custom_marg,
              longitudinal = F, n.col = 3)
  ggsave(paste0("output/graph_wgq/prevalence_", ind ,"_year.png"), p, height=8, width=11, bg = "white")
}

## Do a stacked plot with wgq_1/2/3/4 for all year
c.order <- prevalence %>% filter(question=="wgq_dis_3") %>% arrange(mean) %>% pull(country) %>% unique
c.y.order <- prevalence %>% filter(question=="wgq_dis_3") %>% arrange(mean) %>% pull(facet) %>% unique

graph <- graph_prevalence(prevalence, facet=T)
graph.all.4 <- graph_prevalence(df=prevalence %>% filter(question %in% c("wgq_dis_4")), facet=F)
graph.all.3 <- graph_prevalence(prevalence %>% filter(question %in% c("wgq_dis_3", "wgq_dis_4")), facet=F)
graph.all.2 <- graph_prevalence(prevalence %>% filter(question %in% c("wgq_dis_2", "wgq_dis_3", "wgq_dis_4")), facet=F)
graph.all.1 <- graph_prevalence(prevalence, facet=F)



# c.y.order <- c.y.order %>% sort(decreasing = T)
# graph.h <- graph_prevalence(prevalence, facet=F)
# graph.h.change <- graph_prevalence(prevalence %>% group_by(country, choice_label) %>% filter(n()>1), facet=F)

## save it all
ggsave("output/graph_wgq/prevalence_all_year_wgq3_sorting.png", graph, height=7, width=13, bg = "white")
ggsave("output/graph_wgq/prevalence_all_year_4_wgq3_sorting.png", graph.all.4, height=7.5, width=10, bg = "white")
ggsave("output/graph_wgq/prevalence_all_year_3_wgq3_sorting.png", graph.all.3, height=7.5, width=10, bg = "white")
ggsave("output/graph_wgq/prevalence_all_year_2_wgq3_sorting.png", graph.all.2, height=7.5, width=10, bg = "white")
ggsave("output/graph_wgq/prevalence_all_year_1_wgq3_sorting.png", graph.all.1, height=7.5, width=10, bg = "white")
# ggsave("output/graph_wgq/prevalence_all_year_h.png", graph.h, height=7, width=13, bg = "white")
# ggsave("output/graph_wgq/prevalence_all_year_h_change.png", graph.h.change, height=7, width=13, bg = "white")

## just plot.c.dis with question == wgq_dis_3 and ind_age_cat_wide
plot.age <- plot.c.disag(data = result_lab_country_year_ind_age_cat_wide,
                         col_choice_lab="choice_label",
                         col_question = "question", var = "wgq_dis_3", col_label = "choice_label", col.disag = "ind_age_cat_wide",
                         add.plot = "labs(title='WG-SS severity 3 - by age', fill='Age category', x='')", flip=F) +
  scale_x_discrete(labels= \(x) str_wrap(gsub("3 - ", "", x), 30))
## save this quickly
ggsave("output/graph_wgq/prevalence_wgq_dis_3_age.png", plot.age, height=9, width=12, bg = "white")

## Do a scatter plot of prevalence to see if there is a linear relationship between wgq_dis_1 / wgq_dis_2 / wgq_dis_3 / wgq_dis_4 within/across countries
## split prevalence in two by average mean of each country_col
countries.r <- prevalence %>% group_by(country_col) %>% mutate(mean.c=mean(mean)) %>% arrange(desc(mean.c)) %>% pull(country_col) %>% unique
df_scatter <- prevalence %>% mutate(country_col=factor(country_col, levels=countries.r), choice_label=str_wrap(choice_label, 40))

scatter_1 <- df_scatter %>% filter(country_col %in% countries.r[1:5]) %>% plot.scatter
scatter_2 <- df_scatter %>% filter(country_col %in% countries.r[6:10]) %>% plot.scatter
scatter_3 <- df_scatter %>% filter(country_col %in% countries.r[11:16]) %>% plot.scatter

graph_all <- scatter_1 + labs(title="", subtitle="") +
  scatter_2 + labs(title="", subtitle="") +
  scatter_3 + labs(title="", subtitle="") +
  patchwork::plot_annotation(
    title = "Prevalence of disability by severity level for each country/year",
    subtitle = "Scatter plot of prevalence grouped by country to identify relationship between prevalence and severity level, for each country",
    caption = "For each country a second degree polynomial regression line is fitted to identify relationship between prevalence at each severity level. Each point represents a prevalence for a given severity in a context for one year.",
    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5),
                  plot.subtitle = element_text(size = 15, hjust=0.5),
                  plot.caption = element_text(hjust=0))
  )

ggsave("output/graph_wgq/scatter_prevalence_all.png", graph_all, height=10, width=15, bg = "white")

## save both
ggsave("output/graph_wgq/scatter_prevalence_1.png", scatter_1, height=10, width=15, bg = "white")
ggsave("output/graph_wgq/scatter_prevalence_2.png", scatter_2, height=10, width=15, bg = "white")
ggsave("output/graph_wgq/scatter_prevalence_3.png", scatter_3, height=10, width=15, bg = "white")


### MLI case study
df_mli <- result_lab_country_year %>% filter(country=="MLI") %>% mutate(choice_label=str_wrap(choice_label, 30))
p.mli <- plot.c(data = df_mli, var="wgq_dis", flip = T, custom_margin = 0.01)
p.mli
ggsave("output/graph_wgq/prevalence_mli.png", p.mli, height=4, width=13, bg = "white")

### do a line plot instead
p_load_gh("marcmenem/ggshadow")
plot.mli.line <- df_mli %>% filter(indicator=="wgq_dis") %>%
  ggplot(aes(x=year, y=mean, group=choice_label, color=choice_label)) +
  geom_shadowline() +
  geom_area(aes(y=mean, group=choice_label, fill=choice_label), alpha=0.9, position = "identity") +
  scale_color_manual(values = c("#C7C8CA", "#58585A", "#F3BEBD", "#F27D7C", "#EE5859")) +
  scale_fill_manual(values = c("#C7C8CA", "#58585A", "#F3BEBD", "#F27D7C", "#EE5859")) +
  scale_x_continuous(labels=c(2022, 2023, 2024), breaks=c(2022, 2023, 2024)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks=seq.int(0,10,2)*0.01) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), plot.caption=element_text(hjust=0),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size=8), axis.text.x = element_text(size=8), axis.text.y = element_text(size=8)) +
  labs(x="\nYear", y="% individuals\n", fill="", color="", title="Mali WG-SS prevalence rate", subtitle = "By severity level",
       caption=paste0(
         "In Mali, data collection of WG-SS questions evolved through MSNAs:\n",
         str_wrap("In 2022, respondent was asked at household level if there is at least one person with any difficulty in any dimension, then when relevant ask at individual level the difficulty level for all dimensions. Only adults were asked the set of questions.", 130),
         "\n",
         str_wrap("In 2023 respondent was asked at individual level if individual had any difficulty in any dimension, then when relevant ask at individual level the difficulty level for all dimensions.", 130),
         "\n",
         "In 2024 respondent was asked at individual the difficulty level for all dimensions."
       ))
plot.mli.line
ggsave("output/graph_wgq/prevalence_mli_line.png", plot.mli.line, height=8, bg = "white")

#### Final check for resp_gender vs hoh_gender vs ind_gender
## do a OLS (general first, then one by country), with country fixed effect and interaction term for ind_genderxresp_gender controlling for hoh_gender

### pooled OLS
df_wgq_model <- df_wgq |> mutate(ind_age_sqr=ind_age*ind_age) |>
  filter((!str_detect(resp_gender,"gender_non_conforming|NA|undefined|other") & !str_detect(hoh_gender,"gender_non_conforming|NA|undefined|other")) | country=="NER") |>
  mutate(women_resp_male_hoh_women_ind = ifelse(resp_gender=="female respondent" & hoh_gender == "male" & ind_gender=="female", 1, 0),
         women_resp_ind_gender = paste0(resp_gender, "_", ind_gender),
         population_group=coalesce(pop_group, pop_group_cat),
         caregiver_proxy=ifelse(wgq_caregiver_proxy %in% c("pnta", "dnk"), NA, wgq_caregiver_proxy),
         proxy_resp=ifelse(wgq_proxy_resp %in% "", NA, wgq_proxy_resp),
         wgq_resp_profile = ifelse(wgq_resp_profile =="NA", NA, wgq_resp_profile) %>% paste0(., "-", country))

df_wgq_model_prox <- df_wgq_model %>% filter(country %in% c("UKR", "CAR", "BGD", "DRC", "NER", "MLI"), !str_detect(wgq_resp_profile, "NA"))

# test <- df_wgq_model_prox %>%
#   group_by(country, year, wgq_resp_profile) %>%
#   count(wgq_dis_3,wt = weight) %>%
#   mutate(prop=n/sum(n)) %>%
#   filter(wgq_dis_3==1)

predictors <- c(
  "ind_gender",
  "ind_age",
  "ind_age_sqr",
  "resp_gender",
  "resp_age",
  # "wgq_resp_profile",
  "hoh_gender",
  "year",
  # "population_group",
  "country"
)

m_4 <- lm(as.formula(paste0("wgq_dis_4 ~ ", paste0(predictors, collapse=" + "))), data = df_wgq_model)
m_3 <- lm(as.formula(paste0("wgq_dis_3 ~ ", paste0(predictors, collapse=" + "))), data = df_wgq_model)
m_2 <- lm(as.formula(paste0("wgq_dis_2 ~ ", paste0(predictors, collapse=" + "))), data = df_wgq_model)
m_1 <- lm(as.formula(paste0("wgq_dis_1 ~ ", paste0(predictors, collapse=" + "))), data = df_wgq_model)

pacman::p_load(stargazer)
model_table <- stargazer::stargazer(m_1, m_2,m_3, m_4, type="text", title="Pooled OLS model for WG-SS severity",
                                    subtitle="Coefficient are average increase/decrease in predicted probability of being disabled with corresponding severity level",
                                    align=TRUE, single.row=TRUE, header=FALSE,
                                    out="output/result/regression_OLS_pooled.html",
                                    dep.var.labels = rep("WG-SS severity level",4),
                                    dep.var.labels.include = TRUE)


#### QUICK quality check of the data !!
## filter year and countries for which there is data collected on disability for age <5
summary_below_5 <- df_wgq %>% filter(ind_age<5) %>% group_by(year, country) %>%
  summarise(
    across(starts_with(paste0(vars, ".")), ~weighted.mean(., w=weight, na.rm=T)),
    n=n()
  ) %>%
  ungroup %>% arrange(year, country) %>%
  filter(!is.nan(wgq_vision.pnta)) %>% pivot_longer(-c(country, year))

## plot proportion WG-SS and age as geom_line
keep.last.c.year <- function(df){df %>% group_by(country) %>% filter(year==max(year)) %>% ungroup}

df.plot.age.dim <- df_wgq %>% keep.last.c.year %>% filter(ind_age >=5) %>%
  group_by(ind_age) %>% summarise(across(starts_with(paste0(vars, ".")), ~weighted.mean(., w = weight, na.rm=T)), n=n()) %>% filter(ind_age<95) %>%
  rename_with(~str_replace_all(., c("\\.some"=".1_some", "\\.lot"=".2_lot", "\\.cannot"=".3_cannot")), contains("."))

df.plot.age.dim.uga <- df_wgq %>% keep.last.c.year %>% filter(country=="UGA", ind_age >=5) %>%
  group_by(ind_age) %>% summarise(across(starts_with(paste0(vars, ".")), ~weighted.mean(., w = weight, na.rm=T)), n=n()) %>% filter(ind_age<95) %>%
  rename_with(~str_replace_all(., c("\\.some"=".1_some", "\\.lot"=".2_lot", "\\.cannot"=".3_cannot")), contains("."))


df.plot.age <- df_wgq %>% keep.last.c.year %>% group_by(ind_age) %>%
  summarise(across(matches("wgq_dis_(\\d)"), ~weighted.mean(., w = weight, na.rm=T)), n=n()) %>%
  filter(ind_age<95)
df.plot.age.c <- df_wgq %>% keep.last.c.year %>% group_by(country, ind_age) %>%
  summarise(across(matches("wgq_dis_(\\d)"), ~weighted.mean(., w = weight, na.rm=T)), n=n()) %>%
  group_by(country) %>% filter(ind_age<quantile(ind_age, .90))
df.plot.age.c.y <- df_wgq %>% group_by(country=paste0(country, "\n", year), ind_age) %>%
  summarise(across(matches("wgq_dis_(\\d)"), ~weighted.mean(., w = weight, na.rm=T)), n=n()) %>%
  group_by(country) %>% filter(ind_age<quantile(ind_age, .90))


plot.age.dis <- function(df, group=NULL, var="wgq_dis", filtered.val=c(""), smooth=T, w.wrap=180){
  list.col <- c("#C7C8CA", "#58585A", "#F3BEBD", "#F27D7C", "#EE5859")
  plot <- df %>%
    pivot_longer(starts_with(var)) %>%
    filter(!name %in% filtered.val) %>%
    ggplot(aes(x=ind_age, y=value, fill=name))

  if (smooth) plot <- plot +
    stat_smooth(geom = "area", method = "gam", se = FALSE, alpha = 0.9, aes(fill=name), position = "identity", show.legend = T)

  if (!smooth) plot <- plot +
    geom_area(aes(y=value, group=name, fill=name), alpha=0.9, position = "identity")

  plot <- plot +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = list.col, labels = \(x) str_replace_all(x, c("_"=" ", "wgq dis"="WG-SS"))) +
    scale_fill_manual(values = list.col, labels = \(x) str_replace_all(x, c("_"=" ", "wgq dis"="WG-SS"))) +
    labs(x="Individual age", y="% of individuals",
         fill="Severity level",
         caption=str_wrap("Data coming from MSNA datasets collected in 2022-2024 in 17 countries. For countries with multiple MSNA conducted, the most recent dataset has been kept.", w.wrap),
         title="Prevalence of disability",
         subtitle="by age and WG-SS severity level") +
    theme_minimal() +
    theme(plot.title=element_text(hjust=0.5),
          # plot.caption=element_text(hjust=0),
          plot.subtitle=element_text(hjust=0.5),
          legend.position = "bottom", )

  if (smooth) plot <- plot +
    ## extract existing caption and add "\nPrevalence trends were smoothed using a Generalized Additive Model (GAM), allowing flexible, data-driven curves to capture non-linear patterns while avoiding overfitting
    labs(
      caption = paste0(plot$labels$caption, "\n",
                       str_wrap("To account for noise in data, prevalence is smoothed using a Generalized Additive Model (GAM) to capture non-linear patterns while avoiding overfitting.", w.wrap))
    )

  if (!is.null(group)) {
    plot <- plot +
      facet_wrap(as.formula(paste0("~", group)))
  }
  return(plot)
}

p1 <- plot.age.dis(df.plot.age) +
  theme(plot.caption = element_text(size=8), plot.title=element_text(size=18, hjust=0.5), plot.subtitle=element_text(size=15,hjust=0.5))
p2 <- plot.age.dis(df.plot.age.c, "country")
p3 <- plot.age.dis(df.plot.age.c.y, "country")
p4.bell <- plot.age.dis(df.plot.age.c.y %>% filter(str_detect(country, "UGA|DRC|BGD")), "country")

## save both
ggsave("output/graph_wgq/age_dis_last_msna_smooth.png", p1, height=7, width=9, bg = "white")
ggsave("output/graph_wgq/age_dis_country_last_msna_smooth.png", p2, width=10, bg = "white")
ggsave("output/graph_wgq/age_dis_country_year_msna_smooth.png", p3, width=12, height=11, bg = "white")
ggsave("output/graph_wgq/age_dis_country_year_msna_bell_smooth.png", p4.bell, width=8, height=7, bg = "white")

### By dimension, plot all severities by age
plot_age_difficulty <- function(data, dimension, max.y=0.6,...) {
  var_name <- paste0("wgq_", dimension)
  filtered_vals <- c(paste0(var_name, ".dnk"), paste0(var_name, ".no_difficulty"), paste0(var_name, ".pnta"))

  color_values <- setNames(
    c("#58585A", "#F3BEBD", "#F27D7C"),
    c(paste0(var_name, ".1_some_difficulty"),
      paste0(var_name, ".2_lot_of_difficulty"),
      paste0(var_name, ".3_cannot_do"))
  )

  title_map <- list(
    vision = "Vision difficulty",
    mobility = "Difficulty walking, taking steps",
    hearing = "Hearing difficulty",
    cognition = "Difficulty concentrating, remembering",
    self_care = "Difficulty self-care (washing, dressing)",
    communication = "Difficulty communicating"
  )

  replacement_pattern <- setNames(
    c("", " ", "", ""),
    c(paste0(dimension, "\\."), "_", "wgq ", "\\d ")
  )

  plot.age.dis(data, var = var_name, filtered.val = filtered_vals,...) +
    # scale_color_manual(values = color_values, labels = ~str_replace_all(., replacement_pattern)) +
    scale_fill_manual(values=color_values, labels = ~str_replace_all(., replacement_pattern)) +
    labs(title = title_map[[dimension]], subtitle = "by age and difficulty level") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(NA, max.y))
}

# Example Usage:
vec.dim <- c("vision", "mobility", "hearing", "cognition", "self_care", "communication")
plot.dim.all <-  map(vec.dim, ~plot_age_difficulty(df.plot.age.dim, .x, smooth=T) + labs(caption="", subtitle="")) %>% wrap_plots() +
  patchwork::plot_annotation(
    title="Prevalence of reported difficulty",
    subtitle="by age, dimension and severity level",
    caption="Data coming from MSNA datasets collected in 2022-2024 in 17 countries. For countries with multiple MSNA conducted, the most recent dataset has been kept.\nTo account for noise in data, prevalence is smoothed using a Generalized Additive Model (GAM) to capture non-linear patterns",
    theme = theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
  )

ggsave("output/graph_wgq/age_dis_dim_sev_smooth.png", plot.dim.all, height=10, width=15, bg = "white")

## for countries with DC below 5 y.o. specific graph by dimension
c.under.five <- summary_below_5 %>% pull(country) %>% unique
df.plot.age.dim.u5 <- df_wgq %>% filter(country %in% c.under.five) %>%  group_by(ind_age) %>%
  summarise(across(starts_with(paste0(vars, ".")), ~weighted.mean(., w = weight, na.rm=T)), n=n()) %>%
  filter(ind_age<95) %>%  rename_with(~str_replace_all(., c("\\.some"=".1_some", "\\.lot"=".2_lot", "\\.cannot"=".3_cannot")), contains("."))

plot.dim.under.5 <- map(vec.dim, ~plot_age_difficulty(df.plot.age.dim.u5, .x) + labs(caption="", subtitle="")) %>% wrap_plots() +
  patchwork::plot_annotation(
    title="Prevalence of difficulty, by age, dimension and severity level",
    subtitle="Case study on countries collecting data for all ages, including below 5 years old.",
    caption="Data coming from KEN (23), MLI (23/24) and NER (23), where individual below 5 were also interviewed.",
    theme = theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), plot.caption=element_text(hjust=0))
  )

ggsave("output/graph_wgq/age_dis_dim_sev_u5_smooth.png", plot.dim.under.5, height=10, width=15, bg = "white")

## for UGA
plot.dim.ug <- map(vec.dim, \(x) plot_age_difficulty(df.plot.age.dim.uga, x, max.y = .8)+ labs(caption="", subtitle=""))
plot.uga <- patchwork::wrap_plots(plot.dim.ug) +
  patchwork::plot_annotation(
    title="UGA 2024 case study",
    subtitle="Prevalence of difficulty by domain and age",
    caption="Data coming from MSNA 2024 in rolled out in Uganda.",
    theme = theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), plot.caption=element_text(hjust=0))
  )

ggsave("output/graph_wgq/age_dis_dim_sev_uga_smooth.png", plot.uga, height=10, width=15, bg = "white")

### uga special graph with both plot.bell.ug and plot.dim.ug side by side
p4.bell.uga <- plot.age.dis(df.plot.age.c.y %>% filter(str_detect(country, "UGA")), "country") +
  labs(caption="") +
  annotate("segment", x=23, y=.5, xend=12.5, yend=.3, arrow = arrow(length = unit(0.2, "cm")), color="black") +
  annotate("label", x=25, y=.5, label="Decrease in reported prevalences\nfrom 5 years old to 12.5 years old", size=3) +
  annotate("rect", xmin=4, xmax=18.5, ymin=-.025, ymax=0.28, alpha=.2)

p4.bell.uga

plot.uga.total <- p4.bell.uga +
  plot.dim.ug[[5]] +
  scale_y_continuous(labels=scales::percent_format(), limits=c(NA,.4)) +
  labs(subtitle="by age and difficulty level") +
  annotate("segment", x=35, y=.25, xend=12.5, yend=.14, arrow = arrow(length = unit(0.2, "cm")), color="black") +
  annotate("label", x=40, y=.25, label="Higher reporting of self-care\ndifficulties for younger age", size=3) +
  annotate("rect", xmin=4, xmax=18.5, ymin=-.01, ymax=.12, alpha=.2) +
  patchwork::plot_annotation(title="Figure: difficulty U-shaped curve, the case of Uganda MSNA",
                             theme=theme(plot.title = element_text(size=15)))+
  patchwork::plot_annotation(
    caption="To account for noise in data, prevalence is smoothed using a Generalized Additive Model (GAM) to capture non-linear patterns."
  )
plot.uga.total

ggsave("output/graph_wgq/uga_difficulty_u_shaped_smooth.png", plot.uga.total, height=6, width=10, bg = "white")

#### Do the same for all MSNAs pooled
plot.msna.total <- p1 + labs(caption="") +
  annotate("segment", x=27, y=.25, xend=10, yend=.14, arrow = arrow(length = unit(0.2, "cm")), color="black") +
  annotate("label", x=27, y=.3, label="Slight decrease in reported prevalences\nfrom 5 years old to 12.5 years old", size=3) +
  annotate("rect", xmin=4, xmax=12.5, ymin=-.025, ymax=0.125, alpha=.2) +
  plot.dim.all[[5]] + labs(subtitle="by age and difficulty level") +
  scale_y_continuous(labels=scales::percent_format(), limits=c(NA, .30)) +
  annotate("segment", x=40, y=.15, xend=10, yend=.08, arrow = arrow(length = unit(0.2, "cm")), color="black") +
  annotate("label", x=35, y=.15, label="Higher reporting of self-care\ndifficulties for younger age", size=3) +
  annotate("rect", xmin=4, xmax=15, ymin=-.015, ymax=0.07, alpha=.2)+
  patchwork::plot_annotation(
    caption="Data coming from MSNA datasets collected in 2022-2024 in 17 countries. For countries with multiple MSNA conducted, the most recent dataset has been kept.\nTo account for noise in data, prevalence is smoothed using a Generalized Additive Model (GAM) to capture non-linear patterns."
  )

ggsave("output/graph_wgq/all_msna_difficulty_u_shaped_smooth.png", plot.msna.total, height=6, width=10, bg = "white")


## By country OLS regression
all.c <- unique(df_wgq_model$country)
for (c in all.c) {
  df_wgq_model_c <- df_wgq_model |> filter(country == c) |> mutate(population_group = coalesce(pop_group, pop_group_cat))
  pred.lev <- df_wgq_model_c %>% select(all_of(predictors)) %>% select(where(~n_distinct(.)==1)) %>% names()
  predictors.c <- setdiff(predictors, pred.lev)
  formulas <- c("wgq_dis_4", "wgq_dis_3", "wgq_dis_2", "wgq_dis_1")
  models <- lapply(formulas, function(dep_var) {lm(reformulate(predictors.c, response = dep_var), data = df_wgq_model_c)})
  model_table <- stargazer::stargazer(models[[4]], models[[3]], models[[2]], models[[1]],
                                      type = "text",
                                      title = paste0(c, " - Pooled OLS model for WG-SS severity"),
                                      subtitle = "Coefficient are average increase/decrease in predicted probability of being disabled with corresponding severity level",
                                      align = TRUE, single.row = TRUE, header = FALSE,  dep.var.labels = "WG-SS severity level", dep.var.labels.include = TRUE,
                                      out = paste0("output/result/regression_OLS_pooled", c, ".html"))
}

### quickly do a weighted analysis of resp_gender for all countries year
res_resp_gender <- analyse(
  df_wgq |> expand.select.multiple.vec(c("resp_gender", "hoh_gender", "ind_gender")),
  group_var = c("country", "year"),
  var = c("resp_gender.male", "resp_gender.female",
          "hoh_gender.male", "hoh_gender.female",
          "ind_gender.male", "ind_gender.female"),
  col_weight="weight"
)

options(scipen = 999)  # Avoids scientific notation

## graphs for LBN only
ind <- "wgq_dis"
p1 <- plot.c.disag(data = result_lab_country_year_resp_gender %>% only_diff |> filter(country=="LBN"),
                   var=ind, col.disag=c("resp_gender"),sub_lab="Disaggregated by respondent gender", n.col=6,
                   add.plot="scale_y_continuous(labels=scales::percent_format(), breaks=c(0,0.1,0.2,0.3,.4,.5,.6), limits=c(0,0.5))")
p2 <- plot.c.disag(data = result_lab_country_year_hoh_gender %>% only_diff |> filter(country=="LBN"),
                   var=ind, col.disag=c("hoh_gender"), sub_lab = "Disaggregated by HoH gender", n.col=6,
                   add.plot="scale_y_continuous(labels=scales::percent_format(), breaks=c(0,0.1,0.2,0.3,.4,.5,.6), limits=c(0,0.5))")
ggsave(paste0("output/graph_wgq/detail/", ind, "_resp_hoh_gender_LBN",".png"), p1/p2, height=8, width=8, bg = "white")

## quick plot of resp gender and HoH gender for country c
p.gender.all <-plot.c.disag(data = res_resp_gender |> mutate(indicator="gender", indicator_label="MSNA gender breakdown", question=gsub("_"," ", question)) |> filter(!country %in% "NER"),
                            var="gender", col_question = "indicator", sub_lab="Sample composition: individual vs HoH vs respondent gender", col_choice_lab = "question",
                            col.disag = "choice", add.plot = "scale_y_continuous(labels=scales::percent_format(), breaks=seq(0, 1, by=0.25), limits=c(0,1.15))")
ggsave(paste0("output/graph_wgq/", "msna_gender_breakdown", ".png"), p.gender.all, bg="white", height=12, width=12)

## just lebanon case:
p.gender.lbn <-plot.c.disag(data = res_resp_gender |> mutate(indicator="gender", indicator_label="Lebanon MSNA gender breakdown",
                                                             question=gsub("_"," ", question)) |> filter(country %in% "LBN"),
                            var="gender", col_question = "indicator", sub_lab="Sample composition: individual vs HoH vs respondent gender", col_choice_lab = "question",
                            col.disag = "choice", add.plot = "scale_y_continuous(labels=scales::percent_format(), breaks=seq(0, 1, by=0.25), limits=c(0,1.15))")
ggsave(paste0("output/graph_wgq/", "msna_gender_breakdown_lbn", ".png"), p.gender.lbn, bg="white", height=4, width=8)

ind <- "resp_gender"
p.hoh.gender <-plot.c.disag(res_resp_gender |> mutate(choice_label="", indicator_label="MSNA respondent gender") |>
                              filter(!country %in% "NER"), var=ind, col_question = "question", sub_lab="",
                            col.disag = "choice", add.plot = "scale_y_continuous(labels=scales::percent_format(), breaks=seq(0, 1, by=0.25), limits=c(0,1.15))")


ggsave(paste0("output/graph_wgq/", ind, ".png"), p.hoh.gender, bg="white", height=6, width=12)

check <- df_wgq |>
  group_by(country, year) |>
  filter(!str_detect(resp_gender, "NA|undefined"), !hoh_gender %in% c("NA", "undefined")) |>
  count(resp_gender, hoh_gender, wt = weight) |>
  mutate(prop=n/sum(n))

## quickly tmap countries with MSNA for 2022-2023-2024 that are present in df_wgq
require(pacman)
p_load(tmap,sf)
## get boundaries for world countries from any package
world_countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> st_transform(4326)
check_all <- df_wgq |> group_by(country, year) |> summarise(n = n()) |> filter(n > 0)
df.map <- world_countries %>% filter(!adm0_a3 %in% "SOL") %>% mutate(country = str_replace_all(adm0_a3_cn, c("CAF"="CAR", "COD"="DRC", "PSX"="OPT"))) |> inner_join(check_all, by = c("country"))

map_year <- function(df, year_val){
  df %>% filter(year==year_val) %>% mutate(col=1) %>%
    tm_shape() + tm_borders() + tm_text("country", size = 0.35) + tm_fill(col = "col", title = "Number of individuals", style = "quantile", palette = "Reds", legend.show = FALSE) +
    tm_shape(world_countries) + tm_borders() + tm_layout(legend.position = c("left", "bottom"))+ tm_layout(legend.position = c("left", "bottom"))
}

map_22 <- map_year(df.map, 2022) +
  tm_layout(main.title = paste0("\n2022 - ", length(df.map[df.map$year == 2022, ][["country"]]), " MSNAs\n"), main.title.size = 1, main.title.position = c("center", "top"))
map_23 <- map_year(df.map, 2023) +
  tm_layout(main.title = paste0("\n2023 - ", length(df.map[df.map$year == 2023, ][["country"]]), " MSNAs\n"), main.title.size = 1, main.title.position = c("center", "top"))
map_24 <- map_year(df.map, 2024) +
  tm_layout(main.title = paste0("\n2024 - ", length(df.map[df.map$year == 2024, ][["country"]]), " MSNAs\n"), main.title.size = 1, main.title.position = c("center", "top"))
## save it with same length/width ratio

tmap_save(tm = map_22, "output/map_msna_wgq_22.png", width = 6, height = 6)
tmap_save(tm = map_23, "output/map_msna_wgq_23.png", width = 6, height = 6)
tmap_save(tm = map_24, "output/map_msna_wgq_24.png", width = 6, height = 6)

## plot together three maps side by side with one title for all, set up zoom
## tmap_arrange, but keep same zoom for each map than if it was separately saved
tmap_arrange(list(map_22, map_24, map_23), ncol = 2) %>% tmap_save("output/map_msna_wgq_22_23_24.png", width = 12, height = 6)

## for ppt just get out of this the wgq_lot of dif or cannot and plot the country/year averages as boxplot for each dimension on x axis

## quickly get prev by dimensions for any_difficulty


p_load(ggrepel)


plot.box.dim <- function(ind="wgq_any_difficulty"){
  df.plot.dim <- result_lab_country_year %>% keep.last.c.year %>% filter(indicator==ind) %>%
    group_by(choice_label) %>% mutate(max_box = quantile(mean, probs = .75) + 2*(quantile(mean, probs = .75)-median(mean))) %>% ungroup %>%
    mutate(country_f = ifelse(mean>max_box, paste0(country, " ", year), ""))
  lev.dim <- df.plot.dim %>% group_by(choice_label) %>% summarise(median=median(mean)) %>% arrange(median) %>% pull(choice_label)
  plot.dim <-  df.plot.dim %>%
    mutate(choice_label=factor(choice_label, levels=lev.dim)) %>%
    ggplot(aes(x=choice_label, y=mean)) +
    geom_boxplot() +
    stat_summary(fun = median, geom = "segment", color = "indianred", size=1,
                 aes(x = as.numeric(choice_label) - 0.37,
                     xend = as.numeric(choice_label) + 0.375,
                     yend = after_stat(y))) +
    geom_text(aes(label=country_f), size=2, vjust=-2) +
    # geom_label_repel(aes(label=country_f), size=1, vjust="inward", box.padding = .5, force=1, max.overlaps = Inf) +
    scale_y_continuous(labels=scales::percent_format())+
    theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) +
    labs(title=unique(result_lab_country_year[result_lab_country_year$indicator==ind, "indicator_label"]),
         subtitle="Boxplot of country prevalence, by dimension",
         caption="Boxplot representing the distribution of country level prevalences for the latest available MSNA in country with WG-SS data.", y="\n% individuals", x="Dimension\n") +
    coord_flip()
  plot.dim
  ggsave(paste0("output/graph_wgq/wgq_",ind,"_boxplot_latest_msna.png"), plot.dim, bg="white", height=6, width=8)
}

map(c("wgq_any_difficulty", "wgq_cannot_do_or_lot_of_difficulty", "wgq_cannot_do"), plot.box.dim)

lev.age <- result_lab_country_year_ind_age_cat_wide %>% mutate(age_min=str_replace_all(ind_age_cat_wide, "-.*", "") %>% as.numeric) %>%
  arrange(age_min) %>% pull(ind_age_cat_wide) %>% unique
result_lab_country_year_ind_age_cat_wide <- result_lab_country_year_ind_age_cat_wide %>% mutate(ind_age_cat_wide=factor(ind_age_cat_wide, levels=lev.age)) %>% keep.last.c.year
plot.age <- result_lab_country_year_ind_age_cat_wide %>% filter(question=="wgq_dis_3") %>%
  group_by(ind_age_cat_wide) %>% mutate(max_box = quantile(mean, probs = .75) + 2*(quantile(mean, probs = .75)-median(mean))) %>% ungroup %>%
  mutate(country_f = ifelse(mean>max_box, paste0(country, " ", year), "")) %>%
  ggplot(aes(x=ind_age_cat_wide, y=mean)) + geom_boxplot() +
  stat_summary(fun = median, geom = "segment", color = "indianred", size=1,
               aes(x = as.numeric(factor(ind_age_cat_wide)) - 0.37, xend = as.numeric(factor(ind_age_cat_wide)) + 0.375, yend = after_stat(y))) +
  geom_text(aes(label=country_f), size=2, vjust=-2) + scale_y_continuous(labels=scales::percent_format())+
  theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) +
  labs(title="Prevalence of WG-SS 3", subtitle="Boxplot of country prevalence, by age group",
       caption="Boxplot representing the distribution of country level prevalences for the latest available MSNA in country with WG-SS data.", y="\n% individuals", x="Age group\n") + coord_flip()
# plot.age
ggsave("output/graph_wgq/wgq_dis_3_boxplot_age_latest_msna.png", plot.age, bg="white", height=6, width=8)


###### AFG Case study ##########################################################
### Comparing WG-SS AFG with ALCS and MDS
res_all_afg <- list.res %>% map_df(\(x) x, .id = "disag_level") %>%
  bind_rows %>% filter(country=="AFG") %>% filter((question=="wgq_dis_3" & disag_level %in% c("result_lab_country_year",
                                                                                              "result_lab_country_year_gender",
                                                                                              "result_lab_country_year_ind_age_cat",
                                                                                              "result_lab_country_year_ind_age_cat_wide")) |
                                                    (indicator=="wgq_cannot_do_or_lot_of_difficulty" & disag_level %in% c("result_lab_country_year")))
colnames(res_all_afg)
res_all_afg %>%head(20)

## quick wgq_analysis using new age_detail column
res_afg <- list()

var_an <- c("wgq_dis_3", "wgq_dis_2",
            "wgq_vision_cannot_do_or_lot_of_difficulty_d",
            "wgq_hearing_cannot_do_or_lot_of_difficulty_d",
            "wgq_mobility_cannot_do_or_lot_of_difficulty_d",
            "wgq_cognition_cannot_do_or_lot_of_difficulty_d",
            "wgq_self_care_cannot_do_or_lot_of_difficulty_d",
            "wgq_communication_cannot_do_or_lot_of_difficulty_d"
)
group<- list(
  c("country", "year"),
  c("country", "year", "ind_gender"),
  c("country", "year", "age_detail"),
  c("country", "year", "age_mds"),
  c("country", "year", "ind_gender", "age_detail")
)

df_wgq$age_detail %>% unique
df_wgq_afg <- df_wgq %>% filter(country=="AFG", ind_age>=0) %>%
  mutate(age_detail=case_when(age_detail=="85-120" ~ "85+", T ~ age_detail),
         age_mds=case_when(age_mds=="56-120" ~ "56+", T ~ age_mds))

for (g in group){
  vars_analyse <- var_an
  if (!all(g %in% c("country", "year"))) vars_analyse <- "wgq_dis_3"
  res_afg[[paste(g, collapse = "_")]] <- analyse(
    df = df_wgq_afg,
    group_var = g,
    var = vars_analyse,
    col_weight = "weight"
  )
}

df_res_afg <- res_afg %>% map_df(\(x) x, .id = "disag_level") %>%
  mutate(age_min = str_replace_all(age_detail, c("-.*|\\+"="")) %>% as.numeric)
lev.age <- df_res_afg %>% arrange(age_min) %>% pull(age_detail) %>% unique
df_res_afg <- df_res_afg %>% mutate(age_detail=factor(age_detail, levels=lev.age)) %>% filter(n!=0) %>% mutate(study="MSNA 2023") %>%
  mutate(question = ifelse(!is.na(choice), paste0(question, "_", choice), question)) %>%
  left_join(wgq_lab_2 %>% select(question=`final id`, indicator, indicator_label, choice_label)) %>%
  relocate(indicator, indicator_label, choice_label, .before="mean")

## read other datasets
res_alcs_mds <- import("data/alcs_mds_afg_res.xlsx") %>% mutate(across(matches("age"), ~str_replace_all(., c(" - "="-", " \\+"="+"))), year=str_replace_all(study, ".* ", "") %>% as.numeric)
res_afg_all <- df_res_afg %>% bind_rows(res_alcs_mds) %>% mutate(choice_label=str_replace_all(choice_label, "3 - ", ""))

### graph it all

## general prevalence
res_afg_all_prev <- res_afg_all %>% filter(disag_level=="country_year", question=="wgq_dis_3")
res_afg_all_prev_gender <- res_afg_all %>% filter(disag_level %in% c("country_year_gender", "country_year_ind_gender"), question=="wgq_dis_3")
res_afg_all_dim <- res_afg_all %>% filter(disag_level=="country_year", indicator=="wgq_cannot_do_or_lot_of_difficulty")

## plot with fill=study
plot.prev <- res_afg_all_prev %>% mutate(study=paste0(study, "\n", "n=",n)) %>%
  ggplot(aes(x=reorder(study,-mean), y=mean)) + geom_bar(stat="identity", position="dodge", fill="#EE5859") + scale_y_continuous(labels=scales::percent_format(), limits=c(0,.20)) +
  # scale_fill_manual(values=c("#EE5859", "#0067A9", "#D2CBB8")) +
  scale_x_discrete(labels=~str_wrap(., 25)) +
  labs(x="", y="% of individuals", title="Disability prevalence in Afghanistan", subtitle="By study", caption="", fill="Study")+
  theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
  geom_text(aes(label=scales::percent(mean)), position=position_dodge(width=1), vjust=-0.75, size=3)
plot.prev
## plot by gender
plot.prev.gender <- res_afg_all_prev_gender %>%
  ggplot(aes(x=reorder(study,-mean),y=mean, fill=ind_gender)) + geom_bar(stat="identity", position="dodge") + scale_y_continuous(labels=scales::percent_format(), limits=c(0,.20)) +
  scale_fill_manual(values=c("#EE5859", "#0067A9")) +
  scale_x_discrete(labels=~str_wrap(., 25)) +
  labs(x="", y="% of individuals", title="Disability prevalence in Afghanistan", subtitle="Disaggregated by study/gender", fill="Individual gender") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
  geom_text(aes(label=scales::percent(mean)), position=position_dodge(width=1), vjust=-0.75, size=3)
# plot.prev.gender

## plot by dimension
plot.dim <- res_afg_all_dim %>%
  ggplot(aes(x=reorder(study,-mean),y=mean, fill=choice_label)) + geom_bar(stat="identity", position="dodge") + scale_y_continuous(labels=scales::percent_format(), limits=c(0,.03)) +
  scale_fill_manual(values=c("#EE5859", "#0067A9", "#D2CBB8", "#F4A261", "#2A9D8F", "#264653")) +
  scale_x_discrete(labels=~str_wrap(., 25)) +
  labs(x="", y="% of individuals", title="Disability prevalence in Afghanistan", subtitle="Disaggregated by dimension", fill="Dimension") +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
  geom_text(aes(label=scales::percent(mean, accuracy = .01)), position=position_dodge(width=.9), vjust=-0.75, size=2.5)
plot.dim

## plot by age
## age_detail
df.age <- res_afg_all %>% mutate(age_detail=factor(age_detail, levels=lev.age)) %>% filter(!is.na(age_detail), is.na(ind_gender))
plot.age <- df.age %>%
  ggplot(aes(x = age_detail, y = mean, fill = study)) +
  geom_bar(data = df.age %>% filter(study == "ALCS 2017"), stat = "identity", position = "identity") +
  geom_bar(data = df.age %>% filter(study == "MSNA 2023"), stat = "identity", position = "identity", aes(y = -mean)) +
  scale_y_continuous(labels = function(x) {scales::percent(abs(x))}, limits = c(-.6, 0.6)) +
  scale_fill_manual(values = setNames(c("#D2CBB8", "#F4A261"), c("MSNA 2023", "ALCS 2017"))) + coord_flip() +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
  labs(title="Comparison of prevalence by age", subtitle="By study", y="prevalence", x="Age group\n", fill="Study")
plot.age
## age_mds
# res_afg_all$age_mds %>% unique
df.age.mds <- res_afg_all %>% mutate(age_mds=factor(age_mds, levels=c("18-25", "26-35", "36-45", "46-55","56+"))) %>% filter(!is.na(age_mds))
plot.age.mds <- df.age.mds %>%
  ggplot(aes(x = age_mds, y = mean, fill = study)) +
  geom_bar(data = df.age.mds %>% filter(study == "MDS 2019"), stat = "identity", position = "identity") +
  geom_bar(data = df.age.mds %>% filter(study == "MSNA 2023"), stat = "identity", position = "identity", aes(y = -mean)) +
  scale_y_continuous(labels = function(x) {scales::percent(abs(x))}, limits = c(-0.3, 0.3)) +
  scale_fill_manual(values = setNames(c("#D2CBB8", "#2A9D8F"), c("MSNA 2023", "MDS 2019"))) + coord_flip() +
  theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
  labs(title="Comparison of prevalence by age", subtitle="By study", y="prevalence", x="Age group\n", fill="Study")
plot.age.mds
## do two side by side age_pyramid plots with first MSNA 2023 then ALCS 20174
age_cat_table <- res_afg_all %>% filter(!is.na(age_detail), !is.na(ind_gender))
plot.pyr <- plot_age_pyramid(df = age_cat_table,
                             var_gender = "ind_gender",
                             var_age_cat = "age_detail",
                             col_stat = "mean",
                             col_n_unw = "n",
                             unit = "Individual",
                             group_var = "study",
                             save = F,
                             age.labs = NULL,
                             value_men = "male",
                             value_women = "female",
                             title = "Age/gender pyramid",
                             subtitle = "Disability prevalence, by study",
                             n_col = 2) + labs(caption="")
plot.pyr

ggsave("output/graph_wgq/AFG_comparison.png", (plot.prev.gender+plot.age.mds) / (plot.pyr+plot.dim), width = 10, height = 10, units = "in", dpi = 300, bg="white")



## quick plot of prevalence for MOZ
res_wgq_amdin1 <- df_wgq %>% group_by(country, year, admin1) %>% count(wgq_dis_3, wt = weight) %>% filter(!is.na(wgq_dis_3)) %>% mutate(prop=n/sum(n))
res_wgq_amdin1 %>% filter(wgq_dis_3==1, !is.na(admin1)) %>%
  ggplot() +
  geom_bar(aes(x=reorder(admin1, prop), y=prop), stat="identity") +theme_minimal() +coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) + facet_wrap(year~country, scales="free")


### quick for AFG by reg
labs.afg <- import("data/mapping/MSNA_2024_tools/AFG1.xlsx", sheet=2) %>% filter(list_name=="l_admin1") %>%
  rename(admin1=name, admin1_name=`label::English`)

res_wgq_amdin1 %>% filter(wgq_dis_3==1, !is.na(admin1), country=="AFG") %>% left_join(labs.afg) %>%
  ggplot() +
  geom_bar(aes(x=reorder(admin1_name, prop), y=prop), stat="identity") + theme_minimal() + coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) + facet_wrap(year~country, scales="free") +
  labs(y="", x="")


## test analysis, what would be CAR 2023 wgq_dis_3 prevalence if only adults were interviewed?
df_wgq %>%
  filter(country=="CAR", year==2023, ind_age>=18) %>%
  summarise(
    wgq_dis_3=weighted.mean(wgq_dis_3, weight, na.rm=T)
  )


## self vs proxy
map_proxy <- import("data/proxy_resp_review.xlsx")

map_proxy <- map_proxy %>%
  mutate(
    proxy_available=case_when(proxy == "no" ~ "no",
                              !is.na(`skip logic comment`) ~ "yes*",
                              T ~ "yes"),
    caregiver_available=case_when(caregiver == "no" ~ "no",
                                  !is.na(`skip logic comment`) ~ "yes*",
                                  T ~ "yes")
         ) %>%
  filter(year==2023)

test_table <- map_proxy %>%
  select(country, proxy_available, caregiver_available) %>%
  mutate_all(as.character) %>%
  pivot_longer(-country) %>%
  pivot_wider(names_from = country, values_from = value)

## do a ggplot geom_tile with green coloring for yes or yes* values and red for no
map_proxy %>%
  select(country, proxy_available, caregiver_available) %>%
  pivot_longer(-country) %>%
  ggplot(aes(x=country, y=name, fill=value)) +
  ##
  geom_tile() +
  scale_fill_manual(values = c("yes" = "#74C476", "yes*" = "#74C476", "no" = "firebrick1")) +
  scale_y_discrete(labels=~str_replace_all(.,"_", " "))+
  geom_text(aes(label=value), color="black", size=3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(title="Proxy and caregiver availability", subtitle="By country", x="", y="") +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))


#### Quick analysis for AFG of support services, for MOZ 24 and BGD 23 ind_challenges
cols.is.challenge <- str_subset(colnames(df_wgq), "wgq_ind_dis_challenges/")
df_wgq_moz <- df_wgq %>%
  filter(country=="MOZ") %>%
  collapse.select.multiple(cols = cols.is.challenge, name = "wgq_ind_dis_challenges_type") %>%
  mutate(wgq_ind_dis_challenges_type=str_replace_all(wgq_ind_dis_challenges_type, c("\\bwgq_ind_dis_challenges\\/"="")))
df_wgq_moz <- df_wgq_moz %>% mutate(wgq_ind_dis_challenges_type=ifelse(wgq_ind_dis_challenges_type=="",NA, wgq_ind_dis_challenges_type))

df_wgq_all <- df_wgq %>% filter(country!="MOZ") %>% bind_rows(df_wgq_moz) %>%
  mutate(wgq_ind_dis_challenges = case_when(country=="MOZ"  ~ NA_character_, T ~ wgq_ind_dis_challenges))

var_an <- c(
  "wgq_support_services",
  "wgq_ind_dis_challenges",
  "wgq_ind_dis_challenges_type"
)

test <- df_wgq_all %>% filter(str_detect(country, "BGD")) %>%
  filter(!is.na(wgq_ind_dis_challenges)) %>% select(matches("wgq_dis"), wgq_ind_dis_challenges, everything())

df_wgq_all <- df_wgq_all %>% expand.select.multiple.vec(var_an)

var_an <- df_wgq_all %>% select(starts_with(paste0(var_an, "."))) %>% colnames

group <- list(
  c("country", "year"),
  c("country", "year", "ind_gender"),
  c("country", "year", "ind_age_cat"),
  c("country", "year", "ind_age_cat_wide")
)

res <- list()

for (g in group){
  res[[paste(g, collapse = "_")]] <- analyse(
    df = df_wgq_all,
    group_var = g,
    var = var_an,
    col_weight = "weight"
  )
}

res_add <- res %>% bind_rows() %>% filter(!is.nan(mean)) %>% mutate(choice_label=str_replace_all(choice, c("_"=" ")))

labels.add <- data.frame(
  question = c("wgq_support_services",
               "wgq_ind_dis_challenges",
               "wgq_ind_dis_challenges_type"),
  question_label = c("Access to the required disability support services (e.g., physical rehabilitation services, psycho-social and theraputic, or required devices)",
                     "Challenges experienced in accessing education, healthcare, food, shelter support, humanitarian assistance, and in employment, and social interactions due to his/her impairment",
                     "Challenges experienced in accessing services due to impairment, by type of service")
)

res_add_label <- res_add %>% left_join(labels.add) %>% relocate(question_label, .after="question") %>% relocate(choice_label, .after="choice")

ord <- c("yes", "no", "no answer", "always", "often", "sometimes", "rarely", "never", "dont know")

res_add_label <- res_add_label %>% mutate(choice_label=factor(choice_label, levels=c(ord, unique(res_add_label$choice_label) %>% keep(!. %in% ord)), ordered = T))

res_add_label_c <- res_add_label %>% filter(is.na(ind_age_cat), is.na(ind_gender), is.na(ind_age_cat_wide))
res_add_label_gender <- res_add_label %>% filter(is.na(ind_age_cat), !is.na(ind_gender), is.na(ind_age_cat_wide))
res_add_label_age <- res_add_label %>% filter(!is.na(ind_age_cat), is.na(ind_gender), is.na(ind_age_cat_wide))
res_add_label_age_wide <- res_add_label %>% filter(is.na(ind_age_cat), is.na(ind_gender), !is.na(ind_age_cat_wide))


for (x in unique(res_add_label$question)){
  x <- unique(res_add_label$question)[3]
  p <- plot.c(data = res_add_label_c,
              var = x,
              custom_margin = .025,
              factor=F,
              col_question = "question",
              col_label = "question_label",
              col_choice_lab = "choice_label",flip = T) +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0,.6)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 80)) +
    labs(y="% of individuals")
  p
  ggsave(paste0("output/graph_wgq/additionnal_indicator/plot_", x, ".png"), p, width = 8, height = 4.5, units = "in", dpi = 300, bg="white")

  p <- plot.c.disag(data = res_add_label_gender,
              var = x,
              factor=T,
              # custom_margin = .079,
              col.disag = "ind_gender",
              col_question = "question",
              col_label = "question_label",
              col_choice_lab = "choice_label",flip = F) +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0,.4)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 80))
  p
  ggsave(paste0("output/graph_wgq/additionnal_indicator/plot_", x, "_gender.png"), p, width = 4.75, height = 6, units = "in", dpi = 300, bg="white")


  p <- plot.c.disag(data = res_add_label_age_wide,
                    var = x,
                    factor=T,
                    # custom_margin = .15,
                    col.disag = "ind_age_cat_wide",
                    col_question = "question",
                    col_label = "question_label",
                    col_choice_lab = "choice_label",
                    flip = F) +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0,.6)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 80))
  p
  ggsave(paste0("output/graph_wgq/additionnal_indicator/plot_", x, "_age_cat_wide.png"), p, width = 6, height = 6, units = "in", dpi = 300, bg="white")

}

### quickly plot diff in prevalence with loliplot
res.diff <- result_lab_country_year_resp_gender

plot.loli.wgq <- function(df=res.diff, var="wgq_dis_3", filter_diff=0.01){

  ## if no "mean/low" in colnames compute lower and upper confidence interval
  if (!("mean/low" %in% colnames(df))){
    df <- df %>% mutate(`mean/low` = mean - 1.96*sqrt(mean(1-mean)/n), `mean/upp` = mean + 1.96*sqrt(mean(1-mean)/n))
  }

  res.diff.filt <- df %>% mutate(country_year=paste0(country, "\n", gsub("^20", "", year))) %>% filter(question==var) %>%
    group_by(country_year, question) %>% mutate(min_y=min(mean, na.rm=T), max_color = first(resp_gender[order(-mean)], default = NA_character_)) %>% add.lab %>%
    mutate(mean_women = mean[resp_gender=="female respondent"],
           mean_men = mean[resp_gender=="male respondent"],
           test =`mean/low`[resp_gender=="female respondent"]-`mean/upp`[resp_gender=="male respondent"],
           significant = case_when(
             mean_women > mean_men & `mean/low`[resp_gender=="female respondent"]>`mean/upp`[resp_gender=="male respondent"] ~ T,
             mean_women < mean_men & `mean/low`[resp_gender=="male respondent"]>`mean/upp`[resp_gender=="female respondent"] ~ T,
             T ~ F),
           mark_sign = case_when(
             resp_gender=="female respondent" & significant ~ "*",
             resp_gender=="male respondent" & significant ~ "*",
             T ~ ""),
           mark_height = (mean[resp_gender=="female respondent"]+mean[resp_gender=="male respondent"])/2
    ) %>% ungroup %>%
    mutate(diff=mean-min_y) %>% arrange(desc(diff)) %>%
    mutate(country_year=factor(country_year, levels=unique(country_year))) %>% arrange(country_year)

  c.filt <- res.diff.filt %>% filter(diff >= filter_diff) %>% pull(country_year)
  res.diff.filt <- res.diff.filt %>% filter(country_year %in% c.filt)

  plot <- ggplot() +
    geom_segment(data = res.diff.filt, aes(x=country_year, xend=country_year, y=min_y, yend=mean, color=max_color), size=0.75, show.legend = F) +
    geom_point(data = res.diff.filt, aes(color=resp_gender, fill=resp_gender, x=country_year, y=mean), size=2) +
    scale_colour_manual(values=c("indianred", "#0067A9"),labels=\(x) str_replace_all(x, c("_"=" "))) +
    scale_fill_manual(values=c("indianred", "#0067A9"),labels=\(x) str_replace_all(x, c("_"=" "))) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0,NA)) +
    scale_x_discrete(labels=\(x) str_replace(x, " \\d\\d", "")) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), plot.caption = element_text(hjust=0)) +
    geom_text(data = res.diff.filt, aes(y=mean, x=country_year,label = ifelse(significant, paste0(round(100*mean, 0), "%"), "")), hjust=-.5, size=3)+
    labs(fill="Respondent\ngender", color="Respondent\ngender", title="Prevalence of disability - by respondent gender",
         y="% of individuals", x="", subtitle=str_wrap(unique(res.diff.filt$choice_label), 75),
         caption=str_wrap("Whenever the difference in prevalence is significant at a level of 5%, prevalence values are displayed with % labels in graph (non-overlapping 95% confidence intervals)", 100))
  plot
  return(plot)
}

## iterate over x and y to gave var on x and filter_diff level on y
plots <- map2(
  c("wgq_dis_1",
    "wgq_dis_2",
    "wgq_dis_3",
    "wgq_dis_4"),
  c(0.025, 0.015, 0.01, 0.003),
  ~{
    p <- plot.loli.wgq(var = .x, filter_diff = .y) +
      labs(caption= paste0(
             str_wrap("Whenever the difference in prevalence is significant at a level of 5%, prevalence values are displayed with % labels in graph (non-overlapping 95% confidence intervals)", 100),
             "\nCountries where prevalence difference is above ", paste0(100*.y, "%"), " are displayed"
           ))
    ggsave(filename = paste0("output/graph_wgq/prevalence diff/wgq_diff_", ., "_resp_gender.png"),plot = p, width = 6.5, height = 6, units = "in", dpi = 300, bg="white")
    return(p + labs(title="",
             caption= paste0(
               "Countries where prevalence difference is above ", paste0(100*.y, "%"), " are displayed"
             )))
  }
)


all.plots.wgq.resp.gender <- plots %>% wrap_plots(ncol=2) +
  patchwork::plot_annotation(
    title="Prevalence of disability", subtitle="By severity level, disaggregated by respondent gender",
    caption =str_wrap("Whenever the difference in prevalence is significant at a level of 5%, prevalence values are displayed with % labels in graph (non-overlapping 95% confidence intervals)", 250),
    theme = theme(plot.title = element_text(hjust = 0.5, size=20), plot.subtitle = element_text(hjust = 0.5, size=16), plot.caption = element_text(hjust = 0))
  )
ggsave(
  "output/graph_wgq/prevalence diff/wgq_diff_all.png", plot =all.plots.wgq.resp.gender, width = 12, height = 10, units = "in", dpi = 300, bg="white"
)


##### Plot other SDR prevalence
other_prevalence <- import("data/SDR - prevalences review.xlsx",sheet=1)


plot_clean_prevalence <- function(df=other_prevalence) {
  # Prepare data
  df <- df %>%
    rename(prevalence = `prevalence (WG-SS 3)`) %>%
    mutate(
      country = paste0(str_to_title(country), "\n", year),
      choice_label = "WG-SS 3"  # dummy label to reuse styling logic
    ) %>%
    arrange(prevalence) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  # Color for WG-SS 3 only
  col_fill <- c("WG-SS 3" = "#F27D7C")

  # Create plot
  p <- ggplot(df, aes(x = country, y = prevalence, fill = choice_label)) +
    geom_bar(stat = "identity",show.legend = F) +
    geom_text(aes(label = ifelse(prevalence > 0.01, paste0(round(100 * prevalence, 0), "%"), "*")),
              vjust = -0.8, size = 3) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(df$prevalence, na.rm = TRUE) + 0.05)) +
    scale_fill_manual(values = col_fill) +
    labs(
      title = "Disability Prevalence - Secondary Data Review",
      subtitle = "From national surveys using the Washington Group Short Set with WG-SS 3 threshold",
      x = "Country - year", y = "% of individuals",
      fill = "WG-SS\nSeverity Level",
      caption = "Outlier data coming from HNAP study in Syria - Disability in Sytria, Investigation on the intersectional impacts of gender, age and a decade of conflict on persons with disabilities"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )

  return(p)
}

plot_other <- plot_clean_prevalence()
ggsave("output/graph_wgq/additionnal_indicator/other_prevalence.png", plot = plot_other, width = 10, height = 6, units = "in", dpi = 300, bg="white")

## quickly check df_wgq for MMR

test <- df_wgq %>% filter(country=="MMR") %>% select(ind_age, wgq_caregiver_proxy, wgq_proxy_resp, starts_with("wgq"), everything())
