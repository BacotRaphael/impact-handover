require(pacman)
p_load(readxl, dplyr, tidyr, stringr, purrr, writexl, rio, humind, data.table, cleaningtools)

source("utils.R")

## Read tool 
path.tool <- "../ETH_Toolkit/v6/Tool 1 - End user survey (ETH reviewed) v6.xlsx"
tool <- read_excel(path.tool, sheet = "survey") %>% separate(type, int=c("q.type", "list_name"), sep=" ", remove = F) %>% mutate(row=row_number())
choices <- read_excel(path.tool, sheet = "choices")
tool.other <- tool %>% filter(!str_detect(q.type, "select_"))
col_lab <- "label::English (en)"
col_lab2 <- "label::Arabic (ar)"
survey_combined <- tool %>% 
  filter(str_detect(q.type, "select_")) %>% 
  left_join(choices %>% select(list_name, choice_name=name, choice_label=any_of(col_lab), choice_label_ar=any_of(col_lab2)), by="list_name") %>%
  bind_rows(tool.other) %>% arrange(row)

## Read data
path.data <- "data/data_jmmi_fake_data.csv"
raw.data <- import(path.data)

## Read labels 
path.labels <- "resources/labels.xlsx"

ind.name <- import(path.labels, sheet=1) %>% select(name=`final id`, !!sym(col_lab):=`question label`, short_label=`Short Label`)
ind.label <- import(path.labels, sheet=2) %>% select(name=`global id`, choice_name=class, choice_label=class_label)
indicator.label <- ind.name %>% left_join(ind.label) %>% filter(!is.na(name))

## append labels to survey combined
## For now, until integrated in the final version of the tool with vlook up
ind.name.all <- tool %>% select(name, any_of(col_lab), short_label) %>% bind_rows(ind.name) %>% filter(!is.na(short_label))
survey_combined <- survey_combined %>% bind_rows(indicator.label)

## extract variable names
so.questions <- tool %>% filter(q.type=="select_one") %>% pull(name) %>% keep(. %in% colnames(raw.data))
sm.questions <- tool %>% filter(q.type=="select_multiple") %>% pull(name) %>% keep(. %in% colnames(raw.data))
int.questions <- tool %>% filter(q.type=="integer") %>% pull(name) %>% keep(. %in% colnames(raw.data))

## Washingtown group question!!
# wgq.col <- tool %>% filter(str_detect(name, "difficulty_")) %>% pull(name)
# wgq.choices <- choices %>% filter(list_name %in% "difficulty") %>% pull(name)
data.composed <- raw.data %>%
  add_loop_wgq_ss(ind_age = "age",
                  vision = "difficulty_seeing",
                  hearing = "difficulty_hearing",
                  mobility = "difficulty_walking",
                  cognition = "difficulty_remembering",
                  self_care = "difficulty_selfcare",
                  communication = "difficulty_communicating",
                  no_difficulty = "no_difficulty",
                  some_difficulty = "yes_some_difficulty",
                  lot_of_difficulty = "yes_a_lot_of_difficulty",
                  cannot_do = "cannot_do_at_all",
                  undefined = c("dnk", "pnta"))

colnames(data.composed) %>% str_subset("wgq")

## displacement status
## calculate if displ_arrival_duration is missing
if (!"displ_arrival_duration" %in% colnames(data.composed)) {
  data.composed$displ_arrival_duration <- data.composed$today-data.composed$displ_arrival 
}

data.composed <- data.composed %>%
  mutate(
    displ_arrival_duration_recoded = case_when(
      displ_arrival_duration < 90 ~ "less_3_months",
      displ_arrival_duration >= 90 & displ_arrival_duration < 180 ~ "3_6_months",
      displ_arrival_duration >= 180 & displ_arrival_duration < 365 ~ "7_12_months",
      displ_arrival_duration >= 365 ~ "more_1_year",
      T ~ NA_character_
    )
  )
  
## create displacement category mixing both country_origin and displ_arrival_duration > 1 year vs < 1 year
## use survey_combined filtering name=="displ_origin_label" to get choice_name and choice_label
lab_country <- survey_combined %>% filter(name=="displ_origin") %>% select(choice_name, choice_label)

data.composed <- data.composed %>%
  mutate(
    ## use lab_country to get choice_label depending on displ_origin using str_replace_all or match
    displ_origin_label = str_replace_all(displ_origin, setNames(lab_country$choice_label, lab_country$choice_name)),
    displacement_status = case_when(
      displ_arrival_duration > 365 ~ paste0(displ_origin_label, " displaced more than 1 year")
    ) 
  )

## recode int questions into select.one with categories 0, 1, 2, 3, 4, 5 , 6 , > 7 for phones_in_household and devices_in_household
## define threshold for categorisation
threshold_phone=5
threshold_device=5

data.composed <- data.composed %>%
  mutate(
    
    phones_in_household_cat = case_when(
      phones_in_household < threshold_phone ~ as.character(phones_in_household),
      phones_in_household >= threshold_phone ~ paste0(threshold_phone, "_plus")
    ),
    
    devices_in_household_cat = case_when(
      devices_in_household < threshold_device ~ as.character(devices_in_household),
      devices_in_household >= threshold_device ~ paste0(threshold_device, "_plus")
    )
  )

## recode age question into selec.one with wider categories 
## 0_17, 18_25, 26_34, 35_50, 50_plus
data.composed <- data.composed %>%
  mutate(
    age_cat = case_when(
      age < 18 ~ "0_17",
      age >= 18 & age < 26 ~ "18_25",
      age >= 26 & age < 35 ~ "26_34",
      age >= 35 & age < 50 ~ "35_50",
      age >= 50 ~ "50_plus"
    )
)

## recode country of origin to have displacement status (idp vs refugee from XXX)

## Coverage network, by location and type of coverage [combine both]
# network_coverage
# data_coverage
# wifi_coverage
# col_coverage_loc <- c("network_coverage", "data_coverage", "wifi_coverage")
# ## for each of the above, mutate .{col}_at_home if it is equal to "coverage_at_home" 
# data.composed <- data.composed %>%
#   mutate(
#     across(all_of(col_coverage_loc), ~case_when(
#       . == "coverage_at_home" ~ "coverage_at_home",
#       . == "coverage_at_home" ~ "coverage_at_home",
#     ))
#   )

## Phone access and ownership
## combine questions on # phones in the HH 

data.composed <- data.composed %>%
  mutate(
    phone_access_and_ownership = case_when(
      phones_in_household > 0 & owned_phone_type == "smartphone" ~ "owns_smartphone",
      phones_in_household > 0 & owned_phone_type == "feature_phone" ~ "owns_feature_phone",
      phones_in_household > 0 & owned_phone_type == "basic_phone" ~ "owns_basic_phone",
      str_detect(access_someone_elses_phone, "^yes") ~ "can_borrow",
      owned_phone_type == "none" & access_someone_elses_phone == "no" ~ "no_access",
      T ~ NA_character_
    )
  )

## Individual access to an internet-enabled device, by type of ownership/place of use.
## Combine questions on heard of internet / used any other device

data.composed <- data.composed %>%
  mutate(
    access_internet_enabled_device = case_when(
      (heard_of_the_internet == "no" | used_other_device_recurrence=="never") ~ "no_access", ## to define starting which threshold we consider meaningful use?
      (owned_device_type == "none" &  used_other_device_recurrence != "never") |
        (used_other_device_recurrence != "never" & devices_in_household==0) ~ "can_borrow", ## to define starting which threshold we consider meaningful use?
      (owned_device_type != "none") | (owned_phone_type %in% c("smartphone", "feature_phone")) ~ "owns", ## combine any other device owned with phone accessing internet
      T ~ NA_character_)
        )

## Analysis ###########################################################################

## additionnal composed indicators
add.ind <- c("wgq_dis_1", "wgq_dis_2", "wgq_dis_3", "wgq_dis_4",
             "phone_access_and_ownership", "access_internet_enabled_device", "displ_arrival_duration_recoded",
             "phones_in_household_cat", "devices_in_household_cat","age_cat")

## expand child columns for all 
data.composed <- data.composed %>% 
  expand.select.multiple.vec(c(so.questions, add.ind))

## define columns to be analysed
pattern.select <- paste0(so.questions, ".") %>% append(paste0(sm.questions, ".")) %>% 
  append(paste0(add.ind, ".")) %>% str_subset("^admin|^site|^enumerator", negate=T)
col.select.analyse <- data.composed %>% select(starts_with(pattern.select)) %>% colnames
col.select.analyse.int <- int.questions
  
## Define all the disaggregation levels (one row per analysis, with a vector of disaggregation variables if more than one)
group_var <- import("resources/labels.xlsx", sheet=3) %>% 
  mutate(across(everything(), ~ifelse(.=="none", "",.))) %>%
  mutate(group_var = map2(varname_disag_1, varname_disag_2, ~ c(as.character(.x), as.character(.y)))) %>%
  pull(group_var) %>% map(~ keep(., ~ !is.na(.)))

## test Analysis with integer median/mean
col.select.analyse <- data.composed %>% select(starts_with(pattern.select)) %>% colnames
col.select.analyse.integer <- int.questions

res_all <- map(group_var, 
               \(x) {
                 col.select.analyse.not.in.group <- col.select.analyse
                 if (all(x!="")) {
                   col.remove <- data.composed %>% select(starts_with(x)) %>% colnames
                   col.select.analyse.not.in.group <- col.select.analyse %>%  keep(!. %in% col.remove)
                 }
                 if (any(x=="") & length(x)==1) x<-NULL ## if no disag, replace group_var by NULL for analyse() to work
                 map(1:length(x), \(i) assign(paste0("x", i), x[i], envir = .GlobalEnv))
                 
                 res <- data.composed %>% 
                   mutate(across(any_of(x) %>% keep(~is.numeric(.x)), as.character)) %>% ## mutate as character all grouping vars in x are numeric
                   analyse_all(var_select = col.select.analyse.not.in.group, 
                               var_int = col.select.analyse.integer,
                               group_var = x) %>% ungroup
                 
                 ## create grouping var columns if any
                 if (!is.null(x)){
                   map(1:length(x), \(i) {
                     res <<- res %>% 
                       mutate(!!sym(paste0("group_var_", i)) := !!sym(paste0("x", i)),
                              !!sym(paste0("group_var_val_", i)) := !!sym(get(paste0("x", i)))) %>% 
                       select(-any_of(get(paste0("x", i))))
                   }) %>% invisible()
                 }
                 
                 res <- res %>%
                   left_join(survey_combined %>% select(question=name, question_label=col_lab, choice=choice_name, choice_label)) %>%
                   select(starts_with("question"), starts_with("choice"), everything())
                 
                 return(res)
               })

res_all_bind <- bind_rows(res_all) %>% select(starts_with("group_"), everything())

## join labels for all group_var and group_val using survey_combined
res_all_bind_label <- res_all_bind %>%
  left_join(survey_combined %>% filter(!is.na(!!sym(col_lab))) %>% select(group_var_1=name, group_var_1_lab=col_lab, group_var_val_1=choice_name, group_var_val_1_lab=choice_label)) %>%
  left_join(survey_combined %>% filter(!is.na(!!sym(col_lab))) %>% select(group_var_2=name, group_var_2_lab=col_lab, group_var_val_2=choice_name, group_var_val_2_lab=choice_label)) %>%
  left_join(tool %>% select(question=name, short_label) %>% filter(!is.na(question) , !is.na(short_label))) %>% relocate(short_label, .after="question_label") %>%
  select(matches("group_var(|_val)_1"), matches("group_var(|_val)_2"), everything()) 

### Gender and disability gap analysis ######################################

## put aside results when group var 1 is not gender or wgq_2
res_all_bind_label_no_gap <- res_all_bind_label %>% 
  filter((!group_var_1 %in% c("wgq_dis_1", "wgq_dis_2", "wgq_dis_3", "wgq_dis_4", "gender")) | is.na(group_var_1))

## Compute gender gap for all grouping levels when only 2 cross disag maximum
## For gender/disab gap, the gender/disab disag should be in group_var_1 with this code
## compute the gender gap for each question, defined as:
## the value of prop for group_var_val_1=="women" prop for group_var_val_1== "men" all of this divided by value for group_var_val_1=="men"
## for each question / group_var_val_1 / choice

## Define variables for which Gender / disability gap should be calculated
var.gap <- tool %>% 
  filter(row_number()>which(name=="phone_access"), row_number()<which(name=="phone_use")) %>%
  filter(q.type %in% c("select_one", "select_multiple")) %>% 
  pull(name) %>% append(c("phone_access_and_ownership", "access_internet_enabled_device"))

res_all_bind_label_gender_gap <- res_all_bind_label %>%
  filter(group_var_1=="gender") %>%
  group_by(question, group_var_2, group_var_val_2, choice) %>%
  mutate(
    gender_women = mean(ifelse(group_var_val_1=="female" & question %in% var.gap, prop, NA), na.rm=T),
    gender_men = mean(ifelse(group_var_val_1=="male" & question %in% var.gap, prop, NA), na.rm=T),
    gender_gap = ifelse(length(gender_women) > 0 & length(gender_men) > 0 & question %in% var.gap,
                        (gender_men-gender_women) / gender_women,
                        NA),
    across(starts_with("gender_"), ~ifelse(is.nan(.), NA, .))) %>%
  ungroup()

## for all three disability threshold, compute disability gap
res_all_bind_label_disability_gap <- res_all_bind_label %>% 
  filter((group_var_1 %in% c("wgq_dis_1", "wgq_dis_2", "wgq_dis_3", "wgq_dis_4"))) %>% ## filter to keep only wgq_2
  group_by(question, group_var_1, group_var_2, group_var_val_2, choice) %>%
  mutate(disability_yes = mean(ifelse(group_var_val_1=="1" & question %in% var.gap, prop, NA), na.rm=T),
         disability_no = mean(ifelse(group_var_val_1=="0" & question %in% var.gap, prop, NA), na.rm=T),
         disability_gap = ifelse(length(disability_yes) > 0 & length(disability_no) > 0 & question %in% var.gap, ## calculate disability gap
                                 (disability_no-disability_yes) / disability_yes, 
                                 NA),
         across(starts_with("disability_"), ~ifelse(is.nan(.), NA, .)))

## Bind all results together
res_all_bind_final <- bind_rows(
  res_all_bind_label_no_gap,
  res_all_bind_label_gender_gap,
  res_all_bind_label_disability_gap
)

## save all results
dir.create("output", showWarnings = F)
fwrite(res_all_bind_final, paste0("output/all_results_", Sys.Date(), ".csv"))


### OLD CODE
# result <- data.composed %>% analyse(var = col.select.analyse) %>%
#   left_join(survey_combined %>% select(question=name, question_label=col_lab, choice=choice_name, choice_label)) %>%
#   select(starts_with("question"), starts_with("choice"), everything())

# res_all <- map(group_var, 
#     \(x) {
#       data.composed %>% analyse(var = col.select.analyse, group_var = x) %>%
#         mutate(group_var=x, group_var_val=!!sym(x)) %>% select(-any_of(x)) %>%
#         left_join(survey_combined %>% select(question=name, question_label=col_lab, choice=choice_name, choice_label)) %>%
#         select(starts_with("question"), starts_with("choice"), everything())})
#       
# res_all_bind <- bind_rows(res_all) %>% select(starts_with("group_"), everything())

# group_var_2 <- list(
#   "", 
#   "admin1", 
#   "admin2", 
#   "displ_arrival_duration_recoded", 
#   "displacement_status",
#   "wgq_dis_2",
#   "wgq_dis_3",
#   
#   ## for gender gap calculation by admin 1/ wgq level 2 / displacement
#   "gender",
#   c("gender", "admin1"),
#   c("gender", "wgq_dis_2"),
#   c("gender", "displ_arrival_duration_recoded"),
#   
#   ## for disability gap calculation by admin 1/ displacement
#   c("wgq_dis_2", "admin1"),
#   c("wgq_dis_2", "displ_arrival_duration_recoded")
# )

# res_all <- map(group_var, 
#                \(x) {
#                  col.select.analyse.not.in.group <- col.select.analyse
#                  if (all(x!="")) {
#                    col.remove <- data.composed %>% select(starts_with(x)) %>% colnames
#                    col.select.analyse.not.in.group <- col.select.analyse %>%  keep(!. %in% col.remove)
#                  }
#                  if (any(x=="") & length(x)==1) x<-NULL ## if no disag, replace group_var by NULL for analyse() to work
#                  map(1:length(x), \(i) assign(paste0("x", i), x[i], envir = .GlobalEnv))
#                  
#                  res <- data.composed %>% 
#                    mutate(across(any_of(x) %>% keep(~is.numeric(.x)), as.character)) %>% ## mutate as character all grouping vars in x are numeric
#                    analyse(var = col.select.analyse.not.in.group, group_var = x) %>% ungroup
#                  
#                  ## create grouping var columns if any
#                  if (!is.null(x)){
#                    map(1:length(x), \(i) {
#                      res <<- res %>% 
#                        mutate(!!sym(paste0("group_var_", i)) := !!sym(paste0("x", i)),
#                               !!sym(paste0("group_var_val_", i)) := !!sym(get(paste0("x", i)))) %>% 
#                        select(-any_of(get(paste0("x", i))))
#                    }) %>% invisible()
#                  }
#                  
#                  res <- res %>%
#                    left_join(survey_combined %>% select(question=name, question_label=col_lab, choice=choice_name, choice_label)) %>%
#                    select(starts_with("question"), starts_with("choice"), everything())
#                  
#                  return(res)
#                })
