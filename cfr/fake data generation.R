rm(list=ls())
require(pacman)
p_load(readxl, dplyr, tidyr, stringr, purrr, data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("utils.R")

## Read tool 
path.tool <- "../ETH_Toolkit/v6/Tool 1 - End user survey (ETH reviewed) v6.xlsx"
tool <- read_excel(path.tool, sheet = "survey") %>% separate(type, int=c("q.type", "list_name"), sep=" ", remove = F) %>% mutate(row=row_number())
choices <- read_excel(path.tool, sheet = "choices")
tool.other <- tool %>% filter(!str_detect(q.type, "select_"))
col_lab <- "label::English (en)"
col_lab2 <- "label::Arabic (ar)"
tool.choices <- tool %>% 
  filter(str_detect(q.type, "select_")) %>% 
  left_join(choices %>% select(list_name, choice_name=name, choice_label=any_of(col_lab), choice_label_ar=any_of(col_lab2)), by="list_name") %>%
  bind_rows(tool.other) %>% arrange(row)

#### generate fake data
n_rows <- 1000
q.one <- tool %>% filter(q.type=="select_one") %>% pull(name)
q.mul <- tool %>% filter(q.type=="select_multiple") %>% pull(name)
q.int <- tool %>% filter(q.type=="integer") %>% pull(name)
q.dec <- tool %>% filter(q.type=="decimal") %>% pull(name)

## for each select_one generate a vector of size n_rows with random values coming from tool.choices %>% filter(name) %>% pull(choice_name)
## create data.frame with n_rows but no columns
data.cfr <- data.frame(test=rep(NA, n_rows))
for (x in q.one %>% str_subset("pcode", negate = T)){
  data.cfr <- data.cfr %>% mutate(!!sym(x) := sample(tool.choices %>% filter(name==x) %>% pull(choice_name), n_rows, replace = TRUE))
}
for (x in q.mul){
  all.choices <- tool.choices %>% filter(name==x) %>% pull(choice_name)
  data.cfr <- data.cfr %>% mutate(!!sym(x) := map_chr(1:n(), ~str_c(sample(all.choices, size = sample(1:length(all.choices), 1)), collapse = " ")))
}

## expand select multiple child columns 
data.cfr <- data.cfr %>% expand.select.multiple.vec(q.mul, sep=".")

for (x in c(q.dec, q.int)){
  const <- tool %>% filter(name==x) %>% pull(constraint)
  if (!is.na(const)) {
    if (str_detect(const, "\\.>")) min_max <- str_extract_all(const, "(?<=\\.>(|=))\\d(\\d|)(\\d|)(\\d|)(\\.|)(\\d|)(\\d|)(?=\\b)") %>% as.numeric() else min_max <- 0
    if (str_detect(const, "\\.<")) min_max[2] <- str_extract_all(const, "(?<=\\.<(|=))\\d(\\d|)(\\d|)(\\d|)(\\d|)(\\d|)(\\d|)(\\.|)(\\d|)(\\d|)(?=\\b)") %>% as.numeric() else min_max[2] <- 100
    ## generate a random number using uniform distribution between min_max[1] and min_max[2]
  } else {min_max <- c(0, 100)}
  data.cfr <- data.cfr %>% 
    mutate(!!sym(x) := sample(min_max[1]:min_max[2], n_rows, replace=T)) ## random integer between min_max[1] and min_max[2]
    # mutate(!!sym(x) := runif(n_rows, min_max[1], min_max[2]))
}

## pull all calculate from the tool and compute the new variables accoringly
calc <- tool %>% filter(q.type=="calculate") %>% pull(name)
calc.form <- tool %>% filter(q.type=="calculate") %>% pull(calculation)

data.cfr <- data.cfr %>% 
  mutate(admin3_pcode=sample(tool.choices %>% filter(name=="admin3") %>% pull(choice_name), n_rows, replace = TRUE),
         admin2_pcode=substring(admin3_pcode, 1, 6), admin1_pcode=substring(admin3_pcode, 1, 4)) %>%
  mutate(across(matches("price_unit_item$"), ~., .names = "{str_replace_all(.col, '_unit', '')}")) %>%
  left_join(choices %>% filter(str_detect(list_name, "admin1")) %>% select(admin1_pcode=name, admin1_label=any_of(col_lab))) %>%
  left_join(choices %>% filter(str_detect(list_name, "admin2")) %>% select(admin2_pcode=name, admin2_label=any_of(col_lab))) %>%
  left_join(choices %>% filter(str_detect(list_name, "admin3")) %>% select(admin3_pcode=name, admin3_label=any_of(col_lab)))

## randomly generate today, "_submission_time", start, end, _id, _uuid not using data_lby
data.cfr <- data.cfr %>%
  mutate(
    today = sample(seq(Sys.Date(), (Sys.Date()-3), by="-1 day"), n_rows, replace = TRUE),
    `_submission_time` = paste0(sample(seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2021-01-01 23:59:59"), by="hour"), n_rows, replace = TRUE), 1:n_rows, sep="_"),
    start = sample(seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2021-01-01 23:59:59"), by="hour"), n_rows, replace = TRUE),
    end = sample(seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2021-01-01 23:59:59"), by="hour"), n_rows, replace = TRUE),
    `_id` = 1:n_rows,
    `_uuid` = paste(sample(letters, n_rows, replace = TRUE), 1:n_rows, sep="_")
  )
 
## mutate date of arrival
data.cfr <- data.cfr %>%
 mutate(
   ## generate randome date from today to today - 5years 
   displ_arrival = sample(seq(Sys.Date()-3, Sys.Date()-3 - 365*5, by="-1 day"), n_rows, replace = TRUE)
 )


## write fake data 
fwrite(data.cfr, "data/data_jmmi_fake_data.csv")
