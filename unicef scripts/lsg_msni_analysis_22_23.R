require(pacman)
p_load(broom)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

options(scipen = 999)  # Avoids scientific notation


data_22 <- data.table::fread("data/offline_dataset/full_dataset_22.csv")
data_22 <- data_22 %>% mutate(country=toupper(country), weight=weights,
                              wgq_dis_3_at_least_one=str_replace_all(any_difficulty_hh, c("at least one member"="At least one", "no difficulty"="None")))

data_23 <- data.table::fread("data/offline_dataset/hh_data_23.csv")
data_23 <- data_23 %>%
  mutate(country_old=country, country=str_replace_all(country, c("-.*"="")), year=2023,
         pop_displaced = case_when(pop_group_cat %in% c("idp", "other", "refugee") ~ "Displaced", pop_group_cat %in% "non_displaced" ~ "Non-displaced"))

common_col <- intersect(names(data_22), names(data_23)) %>% sort

data_all <- plyr::rbind.fill(data_22, data_23)

data_all_wgq_na <- data_all %>% filter(!wgq_dis_3_at_least_one %in% c("At least one", "None"))
check_data_all_wgq_na <- data_all_wgq_na %>% count(country, year, wgq_dis_3_at_least_one)
data_all <- data_all %>% filter(wgq_dis_3_at_least_one %in% c("At least one", "None"))

## compute analysis table at hh level
col_lsg <- c("msni", "fs_lsg", "wash_lsg", "snfi_lsg", "edu_lsg", "health_lsg", "liv_lsg")
col_lsg_need <- col_lsg %>% paste0("_need")
col_lsg_extr_need <- col_lsg %>% keep(!. %in% "health_lsg") %>% paste0("_extr_need")

col_lsg_sec <- col_lsg %>% keep(!. %in% "msni")

## compute _need and _extr_need
data_all <- data_all %>%
  mutate(
    across(all_of(col_lsg), ~case_when(. >= 3 ~ 1, . < 3 ~ 0, TRUE ~ NA_real_), .names = "{col}_need"),
    across(all_of(col_lsg), ~case_when(. >= 4 ~ 1, . < 4 ~ 0, TRUE ~ NA_real_), .names = "{col}_extr_need"),
    wgq_dis_3 = case_when(wgq_dis_3_at_least_one=="At least one" ~ 1, wgq_dis_3_at_least_one=="None" ~ 0, TRUE ~ NA_real_)
  )

## get # of non NA lsg by country / year to then compute % sectors in need, then rescale
nb_lsg <- data_all %>% group_by(country, year) %>% summarise(across(all_of(col_lsg_sec), ~sum(!is.na(.)))) %>%
  ungroup %>% mutate(nb_lsg = rowSums(across(any_of(col_lsg_sec), ~.>0)))
data_all <- data_all %>% select(-any_of("nb_lsg")) %>% left_join(nb_lsg %>% select(country, year, nb_lsg)) %>%
  mutate(
    nb_need = rowSums(across(all_of(col_lsg_sec), ~.>=3), na.rm=T),
    nb_need_adjusted = nb_need/nb_lsg*6
  )

## aggregate col_lsg_need + col_lsg_extr_need disaggregating by country, year and wgq_dis_3_at_least_one
source("custom raph/utils.R")

## all of it
add.lab <- function(df){df %>% mutate(question_lab = str_replace_all(question, c("_"=" ",
                                                                               "extr need"="extra need",
                                                                               "fs"="food",
                                                                               "liv"="livelihood",
                                                                               "snfi"="shelter",
                                                                               "edu"="education",
                                                                               "nb_need_adjusted"="# sectors in need")),
                                      question_metric = str_replace_all(question_lab, c("extra need"="≥ 4", "need"="≥ 3"))
                                      )}

res <- data_all %>% analyse_ci(group_var = c("country", "year"), var = c(col_lsg_need, col_lsg_extr_need, "wgq_dis_3", "nb_need_adjusted"), col_weight = "weight")
res.wgq <- data_all %>% analyse_ci(group_var = c("country", "year", "wgq_dis_3_at_least_one"), var = c(col_lsg_need, col_lsg_extr_need, "nb_need_adjusted"), col_weight = "weight")
res.wgq <- res.wgq %>% group_by(country, year, question) %>%
  mutate(min_y = min(mean, na.rm=T), max_color = first(wgq_dis_3_at_least_one[order(-mean)], default = NA_character_), sum_count=sum(count)) %>% ungroup %>%
  filter(sum_count>0) %>%  add.lab ## filter out values of lsg that are not possible for country year (count=0) + add var for graph
res.wgq.2 <- data_all %>% filter(year==2023) %>% analyse_ci(group_var = c("country", "year", "wgq_dis_2_at_least_one"), var = c(col_lsg_need, col_lsg_extr_need, "nb_need_adjusted"), col_weight = "weight")
res.wgq.2 <- res.wgq.2 %>% group_by(country, year, question) %>%
  mutate(min_y = min(mean, na.rm=T), max_color = first(wgq_dis_2_at_least_one[order(-mean)], default = NA_character_), sum_count=sum(count)) %>% ungroup %>%
  filter(sum_count>0) %>%  add.lab ## filter out values of lsg that are not possible for country year (count=0) + add var for graph

## by displacement as well
res.wgq.displaced <- data_all %>%
  analyse_ci(group_var = c("country", "year", "wgq_dis_3_at_least_one", "pop_displaced"), var = c(col_lsg_need, col_lsg_extr_need, "nb_need_adjusted"), col_weight = "weight")
res.wgq.displaced <- res.wgq.displaced %>% group_by(country, year, question, pop_displaced) %>%
  mutate(min_y = min(mean, na.rm=T), max_color = first(wgq_dis_3_at_least_one[order(-mean)], default = NA_character_), sum_count=sum(count)) %>% ungroup %>%
  filter(sum_count>0) %>%  add.lab ## filter out values of lsg that are not possible for country year (count=0) + add var for graph

## write result table
data.table::fwrite(res, "output/result/msni_lsg_22_23.csv")
data.table::fwrite(res.wgq, "output/result/msni_lsg_wgq_22_23.csv")
data.table::fwrite(res.wgq.2, "output/result/msni_lsg_wgq_2_22_23.csv")
data.table::fwrite(res.wgq.displaced, "output/result/msni_lsg_wgq_displaced_22_23.csv")

################################################################################
## read again all res files if you don't want to rerun it all
res <- data.table::fread("output/result/msni_lsg_22_23.csv")
res.wgq <- data.table::fread("output/result/msni_lsg_wgq_22_23.csv")
res.wgq.2 <- data.table::fread("output/result/msni_lsg_wgq_2_22_23.csv")
res.wgq.displaced <- data.table::fread("output/result/msni_lsg_wgq_displaced_22_23.csv")

res.wgq.all <- bind_rows(
  res.wgq %>% mutate(wgq_level=3, wgq_dis_at_least_one=wgq_dis_3_at_least_one),
  res.wgq.2 %>% mutate(wgq_level=2, wgq_dis_at_least_one=wgq_dis_2_at_least_one)
)

## quick plot of prev msni against prev dis_3
## plot it up
## use ggplot2, with geom_segment and geom_point to have a point for wgq_dis_3_at_least_one=="At least one" (in red) and another point in blue for wgq_dis_3_at_least_one=="None"
## Also theme_minimal, scale_y_continuous with percentage labels, using as x axis the column question, and with facet country x year,

## set order of question to have msni, then col_lsg, isolate unique val and mutate as factor with corresponding levels
res.wgq.all <- res.wgq.all %>% mutate(type=str_replace_all(question, "_extr|_need", "") %>%
                                        factor(levels=c("msni", "nb_need_adjusted", "health_lsg", "fs_lsg", "edu_lsg", "liv_lsg", "wash_lsg", "snfi_lsg")))
lev.question <- res.wgq %>% pull(question_metric) %>% unique
res.wgq.all <- res.wgq.all %>% mutate(question_metric=factor(question_metric, levels=lev.question))

## filter year (for comparative severity 2/3, only 2023, otherwise put c(2022, 2023))
years<-c(2022,2023)
# for (level in 3) {
#   res.wgq_filtered <- res.wgq.all %>% filter(wgq_level==level, year %in% years)
#   plot.seg <- res.wgq_filtered %>% filter(question != "nb_need_adjusted") %>% loliplot(s.l = level)
#   plot.seg.need <- res.wgq_filtered %>% filter(!str_detect(question, "lsg_extr_need|nb_need_adjusted")) %>% loliplot(s.l = level)
#   plot.seg.extr.need <- res.wgq_filtered %>% filter(!str_detect(question, "lsg_need")) %>% loliplot(s.l = level)
#   plot.seg.need.by.ind <- res.wgq_filtered %>% filter(!str_detect(question, "lsg_extr_need|nb_need_adjusted")) %>%
#     loliplot(facet="question_metric", col_x = "country_facet", n.col = 4, s.l = level) + labs(x="", y="% of HHs")
#   plot.seg.need.by.higher <- res.wgq_filtered %>% filter(str_detect(question, "(health|fs|liv)_lsg_need")) %>%
#     loliplot(facet="question_metric", col_x = "country_facet", n.col = 1, s.l = level) + labs(x="", y="% of HHs")
#   plot.seg.need.by.lower <- res.wgq_filtered %>% filter(str_detect(question, "(edu|snfi|wash)_lsg_need")) %>%
#     loliplot(facet="question_metric", col_x = "country_facet", n.col = 1, s.l = level) + labs(x="", y="% of HHs")
#   plot.seg.need.msni <- res.wgq_filtered %>% filter(str_detect(question, "msni.*_need")) %>%
#     loliplot(facet="question_metric", col_x = "country_facet", n.col = 1, s.l = level) + labs(x="", y="% of HHs")
#   plot.seg.need.nb.sectors <- res.wgq_filtered %>% filter(str_detect(question, "nb_need_adjusted")) %>%
#     loliplot(facet="question_metric", col_x = "country_facet", n.col = 1, s.l = level) + labs(x="", y="Average # of sectoral gaps") +
#     scale_y_continuous(limits=c(0,4), breaks=0:6, labels=0:6) ## discrete labels from 0 to 6
#
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_all_",level,".png"), plot.seg, width=15, height=14, units="in", dpi=300, bg="white")
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_need_",level, ".png"), plot.seg.need, width=15, height=10, units="in", dpi=300, bg="white")
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_extr_need_",level, ".png"), plot.seg.extr.need, width=15, height=10, units="in", dpi=300, bg="white")
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_need_ind_",level, ".png"), plot.seg.need.by.ind, width=11, height=8, units="in", dpi=300, bg="white")
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_need_higher_",level, ".png"), plot.seg.need.by.higher, width=8, height=8, units="in", dpi=300, bg="white")
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_need_lower_",level, ".png"), plot.seg.need.by.lower, width=8, height=8, units="in", dpi=300, bg="white")
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_need_msni_",level, ".png"), plot.seg.need.msni, width=8, height=8, units="in", dpi=300, bg="white")
#   ggsave(paste0("output/graph_wgq/prevalence difference loliplot/msni_lsg_wgq_22_23_need_nb_sectors_",level, ".png"), plot.seg.need.nb.sectors, width=8, height=8, units="in", dpi=300, bg="white")
# }

#### pivot wider wgq_dis_3_at_least_one for value mean, count, and then join res %>% filter(question=="msni_extr_need") select(country, mean_msni_extr_need=mean)
res.wgq.wide <- res.wgq %>% select(-min_y, -sum_count, -analysis_key) %>%
  pivot_wider(names_from = wgq_dis_3_at_least_one, values_from = c(mean, count, n)) %>%
  mutate(prevalence_difference = `mean_At least one`-mean_None) %>%
  left_join(res %>% filter(question=="msni_extr_need") %>% select(country, mean_msni_extr_need=mean))

## other strategy, I want for each indicator, compute the difference between at_least_one and none, then plot this difference against the mean of the indicator
res.diff <- res.wgq %>% select(-min_y, -sum_count, -analysis_key) %>%
  arrange(country, year, question, wgq_dis_3_at_least_one) %>%
  group_by(country, year, question, question_lab) %>%
  summarise(prevalence_difference = mean(mean[wgq_dis_3_at_least_one=="At least one"]-mean[wgq_dis_3_at_least_one=="None"])) %>%
  ungroup() %>% left_join(res %>% select(country, year, mean, question)) %>%
  mutate(var_lab = paste0(toupper(str_sub(question_lab, 1, 1)), str_sub(question_lab, 2, -1)))

## visulaise prevalence difference quickly by sector
var <- "fs_lsg_need"

all.plot.point <- map(unique(res.diff$question), ~plot.diff.point(var=.)) %>% setNames(unique(res.diff$question))

# for FS, then health need and livelihood need get the graphs
plot.difference.all <- (all.plot.point$msni_need + all.plot.point$msni_extr_need) /
  (all.plot.point$health_lsg_need + all.plot.point$fs_lsg_need ) /
  (all.plot.point$liv_lsg_need + all.plot.point$liv_lsg_extr_need) +
  plot_annotation(title = str_wrap("Difference in prevalence between households with member(s) with disability and other households", 50),
                  theme=theme(plot.title = element_text(hjust = 0.5)))
ggsave("output/graph_wgq/plot.prev.diff.all.png", plot.difference.all, width=11, height=11, units="in", dpi=300, bg="white")

# for (t in as.character(unique(res.diff$type))){
#   t <- as.character(unique(res.diff$type))[1]
#   vars <- grep(t, names(all.plot.point), perl = T, value = T)
#   # from vars[1] to vars[length(vars)] do a patchwork plot doing all.plot.point[[vars[1]]] + all.plot.point[[vars[2]]] + ... + all.plot.point[[vars[length(vars)]]]
#   plot.vars <- map(vars, ~all.plot.point[[.]]) %>% setNames(vars)
#   patchwork::wrap_plots(plot.vars)
# }

for (x in unique(res.diff$question)){
  plot.v <- plot.diff(var=x)
  ggsave(paste0("output/graph_wgq/prevalence diff/msni_lsg_wgq_22_23_diff_", x, ".png"), plot.v, width=8, height=8, units="in", dpi=300, bg="white")
}

### Trying to look at regression [probabilty of being in MSNI 4+ given at At least one vs None] controlling for other things
### pooled OLS
list_lab_all <- c("MSNI 4+",
                  "# sectors in need",
                  "Health LSG need",
                  "Livelihood LSG need",
                  "Food LSG need",
                  "Education LSG need",
                  "Shelter LSG need",
                  "WASH LSG need")

data_all_model <- data_all %>%
  mutate(wgq_dis_lev = case_when(
    wgq_dis_4_at_least_one %in% c(1, "1", "At least one") ~ 4,
    wgq_dis_3_at_least_one %in% c(1, "1", "At least one") ~ 3,
    wgq_dis_2_at_least_one %in% c(1, "1", "At least one") ~ 2,
    wgq_dis_1_at_least_one %in% c(1, "1", "At least one") ~ 1,
    wgq_dis_1_at_least_one %in% c(0, "0", "None") ~ 0,
    T ~ NA_real_
  ),
         wgq_dis_3_at_least_one=case_when(wgq_dis_3_at_least_one=="At least one" ~ 1, wgq_dis_3_at_least_one=="None" ~ 0, TRUE ~ NA_real_),
         pop_displaced=case_when(pop_displaced=="Displaced" ~ 1, pop_displaced=="Non-displaced" ~ 0, TRUE ~ NA_real_))

predictors <- c("wgq_dis_3_at_least_one", "pop_displaced", "hoh_age", "country", "year")
# predictors <- c("wgq_dis_lev", "pop_displaced", "hoh_age", "country", "year")

m_1 <- lm(as.formula(paste0("msni_extr_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_1_1 <- lm(as.formula(paste0("msni_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_1_2 <- lm(as.formula(paste0("nb_need_adjusted ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_7 <- lm(as.formula(paste0("health_lsg_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_3 <- lm(as.formula(paste0("liv_lsg_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_2 <- lm(as.formula(paste0("fs_lsg_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_5 <- lm(as.formula(paste0("edu_lsg_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_6 <- lm(as.formula(paste0("snfi_lsg_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)
m_4 <- lm(as.formula(paste0("wash_lsg_need ~ ", paste0(predictors, collapse=" + "))), data = data_all_model, weights = weight)

## extract r2 from m_1
p_load(stargazer)

list_lab_all_msni <- c(list_lab_all[1], "MSNI 3+", list_lab_all[-1])

model_table_lsg <- stargazer::stargazer(m_1, m_1_1, m_1_2, m_7, m_3, m_2, m_5, m_6, m_4, type="text", title="Pooled OLS model",
                                        subtitle="Coefficient are average increase/decrease in predicted probability of being in need as measured by the corresponding indicator in dependent variable",
                                        out="output/result/regression_lsg_need_wgq_OLS_pooled.html", align=TRUE, single.row=TRUE, header=FALSE,
                                        dep.var.labels = list_lab_all_msni, dep.var.labels.include = TRUE)

## get the coefficients with broom
model.id <- data.frame(model_lab=list_lab_all_msni, model=as.character(1:9)) %>%
  mutate(r_squared=lapply(list(m_1, m_1_1, m_1_2, m_7, m_3, m_2, m_5, m_6, m_4), function(x) summary(x)$r.squared) %>% unlist)
coef_pool <- list(m_1, m_1_1, m_1_2, m_7, m_3, m_2, m_5, m_6, m_4) %>% map_df(~tidy(.), .id="model") %>% left_join(model.id)

plot.coef_pool <- coef_pool %>% filter(term=="wgq_dis_3_at_least_one", model_lab!="# sectors in need") %>%
  mutate(model_lab = paste0(model_lab, "\nR²=",round(100*r_squared, 0), "%")) %>%
  ggplot(aes(x=reorder(model_lab, estimate), y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_errorbar(aes(linetype = "95% CI"), width=0.2) + geom_point(aes(colour = "Estimate")) +
  scale_y_continuous(limits = c(0,0.15)) +
  scale_colour_manual(values = c("Estimate"="indianred")) + scale_linetype_manual(values=c("95% CI"="solid")) +
  coord_flip() + theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
  labs(title=paste0("OLS coefficient - by predicated indicator"),
       subtitle = str_wrap(paste0("Increase in predicted dependent variable if HH has at least one individual with WG-SS 3"), width=55),
       y="Coefficient value", x="Dependent variable\n", caption = "Error bars represent 95% confidence intervals.\n", colour = NULL, linetype = NULL)
ggsave("output/result/plot_coef_pool_all.png", plot.coef_pool, width=8, height=7, units="in", bg="white")


## do the same than above with glm (logit)
m_1g <- glm(as.formula(paste0("msni_extr_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
m_1_1g <- glm(as.formula(paste0("msni_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
# m_1_2g <- glm(as.formula(paste0("nb_need_adjusted ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
m_2g <- glm(as.formula(paste0("fs_lsg_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
m_3g <- glm(as.formula(paste0("liv_lsg_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
m_4g <- glm(as.formula(paste0("wash_lsg_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
m_5g <- glm(as.formula(paste0("edu_lsg_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
m_6g <- glm(as.formula(paste0("snfi_lsg_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)
m_7g <- glm(as.formula(paste0("health_lsg_need ~ ", paste0(predictors, collapse=" + "))), family=binomial(link="logit"), data = data_all_model, weights = weight)

## compute marginal average effect for all models and then put it in a table with broom
p_load(marginaleffects, gt, pscl)

## get pR2 pseudo r2 for all models above
pseudo_r2 <- list(m_1g, m_1_1g, m_2g, m_3g, m_4g, m_5g, m_6g, m_7g) %>% map(\(x) {
  x$model$weight <- x$model$`(weights)`
  return(pR2(x)["McFadden"])}) %>% unlist %>% unname

# lab_mod_marg <- list_lab_all_msni %>% keep(!. %in% "# sectors in need")
lab_mod_marg <- c("MSNI 4+", "MSNI 3+", "Food LSG need", "Livelihood LSG need", "WASH LSG need", "Education LSG need", "Shelter LSG need", "Health LSG need")

m_1g_marg <- avg_slopes(m_1g) %>% mutate(model=lab_mod_marg[1])
m_1_1g_marg <- avg_slopes(m_1_1g) %>% mutate(model=lab_mod_marg[2])
m_2g_marg <- avg_slopes(m_2g) %>% mutate(model=lab_mod_marg[3])
m_3g_marg <- avg_slopes(m_3g) %>% mutate(model=lab_mod_marg[4])
m_4g_marg <- avg_slopes(m_4g) %>% mutate(model=lab_mod_marg[5])
m_5g_marg <- avg_slopes(m_5g) %>% mutate(model=lab_mod_marg[6])
m_6g_marg <- avg_slopes(m_6g) %>% mutate(model=lab_mod_marg[7])
m_7g_marg <- avg_slopes(m_7g) %>% mutate(model=lab_mod_marg[8])

# ## predict average probability for m_7g for hh with wgq_dis_3_at_least_one==0 and wgq_dis_3_at_least_one==1
# # Extract the original dataset used for fitting the model
# newdata_avg <- model.frame(m_7g)
#
# # Replace only the `wgq_dis_3_at_least_one` variable with 0 and 1 separately
# newdata_avg_0 <- newdata_avg
# newdata_avg_0$wgq_dis_3_at_least_one <- 0
#
# newdata_avg_1 <- newdata_avg
# newdata_avg_1$wgq_dis_3_at_least_one <- 1
#
# # Predict while averaging over the dataset
# m_7g_pred_0 <- mean(predict(m_7g, newdata = newdata_avg_0, type="response"))
# m_7g_pred_1 <- mean(predict(m_7g, newdata = newdata_avg_1, type="response"))
#
# # Print the results
# m_7g_pred_0
# m_7g_pred_1
# m_7g_pred_1-m_7g_pred_0

# List of model names
marg_list <- list(
  m_1g_marg %>% filter(term=="wgq_dis_3_at_least_one"),
  m_1_1g_marg %>% filter(term=="wgq_dis_3_at_least_one"),
  m_2g_marg %>% filter(term=="wgq_dis_3_at_least_one"),
  m_3g_marg %>% filter(term=="wgq_dis_3_at_least_one"),
  m_4g_marg %>% filter(term=="wgq_dis_3_at_least_one"),
  m_5g_marg %>% filter(term=="wgq_dis_3_at_least_one"),
  m_6g_marg %>% filter(term=="wgq_dis_3_at_least_one"),
  m_7g_marg %>% filter(term=="wgq_dis_3_at_least_one")
)

# marginaleffects::plot_slopes(m_1_1g, variables = "wgq_dis_3_at_least_one", condition="pop_displaced")

# Convert each AME object to a data frame and merge them
mfx_table <- bind_rows(marg_list) %>% mutate(pseudo_r2 = pseudo_r2)

# Rename columns
p <- mfx_table %>%
  mutate(model = paste0(model, "\nR²*=", round(100*pseudo_r2, 0), "%")) %>%
  ggplot(aes(x=reorder(model, estimate), y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
  geom_errorbar(aes(linetype = "95% CI"), width=0.2) + geom_point(aes(colour = "Estimate")) +
  scale_colour_manual(values = c("Estimate"="indianred")) + scale_linetype_manual(values=c("95% CI"="solid")) +
  coord_flip() + theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
  scale_y_continuous(limits=c(0,0.15)) +
  labs(title=paste0("Pooled GLM (logit) regression"),
       subtitle = str_wrap(paste0("Average marginal effect - Increase in predicted variable\n if HH has at least one individual with WG-SS 3"), width=55),
       y="Coefficient value", x="Dependent variable\n", caption = "Error bars represent 95% confidence intervals.\n* Mc Fadden pseudo R² displayed", colour = NULL, linetype = NULL)
#   p
ggsave(paste0("output/result/coef_pooled_glm.png"), p, bg="white", width=8, height=5, units="in")

## re run the above, changing the predictors to add average y value (exluding own observation)
## idea is to check if in low prevalence countries, HH with disability  are more likely to experience need than other HH
data_all_model <- data_all %>%
  mutate(wgq_dis_3_at_least_one=case_when(wgq_dis_3_at_least_one=="At least one" ~ 1, wgq_dis_3_at_least_one=="None" ~ 0, TRUE ~ NA_real_),
         pop_displaced=case_when(pop_displaced=="Displaced" ~ 1, pop_displaced=="Non-displaced" ~ 0, TRUE ~ NA_real_)) %>%
  group_by(country, year) %>%
  mutate(
    across(
      all_of(c("msni", "nb_need_adjusted", "fs_lsg", "liv_lsg", "wash_lsg", "edu_lsg", "snfi_lsg", "health_lsg")),
      ## weighted average excluding current observation
      ~ (sum(., na.rm=T)*weight - .)/(sum(weight, na.rm=T)-weight),
      # ~ weighted.mean(., wt=weight, na.rm=T),
      .names="mean_{.col}"
    )
  ) %>% ungroup

predictors <- c("wgq_dis_3_at_least_one", "pop_displaced", "hoh_age", "country", "year")

# mb_1 <- lm(as.formula(paste0("msni ~ ", paste0(c("mean_msni*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_1_1 <- lm(as.formula(paste0("msni_need ~ ", paste0(c("mean_msni*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_1_2 <- lm(as.formula(paste0("nb_need_adjusted ~ ", paste0(c("mean_nb_need_adjusted*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_2 <- lm(as.formula(paste0("fs_lsg ~ ", paste0(c("mean_fs_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_3 <- lm(as.formula(paste0("liv_lsg ~ ", paste0(c("mean_liv_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_4 <- lm(as.formula(paste0("wash_lsg ~ ", paste0(c("mean_wash_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_5 <- lm(as.formula(paste0("edu_lsg ~ ", paste0(c("mean_edu_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_6 <- lm(as.formula(paste0("snfi_lsg ~ ", paste0(c("mean_snfi_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)
mb_7 <- lm(as.formula(paste0("health_lsg ~ ", paste0(c("mean_health_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight)

model_table_lsg <- stargazer::stargazer(mb_1_1, mb_1_2, mb_7, mb_3, mb_2, mb_5, mb_6, mb_4, type="text", title="Pooled OLS model",
                                        subtitle="Coefficient are average increase/decrease in predicted probability of being in need as measured by the corresponding indicator in dependent variable",
                                        out="output/result/regression_lsg_wgq_OLS_pooled.html", align=TRUE, single.row=TRUE, header=FALSE,
                                        dep.var.labels = str_replace_all(list_lab_all, c(" 4\\+"="", " need"="")),
                                        dep.var.labels.include = TRUE)

## do the same than above with glm (logit)
mb_1_glm <- glm(as.formula(paste0("msni_need ~ ", paste0(c("mean_msni*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)
mb_1_2_glm <- glm(as.formula(paste0("nb_need_adjusted ~ ", paste0(c("mean_nb_need_adjusted*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)
mb_2_glm <- glm(as.formula(paste0("fs_lsg ~ ", paste0(c("mean_fs_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)
mb_3_glm <- glm(as.formula(paste0("liv_lsg ~ ", paste0(c("mean_liv_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)
mb_4_glm <- glm(as.formula(paste0("wash_lsg ~ ", paste0(c("mean_wash_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)
mb_5_glm <- glm(as.formula(paste0("edu_lsg ~ ", paste0(c("mean_edu_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)
mb_6_glm <- glm(as.formula(paste0("snfi_lsg ~ ", paste0(c("mean_snfi_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)
mb_7_glm <- glm(as.formula(paste0("health_lsg ~ ", paste0(c("mean_health_lsg*wgq_dis_3_at_least_one", predictors), collapse=" + "))), data = data_all_model, weights = weight, family=binomial)

model_table_lsg_glm <- stargazer::stargazer(mb_1_glm, mb_1_2_glm, mb_7_glm, mb_3_glm, mb_2_glm, mb_5_glm, mb_6_glm, mb_4_glm, type="text", title="Pooled GLM model",
                                        subtitle="Coefficient are average increase/decrease in predicted probability of being in need as measured by the corresponding indicator in dependent variable",
                                        out="output/result/regression_lsg_wgq_GLM_pooled.html", align=TRUE, single.row=TRUE, header=FALSE,
                                        dep.var.labels = str_replace_all(list_lab_all, c(" 4\\+"="", " need"="")),
                                        dep.var.labels.include = TRUE)


## Country level OLS
predictors.c <- c("wgq_dis_3_at_least_one", "pop_displaced", "hoh_age", "year")
list_lab_all <- c("MSNI 4+",
                  "MSNI 3+",
                  "# sectors in need",
                  "Health LSG need",
                  "Livelihood LSG need",
                  "Food LSG need",
                  "Education LSG need",
                  "Shelter LSG need",
                  "WASH LSG need")

lab.model <- data.frame(
  dependent=c(
    "msni_extr_need",
    "msni_need",
    "nb_need_adjusted",
    "health_lsg_need",
    "liv_lsg_need",
    "fs_lsg_need",
    "edu_lsg_need",
    "snfi_lsg_need",
    "wash_lsg_need"
  ),
  model_lab=model.id$model_lab
)

c.no.liv <- data_all %>% group_by(country) %>% summarise(n=sum(!is.na(liv_lsg))) %>% filter(n==0) %>% pull(country)
coef_all <- data.frame()

for (c in unique(data_all_model$country)){
 # c <- "DRC"
 # c <- unique(data_all_model$country)[10]
 # keep only predictors.c that are not all NA for the country
 na.pred <- data_all_model %>% filter(country==c) %>% select(predictors.c) %>% map(~sum(!is.na(.))==0) %>% keep(.==T) %>% unlist() %>% names() %>% as.character()
 predictors.c <- predictors.c %>% setdiff(na.pred)
 data_all_model_c <- data_all_model %>% filter(country==c)

 c_1 <- lm(as.formula(paste0("msni_extr_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 c_1_1 <- lm(as.formula(paste0("msni_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 c_1_2 <- lm(as.formula(paste0("nb_need_adjusted ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 c_2 <- lm(as.formula(paste0("fs_lsg_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 if (c %in% c.no.liv) c_3 <- NULL else c_3 <- lm(as.formula(paste0("liv_lsg_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 c_4 <- lm(as.formula(paste0("wash_lsg_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 c_5 <- lm(as.formula(paste0("edu_lsg_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 c_6 <- lm(as.formula(paste0("snfi_lsg_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)
 c_7 <- lm(as.formula(paste0("health_lsg_need ~ ", paste0(predictors.c, collapse=" + "))), data = data_all_model_c %>% filter(country==c), weights = weight)

 if (c %in% c.no.liv) list_lab <- list_lab_all %>% keep(!. %in% "Livelihood LSG need") else list_lab <- list_lab_all

 if (!c %in% c.no.liv) mdoel_table_lsg_c <- stargazer::stargazer(c_1, c_1_1, c_1_2, c_7, c_3, c_2, c_5, c_6, c_4, type="text", title=paste0("Pooled OLS model for ", c),
                                           subtitle="Coefficient are average increase/decrease in predicted probability of being in need as measured by the corresponding indicator in dependent variable",
                                           out=paste0("output/result/regression_lsg_need_wgq_OLS_pooled_", c, ".html"), align=TRUE, single.row=TRUE, header=FALSE,
                                           dep.var.labels = list_lab, dep.var.labels.include = TRUE) else {
                                             mdoel_table_lsg_c <- stargazer::stargazer(
                                               c_1, c_1_1, c_1_2, c_7, c_2, c_5, c_6, c_4, type="text", title=paste0("Pooled OLS model for ", c),
                                               subtitle="Coefficient are average increase/decrease in predicted probability of being in need as measured by the corresponding indicator in dependent variable",
                                               out=paste0("output/result/regression_lsg_need_wgq_OLS_pooled_", c, ".html"), align=TRUE, single.row=TRUE, header=FALSE,
                                               dep.var.labels = list_lab, dep.var.labels.include = TRUE)
                                           }

  ## save nice formatted data.frame of coefficient and sd for each model all in one data.frame
 models <- list(
   msni_extr_need = c_1,
   msni_need = c_1_1,
   nb_need_adjusted = c_1_2,
   fs_lsg_need = c_2,
   liv_lsg_need = c_3,   # c_3 may be NULL in some cases
   wash_lsg_need = c_4,
   edu_lsg_need = c_5,
   snfi_lsg_need = c_6,
   health_lsg_need = c_7
 )

 models <- discard(models, is.null)

 coef_c <- models %>%
   imap(~ tidy(.x) %>% mutate(dependent = .y)) %>%  # .y is the name (dependent variable)
   bind_rows() %>% mutate(country=c)

 coef_all <- bind_rows(coef_all, coef_c)

}

coef_all_final <- coef_all %>% left_join(lab.model)

## for each unique value of model_lab, plot with geom_point and geom_segment the coeff value, arranging by desc(estimate) with x=country theme minimal and save the graph
for (m in unique(coef_all_final$model_lab)){
  # m <- unique(coef_all_final$model_lab)[1]
  p <- coef_all_final %>% filter(model_lab==m, term=="wgq_dis_3_at_least_one") %>%
    ggplot(aes(x=reorder(country, estimate), y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    geom_errorbar(aes(linetype = "95% CI"), width=0.2) + geom_point(aes(colour = "Estimate")) +
    scale_colour_manual(values = c("Estimate"="indianred")) + scale_linetype_manual(values=c("95% CI"="solid")) +
    coord_flip() + theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
    labs(title=paste0("OLS coefficient for ", m), subtitle = str_wrap(paste0("Increase in predicted ", m, " if HH has at least one individual with WG-SS 3"), width=55),
         y="Coefficient value", x="country", caption = "Error bars represent 95% confidence intervals.", colour = NULL, linetype = NULL)
  ggsave(paste0("output/result/coef_", m, "_high.png"), width=6, height=8, p, bg="white")
}

## within coef_all, identify for which sectors there is the biggest standard deviation among estimated coefficient for each country
sum_coef_all <- coef_all_final %>%
  filter(term=="wgq_dis_3_at_least_one") %>%
  group_by(model_lab) %>%
  summarise(
    mean_coef = mean(estimate) %>% round(4),
    sd_coef = sd(estimate) %>% round(4),
    cv = round(sd_coef/mean_coef, 4)
  ) %>%
  arrange(cv)
## use clipr:: package to write clip of the data.frame and paste as table in a ppt then
# clipr::write_clip(sum_coef_all, object_type = "table")

coef_all_final_dis <- coef_all_final %>% filter(term=="wgq_dis_3_at_least_one")
## mutate as factor to get order of model from highest to lowest aver coeff value
mod_levels <- coef_all_final_dis %>% group_by(model_lab) %>% summarise(mean_coef  = mean(estimate)) %>% arrange(desc(mean_coef)) %>% pull(model_lab)
sum_coef_all <- sum_coef_all %>% mutate(model_lab=factor(model_lab, levels=rev(mod_levels)))
coef_all_final_dis <- coef_all_final_dis %>% mutate(model_lab=factor(model_lab, levels=rev(mod_levels)))

plot_coef <- ggplot() +
  # Boxplot to show distribution
  geom_boxplot(data = coef_all_final_dis, aes(x = model_lab, y = estimate, fill = model_lab), alpha = 0.3) +
  ## add a indianred segment for the median cause it's beautiful using stat summary
  stat_summary(data = coef_all_final_dis,
               fun = median, geom = "segment", color = "indianred", size=1,
               aes(y=estimate,
                   x = as.numeric(factor(reorder(model_lab, estimate))) - 0.37,
                   xend = as.numeric(factor(reorder(model_lab, estimate))) + 0.375,
                   yend = after_stat(y))) +

  # Points for mean values with error bars (showing SD)
  geom_point(data = sum_coef_all, aes(x = model_lab, y = mean_coef), size = 3, color = "black") +

  ## Put me a beautiful scale_fill_manual using the viridis palette
  scale_fill_manual(values = viridis::viridis(length(unique(coef_all_final_dis$model_lab)))) +

  # geom_errorbar(data = sum_coef_all, aes(x = model_lab, ymin = mean_coef - sd_coef, ymax = mean_coef + sd_coef), width = 0.2, color = "black") +

  # Segments to represent CV values
  geom_segment(data = sum_coef_all, aes(x = model_lab, xend = model_lab, y = mean_coef, yend = mean_coef + (cv * mean_coef / 2)),
               color = "indianred", linetype = "dashed") +

  # Labeling the CV values
  geom_text(data = sum_coef_all, aes(x = model_lab, y = mean_coef + (cv * mean_coef / 2), label = paste0("CV: ", round(cv, 2))),
            color = "black", size = 4, vjust = -0.5, hjust=1 ) +

  # Aesthetics
  theme_minimal() +
  labs(title = "OLS Coefficient distribution by sectoral gap",
       subtitle = "Boxplot shows distribution of OLS coefficient associated with presence of member(s) with disability\nPoints with error bars show mean ± SD, and red segments represent coefficient of variation",
       x = "Sectoral gap indicator (LSG)\n",
       y = "\nCoefficient Estimate",
       fill="OLS Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
        ) +

  coord_flip()
## save
ggsave("output/graph_wgq/coef_all.png", plot_coef, bg="white", height = 10, width = 13)


### General graph mixing msni 3+ prevalence difference + overal prevalence level with a geom_bar

## filter out some countries
country_exclude <- ""
# country_exclude <- c("MMR", "MLI", "CAR")
# country_exclude <- c("AFG", "MLI", "BGD", "CAR", "OPT")

res.wgq.all <- res.wgq.all %>%
  mutate(need_label=str_replace_all(question_lab, c("extra need"="≥4", "need"="≥3")),
         question_label=str_replace_all(question_lab, c(" (extra |)need"="")),
         metric=str_replace_all(question_label, " lsg", ""))

# unique(res.wgq.all$question_lab)
# q_lab_val <- "food lsg need"


prevalence.wgq <- import("output/result/result_wgq_all.xlsx", sheet=1)
## alternative way to display data => %HHs with disability //
prevalence.wgq.2 <- data_all %>%
  group_by(country, year) %>%
  mutate(across(matches("wgq_dis_(\\d)_at_least_one"), ~case_when(.=="At least one" ~ 1, .=="None" ~ 0, T ~ as.numeric(.)))) %>%
  summarise(across(matches("wgq_dis_(\\d)_at_least_one"), ~weighted.mean(., w=weight, na.rm=T))) %>%
  pivot_longer(matches("wgq"), names_to = "question", values_to = "mean") %>%
  mutate(question=str_replace_all(question, "_at_least_one", ""), indicator="wgq_dis",
         indicator_label="% of HHs with at least one member with disability",
         choice_label=str_replace_all(question, "wgq_dis_", "WG-SS ")) %>% filter(!is.nan(mean)) %>%
  filter(question=="wgq_dis_3") ## keep only WG-SS 3

plot_loli_prev <- function(q_lab_val="msni need", countries=country_exclude, df.prev=prevalence.wgq.2,
                           title.cust=NULL, subtitle.cust=NULL){

  df.plot <- res.wgq.all %>% mutate(col_x=paste(country, gsub("^20","", year))) %>%
    filter(wgq_level==3)%>%
    group_by(country, question) %>% filter(year==max(year)) %>% ungroup %>% ## keep only the last available year
    filter(question_lab==q_lab_val) %>%
    # filter(str_detect(question, pattern.msni), wgq_level==3) %>% ## keep only msni indicator
    group_by(country, year) %>% mutate(average_mean=mean(mean)) %>% arrange(average_mean) %>% ungroup %>% ## get order of countries right
    mutate(col_x=factor(col_x, levels=unique(col_x), ordered = T))## as factor to have right ordering in graph

  ## marksignificance of prevalence difference [by checking non-overlapping confidence intervals]
  df.plot <- df.plot %>%
    group_by(country, year, question) %>%
    mutate(mean_at_least = mean[wgq_dis_at_least_one=="At least one"],
           mean_none = mean[wgq_dis_at_least_one=="None"],
           significant = case_when(
             mean_at_least > mean_none & `mean/low`[wgq_dis_at_least_one=="At least one"]>`mean/upp`[wgq_dis_at_least_one=="None"] ~ T,
             mean_at_least < mean_none & `mean/low`[wgq_dis_at_least_one=="None"]>`mean/upp`[wgq_dis_at_least_one=="At least one"] ~ T,
             T ~ F),
           mark_sign = case_when(
             wgq_dis_at_least_one=="At least one" & significant ~ "*",
             wgq_dis_at_least_one=="At least one" & significant ~ "*",
             T ~ ""),
           mark_height = (mean[wgq_dis_at_least_one=="At least one"]+mean[wgq_dis_at_least_one=="None"])/2
    ) %>% ungroup

  ## for prevalence bar plot
  ## add stacked plot with disability prevalence

  df.prev <- df.prev %>% filter(indicator=="wgq_dis") %>% mutate(col_x=paste(country, gsub("^20","", year))) %>%
    mutate(lab_graph=paste0(indicator_label, "\n", choice_label), facet=paste0(country, "\n", year), country_col=country) %>%
    group_by(country, year) %>% arrange(desc(question)) %>%
    mutate(mean_diff = mean - lag(mean, default = 0), .after="mean") %>%
    ## do define height of the next bar only take the change in prevalence from one severity to the next one
    ungroup %>%
    filter(col_x %in% df.plot$col_x) %>% ## keep matching set of countries
    group_by(country, question) %>% filter(year==max(year)) %>% ungroup %>% ## keep only the last available year
    mutate(value = str_replace_all(question, "wgq_dis_", "")) %>%
    filter(question %in% c("wgq_dis_4", "wgq_dis_3"))

  if (unique(df.prev$indicator_label)=="WG-SS severity level"){
    ## add row for AFG 22 to put only mean="no data" if df.prev
    df.prev <- df.prev %>% add_row(country="AFG", year=2022, col_x="AFG 22", choice_label=unique(df.prev$choice_label)[2], mean=0, mean_diff=0)
  }

  col_fill <- c("#F3BEBD", "#F27D7C") %>% setNames(rev(unique(df.prev$choice_label)))


  df.prev <- df.prev %>% filter(!country %in% country_exclude)
  df.plot <- df.plot %>% filter(!country %in% country_exclude)

  plot.all <- ggplot() +
    geom_segment(data = df.plot, aes(x=col_x, xend=col_x, y=min_y, yend=mean, color=max_color), size=0.75, show.legend = F) +
    geom_point(data = df.plot, aes(color=wgq_dis_at_least_one, fill=wgq_dis_at_least_one, x=col_x, y=mean), size=1) +
    geom_text(data = df.plot, aes(y=mean,x=col_x,label = ifelse(significant, paste0(round(100*mean, 0), "%"), "")), hjust=-.5, size=3) +
    geom_bar(data = df.prev, aes(y = -mean_diff, x = col_x, fill = choice_label), position = position_stack(reverse = FALSE), stat = "identity") +
    geom_text(data = df.prev %>% arrange(desc(choice_label)),
              aes(y = -mean_diff, x = col_x, label = case_when(mean > 0.02 ~ paste0(round(100 * mean, 1), "%"), mean==0 ~ "No data", T ~ ""), group = year),
              position = position_stack(reverse = FALSE, vjust = 0.55), size = 3)+
    scale_fill_manual(na.value = "#E6DDCA", values = col_fill, na.translate = FALSE,
                      labels = \(x) str_wrap(str_replace_all(x, c("3 -"="WG-SS 3 -", "4 -"="WG-SS 4 -")), width = 24)) +
    scale_colour_manual(values=c("indianred", "#0067A9"),labels=\(x) str_replace_all(x, c("At least one"="At least\none individual", "None"="No\nindividual"))) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(-.16,1)) +
    scale_x_discrete(labels=\(x) str_replace(x, " \\d\\d", "")) +

    labs(color=paste0("HH with\nat least one\nWG-SS ",3,"\nindividual"), fill="Prevalence\nof disability", x="", y="",
         title=paste0("Prevalence of ", unique(df.plot$question_label)," and disability"),
         subtitle=paste0("\n",
                         str_wrap(paste0(
                           "Prevalence of ",
                           unique(df.plot$question_metric),
                           " disaggregated by household with at least one individual with disability (WG-SS 3)"), 70),
                         "\nPrevalence of disability by severity (WG-SS 3/4) at individual level"),
         caption = paste0("How to read the graph:\n\n",
                          str_wrap(paste0("Segment-dot graph: The segment with red and blue dots represent the difference in prevalence of HHs with ",
                                          unique(df.plot$question_metric),
                                          ") with/without at least one individual with disability. Whenever the difference in prevalence is significant at a level of 5%, prevalence values are displayed with % labels in graph (non-overlapping 95% condidence intervals)"), 130), "\n\n",
                          str_wrap("Bar chart: The bar plot below the segments display the prevalence rate of disability at individual level by severity threshold (WG-SS 3 or WG-SS 4). Percentages are showed only for values above 2%.", 130),
                          "\n\nGraph done using for each country the latest available data from 2022 and 2023 MSNAs.")) +

    theme_minimal() +
    theme(plot.title=element_text(hjust=0.5, size=16), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom",
          # legend.box="vertical",
          plot.caption = element_text(hjust = -0, size = 9), legend.text = element_text(size=9)) +
    labs(
      y=paste(paste(rep(" ", 30), collapse=""), "% of individuals",
              paste(rep(" ", 40), collapse=""), "% of households",
              paste(rep(" ", 80), collapse=""))
    )

  if (!is.null(title.cust)) plot.all <- plot.all + labs(title=title.cust)
  if (!is.null(subtitle.cust)) plot.all <- plot.all + labs(subtitle=subtitle.cust)

  return(plot.all)
}

for (var in unique(res.wgq.all$question_lab) %>% keep(str_detect(., "msni"))){
  # var <- unique(res.wgq.all$question_lab)[8]
  # country_exclude<-c("MMR", "MLI", "CAR")
  if (str_detect(var, "msni")){
    graph.title="Prevalence of multi-sectoral gaps (MSNI) and disability"
    graph.subtitle="Prevalence of MSNI≥3 disaggregated by housholds with at least one individual with disability (WG-SS 3)\nPrevalence of disability by severity (WG-SS 3-4) at individual level"
  if (var=="msni extra need") {
    graph.title=gsub("multi-sectoral gaps", "severe multi-sectoral gaps", graph.title)
    graph.subtitle=gsub("MSNI≥3", "MSNI≥4", graph.subtitle)
    }
  } else {
    graph.title=NULL
    graph.subtitle=NULL
  }
  p <- plot_loli_prev(q_lab_val = var, countries = country_exclude, title.cust = graph.title, subtitle.cust = graph.subtitle) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(-.31,1)) +
    labs(y="% of households", fill="% of HHs with\nat least one member\nwith disability")
  actual_caption <- ggplot_build(p)$plot$labels$caption
  p <- p + labs(caption = str_replace_all(actual_caption,
                                          c("prevalence rate of disability at individual level by severity threshold"="% of households with at least one member with disability",
                                            "WG-SS 3 or WG-SS 4"="WG-SS 3",
                                            "disability\n"="disability ",
                                            " Percentages are showed"="\nPercentages are showed")))

  ggsave(paste0("output/graph_wgq/graph_loliplot_and_wgq_prevalence/", paste0(country_exclude, collapse="_"), "_", var,"_hh_prev.png"), p, bg="white", height=9, width=11, units="in", dpi=600)
}


##########################################################################################

plot_loli_only <- function(q_lab_val="nb need adjusted"){
  df.plot <- res.wgq.all %>% mutate(col_x=paste(country, gsub("^20","", year))) %>%
    filter(wgq_level==3)%>%
    group_by(country, question) %>% filter(year==max(year)) %>% ungroup %>% ## keep only the last available year
    filter(question_lab==q_lab_val) %>%
    # filter(str_detect(question, pattern.msni), wgq_level==3) %>% ## keep only msni indicator
    group_by(country, year) %>% mutate(average_mean=mean(mean)) %>% arrange(average_mean) %>% ungroup %>% ## get order of countries right
    mutate(col_x=factor(col_x, levels=unique(col_x), ordered = T))## as factor to have right ordering in graph

  ## marksignificance of prevalence difference [by checking non-overlapping confidence intervals]
  df.plot <- df.plot %>%
    group_by(country, year, question) %>%
    mutate(mean_at_least = mean[wgq_dis_at_least_one=="At least one"],
           mean_none = mean[wgq_dis_at_least_one=="None"],
           significant = case_when(
             mean_at_least > mean_none & `mean/low`[wgq_dis_at_least_one=="At least one"]>`mean/upp`[wgq_dis_at_least_one=="None"] ~ T,
             mean_at_least < mean_none & `mean/low`[wgq_dis_at_least_one=="None"]>`mean/upp`[wgq_dis_at_least_one=="At least one"] ~ T,
             T ~ F),
           mark_sign = case_when(
             wgq_dis_at_least_one=="At least one" & significant ~ "*",
             wgq_dis_at_least_one=="None" & significant ~ "*",
             T ~ ""),
           mark_height = (mean[wgq_dis_at_least_one=="At least one"]+mean[wgq_dis_at_least_one=="None"])/2
    ) %>% ungroup

  plot <- ggplot() +
    geom_segment(data = df.plot, aes(x=col_x, xend=col_x, y=min_y, yend=mean, color=max_color), size=0.75, show.legend = F) +
    geom_point(data = df.plot, aes(color=wgq_dis_at_least_one, fill=wgq_dis_at_least_one, x=col_x, y=mean), size=1) +
    scale_colour_manual(values=c("indianred", "#0067A9"),labels=\(x) str_replace_all(x, c("At least one"="At least\none individual", "None"="No\nindividual"))) +
    scale_fill_manual(values=c("indianred", "#0067A9"),labels=\(x) str_replace_all(x, c("At least one"="At least\none individual", "None"="No\nindividual"))) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
    scale_x_discrete(labels=\(x) str_replace(x, " \\d\\d", "")) +

    labs(color=paste0("HH with\nat least one\nWG-SS ",3,"\nindividual"),
         fill=paste0("HH with\nat least one\nWG-SS ",3,"\nindividual"),
         x="", y="",
         title=paste0("Prevalence of ", unique(df.plot$question_label)," and disability"),
         subtitle=paste0("\n",
                         str_wrap(paste0(
                           "Prevalence of ",
                           unique(df.plot$question_metric),
                           " disaggregated by household with at least one individual with disability (WG-SS 3)"), 70),
                         "\nPrevalence of disability by severity (WG-SS 3/4) at individual level"),
         caption = paste0("How to read the graph:\n\n",
                          str_wrap(paste0("The segment with red and blue dots represent the difference in prevalence of HHs with ",
                                          unique(df.plot$question_metric),
                                          ") with/without at least one individual with disability. Whenever the difference in prevalence is significant at a level of 5%, prevalence values are displayed with % labels in graph (non-overlapping 95% confidence intervals)"), 180),
                          "\n\nGraph done using for each country the latest available data from 2022 and 2023 MSNAs.")) +

    theme_minimal() +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom", legend.box="vertical",
          plot.caption = element_text(hjust = -0, size = 8), legend.text = element_text(size=8))

  if (q_lab_val=="nb need adjusted"){
    plot <- plot +
      scale_y_continuous(limits=c(0, 3.5)) +
      geom_text(data = df.plot, aes(y=mean,x=col_x,label = ifelse(significant, paste0(round(mean, 1), ""), "")), vjust=-1, size=3)

  } else {
    plot <- plot +
      geom_text(data = df.plot, aes(y=mean,x=col_x,label = ifelse(significant, paste0(round(100*mean, 0), "%"), "")), hjust=-.5, size=3)

  }


  return(plot)
}

plot.nb.sec <- plot_loli_only() +
  labs(title="Average number of sectoral gaps",
       subtitle="disaggregated by presence of member(s) with disability in household",
       y="Average number of sectoral gaps",

       caption=paste0(str_wrap("The segment with red and blue dots represent the difference in average number of sectoral gaps among households with/witout individual(s) with disability. Whenever the difference is significant at a level of 5%, values are displayed in the graph. (non-overlapping 95% confidence intervals", 150),
                      "\n\nGraph done using for each country the latest available data from 2022 and 2023 MSNAs.\nNumber of sectoral gaps has been rescaled to account for variing number of sectors assessed in some context/years.")) +
  coord_flip()

ggsave("output/graph_wgq/nb_sectoral_gap_loliplot.png",
       plot.nb.sec, bg="white", height=8, width=8, units="in", dpi=600)

map(unique(res.wgq.all$question_lab),
    \(x) {
      p <- plot_loli_only(q_lab_val = x) + scale_y_continuous(labels=scales::percent_format(), limits=c(0,NA))
      ggsave(paste0("output/graph_wgq/prevalence difference loliplot/disability_loliplot_", x,"_", paste0(country_exclude, collapse="_"), ".png"),
             p, bg="white", height=8, width=10, units="in", dpi=600)

    })


## additionnal education individual level analysis plotted
res_edu_ind <- import("data/education_ind.xlsx") %>%
  mutate(disability_wgss_3=case_when(
    group_var=="wgq_dis_3" & group_var_value==1 ~ "yes",
    group_var=="wgq_dis_3" & group_var_value==0 ~ "no",
    group_var=="wgq_dis_3" & group_var_value==0 ~ "undefined",
    T ~ NA_character_
  ))

plot.edu <- function(df=res_edu_ind,
                     var="edu_ind_access_d",

                     fill.var="disability_wgss_3",
                     lab="% individual with access to formal schooling",
                     sublab="Disaggregated by disability status"
                     ){
  df %>%
    # filter(!is.na(!!sym(fill.var))) %>%
    filter(analysis_var == var, group_var=="wgq_dis_3") %>%
    ggplot(aes(x=country, y=stat, fill=!!sym(fill.var))) +
    geom_bar(stat="identity", position = "dodge") +
    # geom_errorbar(aes(ymin=stat_low, ymax=stat_upp), width=.2, position=position_dodge(.9)) +
    scale_fill_manual(values=c("#0067A9", "indianred"), labels=~str_replace_all(., "_", " ")) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
    theme_minimal() +
    labs(y="% of individuals", x="", fill="Disability status\n(WG-SS 3)", title=lab, subtitle=sublab) +
    geom_text(aes(label = paste0(round(stat*100, 0), "%\nn=", n_total)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position ="bottom")

}

ggsave(paste0("output/graph_wgq/additionnal_indicator/individual_", var, "dis_wgss3.png"), plot.edu(), bg="white", width=7, height=6)

# plot.edu()


