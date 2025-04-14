## Quick case study MLI comparing short set vs integrating wgq_stress/chronic disease into severity WG-SS calculation
## extended version will include any ind with chronic disease as WG-SS level 4 and any ind with sign of stress & anxiety every day as level 4
## extended version will include any ind with sign of stress & anxiety every day or once a week as level 3

## calculate alternate WG-SS for MLI-2022
vars_extended <- vars %>% append(c("wgq_stress", "wgq_chronic_disease"))
df_wgq_22_mli <- df_wgq_22 %>% filter(country=="MLI")  %>%
  add_loop_wgq_ss(undefined = c("dnk", "pnta", "undefined")) %>%
  mutate(
    wgq_dis_4_alternative_1 = case_when(
      rowSums(across(all_of(vars_extended), ~ . %in% c("cannot_do")), na.rm=T) > 0 ~ 1,
      rowSums(across(all_of(vars_extended), ~ !is.na(.))) > 0 ~ 0, T ~ NA_real_
    ),
    wgq_dis_3_alternative_1 = case_when(
      rowSums(across(all_of(vars_extended), ~. %in% c("cannot_do", "lot_of_difficulty", 1, "all_days")), na.rm=T) > 0 ~ 1,
      rowSums(across(all_of(vars_extended), ~ !is.na(.))) > 0 ~ 0, T ~ NA_real_
    ),
    wgq_dis_2_alternative_1 = case_when(wgq_dis_3_alternative_1 == 1 | wgq_stress %in% "on_week"~ 1, T ~ wgq_dis_2),
    wgq_dis_1_alternative_1 = case_when(wgq_dis_3_alternative_1 == 1 ~ 1, T ~ wgq_dis_1),

    wgq_dis_4_alternative_2 = case_when(
      rowSums(across(all_of(vars_extended), ~ . %in% c("cannot_do", 1, "all_days")), na.rm=T) > 0 ~ 1,
      rowSums(across(all_of(vars_extended), ~ !is.na(.))) > 0 ~ 0, T ~ NA_real_
    ),
    wgq_dis_3_alternative_2 = case_when(
      rowSums(across(all_of(vars_extended), ~. %in% c("cannot_do", "lot_of_difficulty", 1, "all_days", "on_week")), na.rm=T) > 0 ~ 1,
      rowSums(across(all_of(vars_extended), ~ !is.na(.))) > 0 ~ 0, T ~ NA_real_
    ),
    wgq_dis_2_alternative_2 = case_when(wgq_dis_3_alternative_2 == 1 ~ 1, T ~ wgq_dis_2),
    wgq_dis_1_alternative_2 = case_when(wgq_dis_3_alternative_2 == 1 ~ 1, T ~ wgq_dis_1)
  )

## very quick calculation of wgq_dis_3 vs wgq_dis_3_ext and wgq_dis_4 vs wgq_dis_4_ext
lab_alt <- data.frame(

  framework = c("WG-SS extended", "WG-SS extended bis", "WG-SS") %>% lapply(\(x) rep(x, 4)) %>% unlist,

  ind = c("wgq_dis_4_alternative_1", "wgq_dis_3_alternative_1", "wgq_dis_2_alternative_1", "wgq_dis_1_alternative_1",
          "wgq_dis_4_alternative_2", "wgq_dis_3_alternative_2", "wgq_dis_2_alternative_2", "wgq_dis_1_alternative_2",
          "wgq_dis_4", "wgq_dis_3", "wgq_dis_2", "wgq_dis_1"),

  complete_label = c("WG-extended 4 - At least one domain cannot do at all",
                     "WG-extended 3 - At least one domain cannot do at all/lot of difficulty or chronic disease or showing signs worriness/nervousness/anxiety everyday",
                     "WG-extended 2 - At least two domains some difficulty/any domain lot of difficulty/cannot do at all or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",
                     "WG-extended 1 - At least one domain some difficulty/lot of difficulty/cannot do at all or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",

                     "WG-extended_bis 4 - At least one domain cannot do at all or chronic disease or showing signs worriness/nervousness/anxiety everyday",
                     "WG-extended_bis 3 - At least one domain cannot do at all/lot of difficulty or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",
                     "WG-extended_bis 2 - At least two domains some difficulty/any domain lot of difficulty/cannot do at all or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",
                     "WG-extended_bis 1 - At least one domain some difficulty/lot of difficulty/cannot do at all or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",

                     "WG-SS 4 - At least one domain cannot do at all",
                     "WG-SS 3 - At least one domain cannot do at all/lot of difficulty",
                     "WG-SS 2 - At least two domains some difficulty/any domain lot of difficulty/cannot do at all",
                     "WG-SS 1 - At least one domain some difficulty/lot of difficulty/cannot do at all"
  ),

  label = c("WG-SS 4",
            "WG-SS 3 or chronic disease or showing signs worriness/nervousness/anxiety everyday",
            "WG-SS 2 or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",
            "WG-SS 1 or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",

            "WG-SS 4 or chronic disease or showing signs worriness/nervousness/anxiety everyday",
            "WG-SS 3 or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",
            "WG-SS 2 or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",
            "WG-SS 1 or chronic disease or showing signs worriness/nervousness/anxiety everyday/once a week",

            "At least one domain cannot do at all",
            "At least one domain cannot do at all/lot of difficulty",
            "At least two domains some difficulty/any domain lot of difficulty/cannot do at all",
            "At least one domain some difficulty/lot of difficulty/cannot do at all"),

  severity = rep(c("severity 4", "severity 3", "severity 2", "severity 1"), 3)
)

lab_alt <- lab_alt %>% arrange(framework) %>%
  mutate(caption=paste0(framework, " ", severity, ": ", label))

res_mli <- df_wgq_22_mli %>%
  summarise(across(all_of(matches("^wgq_dis_(3|4|2|1)")), list(mean=~weighted.mean(., w=weight, na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) %>%
  pivot_longer(where(is.numeric)) %>% separate(name, c("ind", "fn"), sep = "_(?=[^_]*$)", remove = T) %>%
  pivot_wider(names_from = fn, values_from = value) %>% left_join(lab_alt)

## quickly plot this
plot.mli.22 <- res_mli %>% filter(framework %in% c("WG-SS extended", "WG-SS")) %>%
  ggplot(aes(x=severity, y=mean, fill=framework)) + geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("#EE5859", "#0067A9")) + scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.05)) +
  geom_text(aes(label = scales::percent(round(mean, 4))), position = position_dodge(width = 1), vjust = -1.5, size=3) +
  labs(title="MLI 2022 - Disability prevalence\nComparing WG-SS with extended version",
       subtitle="Adding chronic disease + anxiety/worriness/nervousness items",
       fill="Framework",x="", y="% individuals") + theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), legend.position = "bottom", plot.subtitle = element_text(hjust = 0.5))

plot.mli.22 <- plot.mli.22 + plot_annotation(
  caption =  lab_alt %>% filter(framework %in% c("WG-SS extended", "WG-SS")) %>% pull(caption) %>% str_wrap(., 130) %>% paste(., collapse="\n") %>% paste0("Severity framework:\n\n", .),
  theme = theme(plot.caption = element_text(hjust = 0))
)
## save this
ggsave("output/graph_wgq/plot.mli.22.png", plot.mli.22, width=7.3, height=8, units="in", dpi=300)





