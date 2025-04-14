#' @title Plot best type of phone owned in the HH
#' @description This function plots the best type of phone owned in the HH by population group and country.
#' @param data The input data frame, long result table format
#' @param col_question The name of the column that contains the short name of the question.
#' @param var The name of the variable corresponding to the best type of phone as it appears in the column col_question  
#' @param Vector of variable of maximum length 2 to disaggregate against. The first element will be the fill and the second facet for the ggplot graph 
#' @return The ggplot object 
plot.phone <- function(data = df_long_pop, 
                       col_question = "question", 
                       var = "etc_access_to_phone_select_one", 
                       x = c("pop_group_cat", "country")){
  
  df <- data %>% filter(!!sym(col_question)==var)
  c.order <- df %>% filter(choice=="smartphone") %>% arrange(desc(mean)) %>% pull(country) %>% unique
  
  df <- df %>%
    mutate(question_label=str_wrap(question_label, width = 60)) %>%
    mutate(country = factor(country, levels=c.order)) %>%
    mutate(choice_label=factor(choice_label, levels=c("None", 
                                                      "Basic phone (Calls, SMS, mobile money, no Internet access)",
                                                      "Feature phone (basic internet access, some preinstalled apps, no app store, physical keyboard)",
                                                      "Smartphone (touchscreen, app store, advanced internet access)")))
  
  plot <- df %>%
    ggplot(aes(y=mean, x=!!sym(x[1]), fill=choice_label)) +
    geom_bar(position = position_stack(reverse = F), stat="identity") +
    geom_text(aes(label=ifelse(mean>0.01,paste0(round(100*mean, 0), "%"),"")), position=position_fill(reverse = F, vjust = .5), size=2.75) +
    scale_fill_manual(na.value = "#E6DDCA", values=c("#EE5859","#F3BEBD","#58585A","#C7C8CA","white"), labels = function(x) str_wrap(x, width = 30))+
    theme_minimal() + scale_y_continuous(labels = scales::percent_format()) + 
    scale_x_discrete(labels = ~str_replace_all(.,"_", " ")) +
    theme(legend.position = "bottom") + labs(fill="", x="", y="", title=unique(df$question_label))
  
  if (length(x)==2) plot <- plot + facet_grid(cols=vars(!!sym(x[2])), scales="free_x")
  
  return(plot)
}

plot.coverage <- function(data = df_long_pop, 
                          col_question = "question", 
                          var = "etc_coverage_internet", 
                          x = c("pop_group_cat", "country")){
  
  df <- data %>% filter(!!sym(col_question)==var)
  c.order <- df %>% filter(choice=="internet") %>% arrange(desc(mean)) %>% pull(country) %>% unique
  c.levels <- c("No coverage at all",
                "Only SMS",
                "Voice and no internet (Voice & SMS or only Voice)",
                "Internet (Voice, SMS and Internet or only Internet)")
  
  df <- df %>%
    mutate(question_label=str_wrap(question_label, width = 100)) %>%
    mutate(country = factor(country, levels=c.order)) %>%
    mutate(choice_label=factor(choice_label, levels= c(c.levels, unique(choice_label) %>% keep(!.%in%c.levels))))
  
  plot <- df %>%
    ggplot(aes(y=mean, x=!!sym(x[1]), fill=choice_label)) +
    geom_bar(position = position_stack(reverse = F), stat="identity") +
    geom_text(aes(label=ifelse(mean>0.01,paste0(round(100*mean, 0), "%"),"")), position=position_fill(reverse = F, vjust = .5), size=2.5) +
    scale_fill_manual(na.value = "#E6DDCA", values=c("#EE5859","#F27D7C", "#F3BEBD","#58585A","#C7C8CA","#E6DDCA","white"))+
    theme_minimal() + scale_y_continuous(labels = scales::percent_format()) + scale_x_discrete(labels = ~str_replace_all(.,"_", " ")) +
    theme(legend.position = "bottom") + labs(fill="", x="", y="", title=unique(df$question_label))
  
  if (length(x)==2) plot <- plot + facet_grid(cols=vars(!!sym(x[2])), scales = "free_x") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  return(plot)
}