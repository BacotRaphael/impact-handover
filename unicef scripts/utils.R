is_not_empty <- function(x) {!is.null(x) & length(x) > 0 & !all(is.na(x))}


add_analysis_key_var <- function(df_long, q.type="prop_select"){
  df_long %>%
    dplyr::mutate(analysis_key_var = paste0(q.type, " @/@ ", question, " %/% ", choice))
}

add_group_var_key <- function(df_long, group_var){
  ## if group_var is in colnames(df_long), create the analysis key wit the following format
  ## group_var column with all group vars separated by ~ / ~
  ## group_var_value column with all group var values separated by ~ / ~
  if (any(group_var %in% colnames(df_long))) {
    df_long %>%
      dplyr::mutate(group_var_key = paste(group_var, collapse=" %/% ")) %>%
      tidyr::unite(col = "group_var_value", !!!syms(group_var), sep = " %/% ", remove = F) %>%
      dplyr::relocate(group_var_value, .after="group_var_key")
  } else {
    print(paste0("Group var ", paste(group_var[!group_var %in% colnames(df_long)], collapse="; ") , " not in dataset."))
  }
}

add_analysis_key <- function(df_long, delete.old.col=T){
  if (!"analysis_key_var" %in% colnames(df_long)) stop("analysis_key_var not in dataset")
  if (all(c("group_var_key", "group_var_value") %in% colnames(df_long))){
    df_long <- df_long %>%
      mutate(
        group_var_key_list = strsplit(group_var_key, " ~/~ "),
        group_var_value_list = strsplit(group_var_value, " ~/~ "),
        max_length = pmax(lengths(group_var_key_list), lengths(group_var_value_list)),
        group_var_key_list = mapply(function(keys, len) c(keys, rep("", len - length(keys))), group_var_key_list, max_length, SIMPLIFY = FALSE),
        group_var_value_list = mapply(function(values, len) c(values, rep("", len - length(values))), group_var_value_list, max_length, SIMPLIFY = FALSE),
        group_var_format = mapply(function(keys, values) paste0(mapply(function(k, v) paste0(k, " %/% ", v), keys, values), collapse = " -/- "),
                                  group_var_key_list, group_var_value_list),
        analysis_key = paste0(analysis_key_var, " @/@ ", group_var_format)
      )

  } else {
    warning("No group_var_key column in dataset. Create analysis key without grouping_var.")
    df_long <- df_long %>% mutate(analysis_key=analysis_key_var)
  }

  if (delete.old.col) {
    df_long <- df_long %>% select(-any_of(c("group_var_format", "group_var_value_list", "group_var_key_list", "max_length", "group_var_key", "group_var_value", "analysis_key_var")))
  }

}

add_key <- function(df_long, group_var=NULL, q.type="prop_select", delete.old.col=T){
  df_long <- add_analysis_key_var(df_long, q.type)
  if (is_not_empty(group_var) & !"" %in% group_var) df_long <- add_group_var_key(df_long, group_var)
  df_long <- add_analysis_key(df_long, delete.old.col)
  return(df_long)
}

analyse <- function(df, group_var=NULL, var, col_weight, col_strata=NULL){
  if (any(!var %in% colnames(df))) {
    print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
    var <- var[var %in% colnames(df)]
  }
  df %>% dplyr::group_by(!!!syms(group_var)) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(var),
                                   list(mean=~weighted.mean(., w=!!rlang::sym(col_weight), na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) %>%
    tidyr::pivot_longer(where(is.numeric) & !all_of(group_var)) %>% tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) %>%
    tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) %>%
    dplyr::mutate(fn=ifelse(is.na(choice), stringr::str_replace_all(question, ".*_", ""), fn),
                             question=ifelse(is.na(choice), stringr::str_replace_all(question, "_[^_]*$", ""), question)) |>

    tidyr::pivot_wider(names_from = fn, values_from = value) %>%
    add_key(group_var)
}


analyse_ci <- function(df, group_var=NULL, var, col_weight, col_strata=NULL){
    if (any(!var %in% colnames(df))) {
      print(paste0("Colnames ", paste0(var[!var %in% colnames(df)], collapse="; ") , " not in dataset. Will be excluded from analysis"))
      var <- var[var %in% colnames(df)]
    }
    # if (!is.null(col_strata)) strata_arg <- col_strata else strata_arg <- NULL

    df <- df |>
      srvyr::as_survey_design(weights=!!rlang::sym(col_weight)
                              # , strata=!!rlang::sym(col_strata)
                              ) |>
      srvyr::group_by(!!!rlang::syms(group_var))

    df %>%
      srvyr::summarise(srvyr::across(dplyr::all_of(var), list(mean=~srvyr::survey_mean(., vartype="ci", na.rm=T), count=~sum(., na.rm=T), n=~sum(!is.na(.))))) %>%
      dplyr::rename_with(.fn = ~stringr::str_replace_all(., c("_(?=(low|upp))"="\\/"))) %>%
      tidyr::pivot_longer(where(is.numeric) & !all_of(group_var)) %>% tidyr::separate(name, c("question", "choice.key"), sep = "\\.", remove = T) %>%
      tidyr::separate(choice.key, c("choice", "fn"), sep = "_(?=[^_]*$)", remove = T) %>%
      dplyr::mutate(fn=ifelse(is.na(choice), stringr::str_replace_all(question, ".*_", ""), fn),
                               question=ifelse(is.na(choice), stringr::str_replace_all(question, "_[^_]*$", ""), question)) %>%
      tidyr::pivot_wider(names_from = fn, values_from = value) %>%
      add_key(group_var)
}


analyse_ci_dt <- function(df, group_var = NULL, var, col_weight, col_strata = NULL) {

  survey_design <- df %>%
    srvyr::as_survey_design(weights = !!sym(col_weight), strata = if (!is.null(col_strata)) !!sym(col_strata) else NULL)

  results <- survey_design %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(all_of(var),
             list(mean = ~survey_mean(vartype = "ci", na.rm = TRUE),
                  count = ~sum(., na.rm = TRUE),
                  n = ~sum(!is.na(.)))
      )
    ) %>%
    as.data.table()

  # Transformation des noms de colonnes générés par srvyr
  col_names <- colnames(results)
  var_patterns <- paste0("^(", paste(var, collapse = "|"), ")_")
  results <- melt(
    results,
    measure.vars = patterns(var_patterns),
    variable.name = "question_choice_fn",
    value.name = c("value")
  )

  # Séparer "question_choice_fn" en question, choix et fonction
  results[, c("question", "choice", "fn") := tstrsplit(question_choice_fn, "_", fixed = TRUE)]

  # Restructurer les colonnes pour avoir une vue pivotée (ci_low, ci_upp, etc.)
  results <- dcast(
    results,
    ... ~ fn,
    value.var = "value"
  )

  # Génération de la clé d'analyse
  if (!is.null(group_var)) {
    results[, group_key := paste(group_var, collapse = " ~/~ ")]
    results[, group_value := do.call(paste, c(.SD, sep = " ~/~ ")), .SDcols = group_var]
    results[, analysis_key := paste0(question, " @/@ ", group_key, " %/% ", group_value)]
  } else {
    results[, analysis_key := question]
  }

  return(results)
}

test_pal <- c("#4682B4", "#B0C4DE", "#5F9EA0", "#6495ED", "#00CED1", "#1E90FF")
# test_pal <- c("#EAD1DC", "#B0C4DE", "#b5ceb2", "#D6EADF", "#FED7B0", "#D9D2E9")
# test_pal <- c("#C79FB3", "#8EA4B8", "#94a99c", "#B0C2AF", "#D4B48E", "#B8B0C9")
# test_pal <- c("#B28D9F", "#7C8DA2", "#83967D", "#9AAE8F", "#BA9A6C", "#A797B9")
# test_pal <- c("#9D7B91", "#6B768C", "#727F6E", "#B0C2AF", "#A0875A", "#957EAA")

expand.select.one <- function(df, var, val.parent.na=NA, sep="."){
  unique <- df %>% pull(!!sym(var)) %>% unique %>% na.omit %>% as.character
  lapply(unique,
         function(val) {
           bin.col <- paste0(var, sep, val)
           df <<- df %>%
             mutate(!!sym(bin.col) := case_when(!!sym(var) %in% val.parent.na ~ NA_real_,
                                                !!sym(var) == val ~ 1,
                                                TRUE ~ 0), .after=!!sym(var))
         })
  return(df)
}

expand.select.one.vec <- function(df, x=c(), ...){
  lapply(x, function(var) {df <<- df %>% expand.select.one(var,...)})
  return(df)
}

paste.remove.na <- function(...){return(trimws(gsub("NA", "", paste(...))))}

split.order.c <- function(x){paste(unique(sort(str_split(x, " ")[[1]])), collapse=" ")}

reorder.select.multiple <- function(x){lapply(x, split.order.c) %>% unlist}

collapse.select.multiple <- function(df, cols, name=unique(gsub("(\\.|__|/).*","",cols))){
  if (sum(!cols %in% colnames(df))>0) print("some binary columns are not present in the dataset")
  if (length(name)>1) stop("Cannot find unique name for the parent column from the binary columns.")
  if (sum(unlist(list(df[,cols]>1)), na.rm = T)>1) stop("Not binary variable.")

  cols <- sort(cols)
  var.parent <- name
  df <- df %>% mutate(!!sym(var.parent) := NA_character_)
  lapply(cols, function(c){
    df <<- df %>%
      mutate(!!sym(var.parent) := case_when(
        !!sym(c) > 0 & !!sym(c) <= 1 ~ paste.remove.na(!!sym(var.parent), gsub(paste0("^", var.parent, "(\\.|__)"), "", c)),
        TRUE ~ !!sym(var.parent)
      )
      )
  })
  df <- df %>% mutate(!!sym(var.parent) := reorder.select.multiple(!!sym(var.parent)))
  return(df)
}


expand.select.multiple <- function(df, var, val.parent.na=NA){
  unique <- df %>% pull(!!sym(var)) %>% str_split(" ") %>% unlist %>% unique %>% na.omit %>% as.character
  unique <- unique[!unique %in% ""]
  lapply(unique,
         function(val) {
           bin.col <- paste0(var, ".", val)
           df <<- df %>%
             dplyr::mutate(!!sym(bin.col) := case_when(!!sym(var) %in% val.parent.na ~ NA_real_,
                                                       str_detect(!!sym(var), paste0("(^| )",
                                                                                     str_replace_all(val, c("\\("="\\\\\\(", "\\)"="\\\\\\)", "\\'"="\\\\\\'", "\\/"="\\\\\\/")),
                                                                                     "($| )")) ~ 1,
                                                       TRUE ~ 0), .after=!!sym(var))
         })
  return(df)
}


expand.select.multiple.vec <- function(df, x=c(),...){
  lapply(x, function(var) {df <<- df %>% expand.select.multiple(var,...)})
  return(df)
}

wrap_subtitle_with_placeholder <- function(subtitle="", width=70) {
  parts <- strsplit(subtitle, "\n", fixed = TRUE)[[1]]
  wrapped_parts <- lapply(parts, str_wrap, width = width)
  final_subtitle <- paste(wrapped_parts, collapse = "\n")
  return(final_subtitle)
}

plot.c <- function(data = result_lab_country_year, col_question="indicator", col_label="indicator_label", facet="country", order=F, flip=T, custom_margin=NULL,
                   var="wgq_cannot_do_or_lot_of_difficulty", col_choice_lab="choice_label", col.disag=NULL, longitudinal=T, add.plot=NULL, n.col=5,
                   factor=F
                   ){

  if (longitudinal) data$country <- paste0(data$country, " - ", data$year)

  df <- data %>% filter(!!sym(col_question)==var) %>% mutate(across(matches("lab"), ~str_wrap(., width = 50)))
  c.order <- df[["country"]] %>% unique %>% sort
  if (order) c.order <- df %>% group_by(country) %>% summarise(mean=mean(mean, na.rm=T)) %>% arrange(-mean) %>% pull(country)
  df <- df %>% mutate(country=factor(country, levels=c.order))

  ## if no existing mean/upp & mean/low columns, create using standard rule for CI 95 [p +/- 1.96*sqrt(p(1-p)/n)]
  if (all(!c("mean/upp", "mean/low") %in% colnames(df))){
    z <- qnorm(.975)
    margin <- sqrt((df[["mean"]]*(1-df[["mean"]]))/df[["n"]])*z
    margin <- ifelse(is.nan(margin), 0, margin)
    df$`mean/upp` <- df[["mean"]] + margin
    df$`mean/low` <- df[["mean"]] - margin
  }

  margin <- df[["mean/upp"]] - df[["mean"]]+(max(df[["mean"]])/8)
  if (!is.null(custom_margin)) margin <- custom_margin

  if (factor) {

    plot <- df %>%
      mutate(!!sym(col_choice_lab) := factor(!!sym(col_choice_lab), levels = levels(data[[col_choice_lab]]))) %>%
      ggplot(aes(x=!!sym(col_choice_lab), y=mean))

    } else {

      plot <- df %>% ggplot(aes(x=reorder(!!sym(col_choice_lab), mean), y=mean))

      }


  plot <- plot + geom_bar(position = "dodge", stat = "identity", fill="#0067A9") +
    theme_minimal() + scale_fill_manual(values=test_pal) +
    labs(title=unique(df[[sym(col_label)]]), subtitle = paste0("", unique(df$label)) %>% wrap_subtitle_with_placeholder(width = 75),
         x=paste0(""), y="% HHs") + theme(legend.position = "bottom", plot.title = element_text(hjust=0.5))
  if (!is.null(facet)) plot <- plot + facet_wrap(as.formula(paste("~", facet)), ncol = n.col)

  ## change y scale to label numeric if not percentages
  if (max(df[["mean"]])>1) {plot <- plot +
    geom_text(aes(y = mean + margin, label = ifelse(mean != 0, paste0(round(mean, 1)), "")), position = position_dodge(width = 0.9), size = 2.75) +
    scale_y_continuous(labels=scales::number_format(), breaks=seq(0, max(df[["mean"]]), by = max(df[["mean"]])/5))} else {
      plot <- plot +
        geom_text(aes(y = mean + margin, label = ifelse(mean != 0, paste0(round(100 * mean, 1), "%"), "")), position = position_dodge(width = 0.9), size = 2.75) +
        scale_y_continuous(labels=scales::percent_format(), breaks=seq(0, round(max(df[["mean/upp"]]), 2), by = round(max(df[["mean/upp"]]), 2)/4))}
  if (all(is.na(df[[col_choice_lab]]))) plot <- plot + theme(axis.text.x = element_blank()) else if (flip) plot <- plot + coord_flip()

  ## add add.plot argument as quoted ggplot code that will be evalutaed and added to p
  if (!is.null(add.plot)) plot <- plot + eval(parse(text=add.plot))

  return(plot)
}

## by pop group
plot.c.disag <- function(data = result_lab_country_year_gender, col_question="indicator", col_label="indicator_label", sub_lab=NULL, flip=T, custom_margin=NULL,
                         col.disag = "ind_gender", var = "wgq_cannot_do_or_lot_of_difficulty", col_choice_lab="choice_label", n.col=5,
                         factor=F,
                         longitudinal=T, add.plot=NULL){

  if (longitudinal) data$country <- paste0(data$country, " - ", data$year)

  df <- data %>% filter(!is.na(mean))

  df <- df %>% filter(!!sym(col_question)==var) %>% mutate(across(matches("lab"), ~str_wrap(., width = 50)))
  c.order <- df[["country"]] %>% unique %>% sort
  # c.order <- df %>% group_by(country) %>% summarise(mean=mean(mean, na.rm=T)) %>% arrange(-mean) %>% pull(country)
  df <- df %>% mutate(country=factor(country, levels=c.order))

  ## only if unique values of disag var are \\d after removing "-" ## order disag var as factor [ensuring right ordering of age groups]
  if (all(unique(unlist(df[, col.disag])) %>% str_replace_all("-","") %>% str_detect("\\d"))){
    df$order <- df[[col.disag]] %>% str_replace_all(., "-.*", "") %>% as.numeric
    levels <- df %>% arrange(order) %>% pull(!!sym(col.disag)) %>% unique
    df[[col.disag]] <- factor(df[[col.disag]], levels=levels)
  }

  ## length(col.disag)>1, use first as fill (col.disag) and add the rest to the facet
  facet_var <- "country"
  if (length(col.disag)>1){
    facet_var <- facet_var %>% append(col.disag[-1])
    col.disag <- col.disag[1]
  }

  ## if no existing mean/upp & mean/low columns, create using standard rule for CI 95 [p +/- 1.96*sqrt(p(1-p)/n)]
  if (all(!c("mean/upp", "mean/low") %in% colnames(df))){
    z <- qnorm(.975)
    margin <- sqrt((df[["mean"]]*(1-df[["mean"]]))/df[["n"]])*z
    margin <- ifelse(is.nan(margin), 0, margin)
    df$`mean/upp` <- df[["mean"]] + margin
    df$`mean/low` <- df[["mean"]] - margin
  }

  marg.text <- df[["mean/upp"]] - df[["mean"]]+(max(df[["mean"]])/8)
  if (!is.null(custom_margin)) marg.text <- custom_margin

  if (factor) {

    plot <- df %>%
      mutate(!!sym(col_choice_lab) := factor(!!sym(col_choice_lab), levels = levels(data[[col_choice_lab]]))) %>%
      ggplot(aes(x=!!sym(col_choice_lab), y=mean, fill=!!sym(col.disag)))

  } else {

    plot <- df %>% ggplot(aes(x=reorder(!!sym(col_choice_lab), mean), y=mean, fill=!!sym(col.disag)))

  }


  plot <- plot + geom_bar(position = "dodge", stat = "identity")

  if (all(c("mean/low", "mean/upp") %in% colnames(df))) {
    plot <- plot + geom_errorbar(aes(ymin = `mean/low`, ymax = `mean/upp`), color= "#58585A", width=.3, linewidth=.2, position = position_dodge(width = 0.8))
  }
  plot <- plot +
    theme_minimal() +
    scale_fill_manual(values=c("#EE5859", "#0067A9", "#D2CBB8", "#84A181", "#A9C4EB", "#C7C8CA"),
                      labels = ~ str_replace_all(., "_", " ")) +
    labs(title=unique(df[[col_label]]), subtitle = sub_lab, fill=gsub("_", " ", col.disag), x="", y="% HHs") +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +
    facet_wrap(as.formula(paste0("~ ", paste0(facet_var, collapse=" + "))), ncol = n.col)
  # facet_wrap(~country, ncol = n.col)


  ## change y scale to label numeric if not percentages
  if (max(df[["mean"]], na.rm=T)>1) {plot <- plot +
    geom_text(aes(y = mean + marg.text, label = ifelse(mean != 0, paste0(round(mean, 1)), "")), position = position_dodge(width = 0.9), size = 2.75) +
    scale_y_continuous(labels=scales::number_format(), breaks=seq(0, max(df[["mean"]]), by = max(df[["mean"]])/5))} else {
      plot <- plot +
        geom_text(aes(y = mean + marg.text, label = ifelse(mean != 0, paste0(round(100 * mean, 1), "%"), "")),
                  position = position_dodge(width = 0.9), size = 2.75) +
        scale_y_continuous(labels=scales::percent_format(),
                           breaks=seq(0, round(max(df[["mean/upp"]], na.rm = T), 2), by = round(max(df[["mean/upp"]], na.rm = T), 2)/4))}

  if (all(is.na(df[[col_choice_lab]]))) plot <- plot + theme(axis.text.x = element_blank()) else if (flip) plot <- plot + coord_flip()

  if (!is.null(add.plot)) plot <- plot + eval(parse(text=add.plot))

  return(plot)
}

only_diff <- function(df){df %>% filter(!(indicator %in% vars & choice %in% "no_difficulty"))} ## to lighten graph by domain, don't show no difficulty

## Do a stacked plot with wgq_1/2/3/4 for all year
graph_prevalence <- function(df, facet=T){
  col_fill <- c("#C7C8CA", "#58585A", "#F3BEBD", "#F27D7C", "#EE5859") %>% setNames(rev(unique(prevalence$choice_label)))
  df <- df %>% mutate(value = str_replace_all(question, "wgq_dis_", ""))
  if (facet) df$country <- factor(df$country, levels = c.order) else df$country <- factor(df$facet, levels = rev(c.y.order))

  plot <- ggplot(df, aes(y = mean_diff, x = country, fill = choice_label)) +
    geom_bar(position = position_stack(reverse = FALSE), stat = "identity") +
    geom_text(data = df %>% arrange(desc(choice_label)), aes(label = ifelse(mean > 0.01, paste0(round(100 * mean, 0), "%"), "*"), group = year, y = mean_diff),
              position = position_stack(reverse = FALSE, vjust = 0.5), size = if (facet) 2 else 2.35) +
    scale_fill_manual(na.value = "#E6DDCA", values = col_fill, na.translate = FALSE, labels = function(x) stringr::str_wrap(x, width = 25)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5)) +
    labs(title = "MSNA cross crisis analysis\nDisability prevalence by WG-SS severity level",
         x = if (facet) "country" else "country / year", y = "% of individuals", fill = "WG-SS\nseverity level", subtitle = "By country and year",
         caption = "Prevalence shown in graph correspond to the % individual experiencing the corresponding WG-SS severity level\n(i.e. for UKR in 2022, 29% of individuals experiencing a severity WG-SS 2 includes the 13% of individuals experiencing WG-SS 3)\n* corresponds to prevalence below 1%"
    ) + theme_minimal() +
    theme(legend.position = "top", plot.title = element_text(size = 13, hjust = .5),
          plot.subtitle = element_text(size = 12, hjust = .5), plot.caption = element_text(hjust = 0, size = if (facet) NA else 10),
          legend.text = element_text(size = if (facet) NA else 11))

  if (facet) {plot <- plot + facet_wrap(~year) + coord_flip()}

  return(plot)
}

graph_prevalence.mark <- function(df, facet=T, size_geom=NULL, digit_geom=0, threshold_geom=0.01, flip=T, alphabetic=F){

  lev <- levels(df$facet)
  c.order <- df %>% filter(question=="wgq_dis_1") %>% arrange(mean) %>% pull(country) %>% unique
  c.y.order <- df %>% filter(question=="wgq_dis_1") %>% arrange(mean) %>% pull(facet) %>% unique
  if (alphabetic) {
    c.order <- sort(c.order)
    c.y.order <- sort(c.y.order)
  }

  col_fill <- c("#C7C8CA", "#58585A", "#F3BEBD", "#F27D7C", "#EE5859") %>% setNames(rev(unique(df$choice_label)))
  df <- df %>% mutate(value = str_replace_all(question, "wgq_dis_", ""))
  if (facet) df$country <- factor(df$country, levels = c.order) else df$country <- factor(df$facet, levels = rev(c.y.order))
  if (!is.null(lev)) df$facet <- factor(df$facet, levels = lev)

  if (is.null(size_geom)) size_geom <- 2

  plot <- ggplot(df, aes(y = mean_diff, x = country, fill = choice_label)) +
    geom_bar(position = position_stack(reverse = FALSE), stat = "identity") +
    geom_text(data = df %>% arrange(desc(choice_label)), aes(label = ifelse(mean > threshold_geom, paste0(round(100 * mean, digit_geom), "%"), "*"),
                                                             group = year, y = mean_diff),
              position = position_stack(reverse = FALSE, vjust = 0.5), size = if (facet) size_geom else size_geom+.35) +
    scale_fill_manual(na.value = "#E6DDCA", values = col_fill, na.translate = FALSE, labels = function(x) stringr::str_wrap(x, width = 25)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5)) +
    labs(title = "MSNA cross crisis analysis\nDisability prevalence by WG-SS severity level",
         x = if (facet) "country" else "country / year", y = "% of individuals", fill = "WG-SS\nseverity level", subtitle = "By country and year",
         caption = "Prevalence shown in graph correspond to the % individual experiencing the corresponding WG-SS severity level\n(i.e. for UKR in 2022, 29% of individuals experiencing a severity WG-SS 2 includes the 13% of individuals experiencing WG-SS 3)\n* corresponds to prevalence below 1%"
    ) + theme_minimal() +
    theme(legend.position = "top", plot.title = element_text(size = 13, hjust = .5),
          plot.subtitle = element_text(size = 12, hjust = .5), plot.caption = element_text(hjust = 0, size = if (facet) NA else 10),
          legend.text = element_text(size = if (facet) NA else 11))

  if (facet) {plot <- plot + facet_wrap(~year)}

  ## if facet=F and no argument value is entered to flip (default value) then define flip = F
  if (!facet & missing(flip)) flip <- FALSE

  if (flip) plot <- plot + coord_flip()

  return(plot)

}

plot.scatter <- function(df) {
  df %>% ggplot(aes(y=mean, x=choice_label, fill=country_col, colour = country_col)) +
    geom_point() + theme_minimal() +
    scale_color_brewer(palette = "PuOr") + scale_fill_brewer(palette = "PuOr") +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0,.5)) +
    scale_x_discrete(labels=~str_wrap(., width=15)) +
    # geom_smooth(method = "lm", se = F, aes(group = country_col, color=country_col), size = 0.5) +
    geom_smooth(method = "glm", formula = y ~poly(x, 2), se = F, aes(group = country_col, color=country_col), size = 0.5)+
    labs(x="WG-SS severity level", y="Prevalence\n(% of individuals)", fill="country", color="country",
         title = "Prevalence of WG-SS severity levels by country", subtitle = "Linear regression line by country over available years") +
    theme(legend.position = "bottom", plot.title = element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
}


loliplot <- function(df=res.wgq %>% filter(str_detect(question, "msni.*_need")), facet="question_metric", flip=T,
                     col_x = "country_facet", reorder=T, n.col=5, ci=F, s.l=3, vjust.mark=0.01, hjust.mark=0){

  df <- df %>% mutate(country_facet=paste0(country, " ", gsub("^20","",year)))
  if (reorder) {
    lev <- df %>% group_by(!!sym(col_x)) %>% arrange(mean) %>% pull(!!sym(col_x)) %>% unique
    df <- df %>% mutate(!!sym(col_x) := factor(!!sym(col_x), levels=lev))
  }

  ## if both "mean/low" and "mean/upp" in colnames, check in df if at least one and none CI don't overlap, if so populate mark_sign with * ""
  if (all(c("mean/low", "mean/upp") %in% colnames(df))){
    df <- df %>%
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
  }

  plot <- df %>%
    ggplot(aes(x=!!sym(col_x), y=mean)) +
    geom_segment(aes(xend=!!sym(col_x), y=min_y, yend=mean, color=max_color), size=0.5, show.legend = F) +
    geom_point(aes(color=wgq_dis_at_least_one, fill=wgq_dis_at_least_one), size=1) +
    labs(color=paste0("HH with\nWG-SS ",s.l," individual"), fill=paste0("HH with\nWG-SS ",s.l," individual"),
         title="Prevalence of needs", subtitle=paste0("Disaggregated by HH with at least one individual with disability (severity ", s.l,")")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Percentage labels
    scale_fill_manual(values=c("indianred", "#0067A9")) + scale_colour_manual(values=c("indianred", "#0067A9")) + theme_minimal() +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom")

  ## add geom_errorbar CI if (ci=T)
  if (ci) plot <- plot + geom_errorbar(aes(ymin=`mean/low`, ymax=`mean/upp`), width=0.2)

  ## add geom_text mark_sign to plot in the middle of geom_segment, with vjust to make it slightly above
  if (all(c("mean/low", "mean/upp") %in% colnames(df)) & (!ci)) {
    plot <- plot + geom_text(aes(y=mark_height, label=mark_sign), hjust=hjust.mark, vjust=-vjust.mark, size=2.5) +
      labs(caption="* Prevalence difference is significantly different from 0 at 5% level (95% confidence intervals not overlapping)")
  }


  ## do facet using quoted vector of any length !!!syms(facet)
  plot <- plot + facet_wrap(as.formula(paste0("~ ", paste0(facet, collapse=" + "))), ncol = n.col)

  if (flip) plot <- plot + coord_flip()
  plot

  return(plot)
}

plot.diff.point <- function(df=res.diff, var){
  res.diff.filt <- res.diff %>% mutate(country_year=paste0(country, "\n", gsub("^20", "", year))) %>% filter(question==var) %>% add.lab
  plot <- res.diff.filt %>%
    ggplot(aes(x=reorder(country_year, - prevalence_difference), y=prevalence_difference)) + geom_point(color="indianred") +
    geom_segment(aes(y=0, yend = prevalence_difference), color="indianred")+
    geom_text(aes(label=paste0(round(100 * prevalence_difference, 1), "%"), vjust = -1.5), size=2.5)+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-.05,.5)) +
    scale_fill_manual(values=c("indianred", "#0067A9")) + scale_colour_manual(values=c("indianred", "#0067A9")) + theme_minimal() +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") +
    labs(title=paste0(tolower(unique(res.diff.filt$question_metric))), y="",x="")
  return(plot)
}


plot.diff <- function(df=res.diff, var="msni_need"){
  df.var <- df %>% filter(question==var) %>% mutate(mean_value=mean)
  sd.var <- sd(df.var$prevalence_difference, na.rm=T)
  model_var <- lm(prevalence_difference ~ mean_value, data=df.var)
  var_lab <- df.var %>% pull(var_lab) %>% unique
  df.var <- df.var %>%
    mutate(prediction=predict(model_var, .), pred_error=prevalence_difference - prediction,
           country_lab = ifelse(abs(pred_error) > .75*sd.var, paste0(country, " ", gsub("^20", "", year)), ""))

  eq <- paste0("R² = ", round(summary(model_var)$r.squared, 2), "\n",
               "y = ", round(coef(model_var)[1], 2), " + ", round(coef(model_var)[2], 2), "x")
  plot <- df.var %>%
    ggplot(aes(x=mean_value, y=prevalence_difference)) + geom_point() + geom_smooth(method="lm", se = F) +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    ## ensure that min y lab cannot be >0
    scale_y_continuous(labels=scales::percent_format(accuracy=1), limits = c(min(0, min(df.var$prevalence_difference)), max(df.var$prevalence_difference)))+
    labs(title=str_wrap(paste0(var_lab, " prevalence difference"), 50),
         subtitle=str_wrap(paste0("Prevalence difference between HHs with at least one individual with disability and HHs with none"), 50),
         x=paste0("Overall prevalence of ", tolower(var_lab)), y="Prevalence difference") +
    geom_text(aes(label=country_lab), hjust=-0.25, vjust=0.5, size=3) +
    theme_minimal()+theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) +
    annotate("text", x = (max(df.var$mean)+min(df.var$mean))/2, y = min(df.var$prevalence_difference), label = eq, hjust = 0, vjust = 0, size = 3, parse = FALSE)

  return(plot)

}

