## Access to phone, by best type of phone accessible in the HH  ----------------
#' @title Access to phone, by best type of phone accessible in the HH
#' @description This function creates a new variable that indicates the best type of phone accessible in the HH.
#' @param df The input data frame
#' @param country The country name
#' @param col_access The name of the column that contains the access to phone information. It should have only four possible values according to MSNA indicator bank: "smartphone", "feature_phone", "basic_phone" and "none", alongside "dnk", "pnta"
#' @return The data frame with the new variable

etc_access_phone_best <- function(
    df = dp$MLI$main,
    country = NULL,
    col_access = "etc_access_to_phone"){
  print(paste0(" Country: ", country))
  if (!is.null(NULL)) print(paste0(" Country: ", country))
  if (col_access %in% colnames(df)){
    df <- df %>%
      mutate(etc_access_to_phone_select_one = case_when(
        str_detect(!!sym(col_access), "smartphone") ~ "smartphone",
        str_detect(!!sym(col_access), "feature_phone") ~ "feature_phone",
        str_detect(!!sym(col_access), "basic_phone") ~ "basic_phone",
        str_detect(!!sym(col_access), "none") ~ "none",
        !!sym(col_access) == "dnk" ~ "dnk",
        !!sym(col_access) == "pnta" ~ "pnta",
        T ~ NA_character_))
    print("etc_access_to_phone_select_one indicator created.")
  }
  return(df)
}

## Internet coverage (if all combinaisons are available)  ----------------------
#' @title function to recode coverage network type for cross crisis mainly and have Internet coverage 
#' @description This function creates a new variable that indicates the type of network coverage for internet access.
#' @param df The input data frame
#' @param country The country name
#' @param col_coverage The name of the column that contains the coverage network type information. It should have only four possible values according to MSNA indicator bank: "only_internet", "voice_sms_internet", "only_voice", "only_sms", "no_coverage" and "dnk"
#' @return The data frame with the new variable
#' 
etc_coverage_internet <- function(
    df = dp$SOM$main,
    country = NULL,
    col_coverage = "etc_coverage_network_type"){
  if (!is.null(NULL)) print(paste0(" Country: ", country))
  if (col_coverage %in% colnames(df)){
    df <- df %>%
      mutate(
        etc_coverage_internet = case_when(
          str_detect(!!sym(col_coverage), "\\bonly_internet\\b|\\bvoice_sms_internet\\b") ~ "internet",
          str_detect(!!sym(col_coverage), "\\bonly_voice\\b|\\bvoice_sms\\b") ~ "voice_no_internet",
          str_detect(!!sym(col_coverage), "\\bonly_sms\\b") ~ "only_sms",
          str_detect(!!sym(col_coverage), "no_coverage") ~ "no_coverage",
          !!sym(col_coverage) == "dnk" ~ "dnk",
          !!sym(col_coverage) == "pnta" ~ "pnta",
          T ~ NA_character_
        ) 
      )
    print("etc_coverage_internet indicator created.")
  }
  return(df)
}

## Network coverage and phone owned in HH combined  ----------------------
#' @title function to recode coverage network type and phone owned in HH combined
#' @description This function creates a new variable that indicates the type of network coverage and phone owned in HH combined.
#' @param df The input data frame
#' @param country The country name
#' @param col_coverage_internet The name of the column that contains the coverage network type information. It should have only four possible values according to MSNA indicator bank: "only_internet", "voice_sms_internet", "only_voice", "only_sms", "no_coverage" and "dnk"
#' @param col_access The name of the column that contains the access to phone information. It should have only four possible values according to MSNA indicator bank: "smartphone", "feature_phone", "basic_phone" and "none", alongside "dnk", "pnta"
#' @return The data frame with the new variable
#' 
etc_no_coverage_or_phone <- function(df=data_aap$`HTI-admin1`, 
                                     country=NULL,
                                     col_coverage_internet="etc_coverage_internet",
                                     col_access="etc_access_to_phone_select_one"){
  print(paste0(" Country: ", country))
  if (!is.null(NULL)) print(paste0(" Country: ", country))
  if (col_coverage_internet %in% colnames(df) & col_access %in% colnames(df)){
    df <- df %>%
      mutate(
        etc_coverage_phone_combined = case_when(
          !!sym(col_coverage_internet) %in% "no_coverage" | !!sym(col_access) %in% "none" ~ "no_coverage_or_phone",
          !!sym(col_coverage_internet) %in% c("only_sms", "voice_no_internet") | !!sym(col_access) %in% c("basic_phone") ~ "no_internet_or_basic_phone",
          !!sym(col_coverage_internet) %in% c("internet") & !!sym(col_access) %in% c("feature_phone") ~ "internet_feature_phone",
          !!sym(col_coverage_internet) %in% c("internet") & !!sym(col_access) %in% c("smartphone") ~ "internet_smartphone",
          (!!sym(col_coverage_internet) %in% c("pnta", "dnk")) | (!!sym(col_access) %in% c("pnta", "dnk")) ~ "undefined",
          T ~ NA_character_
        )
      )
    print("etc_coverage_phone_combined indicator created.")
  }
  return(df)
}

## Indicator received assistance with type and date ----------------------
#' @title function to recode received assistance with type and date
#' @description This function creates a new variable that indicates the type of assistance received and the date of the assistance.
#' @param df The input data frame
#' @param country The country name
#' @param col_received_assistance_12m The name of the column that contains the information about the assistance received in the last 12 months. It should have only two possible values according to MSNA indicator bank: "yes" and "no"
#' @param col_received_assistance_date The name of the column that contains the date of the assistance received. It should have only four possible values according to MSNA indicator bank: "past_month", "1_3_months", "4_6_months", "7_12_months" and "dnk"
#' @return The data frame with the new variable `aap_received_assistance_date_formatted`
#' 
## helper function
aap_prepare_received_assistance <- function(df=dp$BFA$main,
                                            country="NA",
                                            col_received_assistance_12m="aap_received_assistance_12m",
                                            col_received_assistance_date="aap_received_assistance_date") {
  print(paste0(" Country: ", country))
  if (sum(c(col_received_assistance_date, col_received_assistance_12m) %in% colnames(df))==2) {
    if (sum(str_detect(colnames(df), "aap_received_assistance\\.(past_month|3m|6m|12m)$"))==4) {
      paste0("new columns already created, will be deleted before function is run,")
      df <- df %>% select(-matches("aap_received_assistance\\.(past_month|3m|6m)$"))
    }
    df <- df %>%
      mutate(aap_received_assistance_date_formatted = case_when(
        !!sym(col_received_assistance_date)  %in% c("past_30d") ~ "past_month",
        !!sym(col_received_assistance_date)  %in% c("1_3_months") ~ "1_3m",
        !!sym(col_received_assistance_date)  %in% c("4_6_months") ~ "4_6m",
        !!sym(col_received_assistance_date)  %in% c("7_12_months") | !!sym(col_received_assistance_12m) == "yes" ~ "7_12m",
        !!sym(col_received_assistance_12m) == "no" ~ "no",
        !!sym(col_received_assistance_12m) %in% c("pnta", "dnk") | !!sym(col_received_assistance_date)  %in% c("pnta", "dnk") ~ "undefined",
        T ~ NA_character_)) %>% 
      mutate(
        aap_received_assistance_past_month = case_when(aap_received_assistance_date_formatted %in% c("past_month") ~ "yes", is.na(aap_received_assistance_date_formatted) ~ NA_character_, TRUE ~ "no"),
        aap_received_assistance_3m = case_when(aap_received_assistance_date_formatted %in% c("past_month", "1_3m") ~ "yes", is.na(aap_received_assistance_date_formatted) ~ NA_character_, TRUE ~ "no"),
        aap_received_assistance_6m = case_when(aap_received_assistance_date_formatted %in% c("past_month", "1_3m", "4_6m") ~ "yes", is.na(aap_received_assistance_date_formatted) ~ NA_character_, TRUE ~ "no"),
        aap_received_assistance_12m = case_when(aap_received_assistance_date_formatted %in% c("past_month", "1_3m", "4_6m", "7_12m") ~ "yes", is.na(aap_received_assistance_date_formatted) ~ NA_character_, TRUE ~ "no")
      ) 
  } else {print(paste0("columns ", col_received_assistance_12m, " and/or ", col_received_assistance_date, " missing."))}
  return(df)
}


## Any barriers to accessing humanitarian assistance encountered ---------------
#' @title function to recode any barriers to accessing humanitarian assistance encountered from the barriers to accessing humanitarian assistance by type indicator
#' @description This function creates a new variable that indicates whether any barriers to accessing humanitarian assistance were encountered.
#' @param df The input data frame
#' @param col_barrier The name of the column that contains the information about the barriers to accessing humanitarian assistance.
#' @param choice_none The choice value that indicates that no barriers were encountered.
#' @param choice_pnta The value value that indicates that the person prefers not to answer.
#' @param choice_dnk The value value that indicates that the person does not know.
#' @return The data frame with the new variable `aap_barriers_assistance_any_barrier`
#' 
add_any_barrier_aap <- function(df=dp$BFA$main,
                                col_barrier="aap_barriers_assistance",
                                choice_none="none",
                                choice_pnta="pnta",
                                choice_dnk="dnk"){
  
  ## check that col_barrier is present in dataframe
  # if_not_in_stop(df, col_barrier, "df")
  if (!col_barrier %in% colnames(df)) {
    print(paste0("Column ", col_barrier, " not found in dataframe."))
    return(df)
  }
  
  df <- df %>%
    mutate(
      aap_barriers_assistance_any_barrier = case_when(
        !!sym(col_barrier) %in% c(choice_none) ~ "no",
        !!sym(col_barrier) %in% c(choice_pnta) ~ "pnta",
        !!sym(col_barrier) %in% c(choice_dnk) ~ "dnk",
        !!sym(col_barrier) %in% c("NA", NA) ~ NA_character_,
        TRUE ~ "yes"
      )
    )
}

## Satisfaction with assistance received ----------------------



## Satisfaction with worker behaviour ----------------------






