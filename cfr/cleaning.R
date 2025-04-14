### Quick cleaning

# find duplicates and add them to deletion log
cleaning_log <- data.composed %>%
  check_duplicate(uuid_column = "_uuid",
                  log_name = "duplicate_log")

if (nrow(cleaning_log$duplicate_log)>0){
  stop("Duplicates detected --> add to deletion log")
}

# check for value(s) in the dataset
cleaning_log <- cleaning_log %>%
  check_value(uuid_column = "_uuid",
              element_name = "checked_dataset",
              values_to_look = c(99, 999, 999, 88, 888, 888))

# check no consent 
cleaning_log <- cleaning_log %>% 
  check_logical(
    uuid_column = "_uuid",
    information_to_add = NULL,
    check_id = "logical_reject",
    check_to_perform = "used_phone_recurrence == \"never\" & !is.na(owned_phone_type) & phones_in_household>0", 
    # check_to_perform = "consent == \"no\" | vendor_above_18_yn == \"no\"",
    columns_to_clean = c("used_phone_recurrence", "owned_phone_type", "phones_in_household"),
    description = "Respondent reported never using a phone and there is more than a phone in the HH and the "
  ) 
