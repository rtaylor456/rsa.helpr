## Working function, saved 1/13/25

clean_utah <- function(data,
                       aggregate = TRUE,
                       unidentified_to_0 = TRUE,
                       # convert_sex = TRUE,
                       # convert_employ = TRUE,
                       remove_desc = TRUE,
                       remove_strictly_na = TRUE,
                       clean_specials = NULL) {
  # remove redundancies in ids
  # add in a new variable that counts redundancies

  # Ensure data is a data.table
  setDT(data)

  ######################
  ## REMOVABLE        ##
  ######################
  # DESCRIPTION columns - unnecessary for analysis
  if (remove_desc){
    desc_cols <- grep("(?i)_desc", names(data), value = TRUE, perl = TRUE)

    # data_selected <- data[, setdiff(names(data), desc_cols), with = FALSE]
    data[, (desc_cols) := NULL]
  }

  # ADMIN columns
  # remove the extra administrative columns
  extra_cols <- grep("(?i)_data_(?!.*(?i)_desc)", names(data),
                     value = TRUE, perl = TRUE)
  data[, (extra_cols) := NULL]


  # EXTRA columns that occurs when writing to csv in R--who knows why
  # Remove column X or x if it exists
  # if ("X" %in% names(data)) {
  #   data[, X := NULL]
  # }
  # if ("x" %in% names(data)) {
  #   data[, x := NULL]
  # }

  # add additional miscellaneous columns to add
  columns_to_remove <- c("X", "x", "V1")
  data[, (columns_to_remove) := NULL, with = FALSE]


  ######################
  ## NUMERIC          ##
  ######################

  # E1_Year_911
  year_col <- grep("(?i)_year|(?i)_yr_(?!.*(?i)_desc)", names(data),
                   value = TRUE, perl = TRUE)


  # E2_Quarter_911
  quarter_col <- grep("(?i)_quarter|(?i)_qt_(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

  # AGE column
  # Rename the column
  # age_cols <- grep("(?i)age.*app|app.*age(?!.*(?i)_desc)", names(data),
  #                  value = TRUE, perl = TRUE)
  age_cols <- grep("(?i)^(?=.*age)(?=.*app)(?!.*(desc|amt))", names(data),
                   value = TRUE, perl = TRUE)

  names(data)[names(data) %in% age_cols] <- "Age_At_Application"


  # AMT and TITLE columns (TITLEI columns are funds expended for different
  #   services)
  # WAGE columns
  amt_cols <- grep("(?i)_amt|(?i)_title|(?i)_wage|(?i)_amount|(?i)_amnt(?!.*(?i)_desc)",
                   names(data), value = TRUE, perl = TRUE)

  # HOURS columns
  hours_cols <- grep("(?i)_hours_|(?i)_hrs_(?!.*(?i)_desc)", names(data),
                     value = TRUE, perl = TRUE)

  numeric_cols <- c(year_col, quarter_col, "Age_At_Application",
                    amt_cols, hours_cols)



  data[, (numeric_cols) := lapply(.SD, as.numeric),
       .SDcols = numeric_cols]


  ## CREATE NEW VARIABLES
  # Create enrollment length variable
  # data[, Overall_Quarter := ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)]
  #
  # data[, `:=`(Min_Overall_Quarter = min(Overall_Quarter),
  #             Max_Overall_Quarter = max(Overall_Quarter)),
  #      by = Participant_ID]
  #
  # # Compute Enroll Length
  # data[, Enroll_Length := Max_Overall_Quarter - Min_Overall_Quarter + 1]
  #
  #
  # # Create variables that count the number of years and quarters
  # data[, `:=`(Total_Years = uniqueN(E1_Year_911),
  #             Total_Quarters = uniqueN(E2_Quarter_911)),
  #      by = Participant_ID]


  ######################
  ## DATE             ##
  ######################
  # DATE columns - in excel date format
  # start, extension, end
  date_cols <- grep("(?i)_date|(?i)_skill_gain|(?i)_start|(?i)_end_|(?i)_extension(?!.*(?i)_desc)",
                    names(data), value = TRUE,
                    perl = TRUE)

  data[, (date_cols) := lapply(.SD, handle_excel_date), .SDcols = date_cols]


  ######################
  ## FACTORS: nominal ##
  ######################
  # CODE columns
  # different kinds of values--zip code, agency code, etc. -- all factors
  code_cols <- grep("((?i)_code_|(?i)_plan_occ|(?i)_referral_source|(?i)_exit_occ)(?!.*(?i)_desc)",
                    names(data), value = TRUE, perl = TRUE)

  data[, (code_cols) := lapply(.SD, handle_blanks),
       .SDcols = code_cols]


  # VENDOR columns - should be values 1,2,3,4 or blank
  vendor_cols <- grep("(?i)_vendor(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

  data[, (vendor_cols) := lapply(.SD, function(x){
    handle_values(x, values = c(1, 2, 3, 4))
  }), .SDcols = vendor_cols]

  ### 1, 0, 9 columns: ###
  # PROVIDER and PURCHASE columns
  prov_purch_cols <- grep("((?i)_provide|(?i)_purchase)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

  # DEMOGRAPHIC columns - 0, 1, 9
  # e10-e16, e42, e49, e54-e60, e62-e73,
  demographic_cols <- grep("^(E1[0-6]_|(E42|E49|E5[4-9]|E6[2-7]|E7[0-3])_)(?!.*(?i)_desc)",
                           names(data), value = TRUE, perl = TRUE)


  # "E63_Plan_Exhaust_TANF_911"           "E64_Plan_Foster_Care_911"
  # [19] "E65_Plan_Homeless_911"               "E66_Plan_Offender_911"
  # [21] "E67_Plan_Low_Income_911"             "E70_Plan_Cultural_Barriers_911"
  # [23] "E71_Plan_Single_Parent_911"          "E72_Plan_Displaced_Homemaker_911"
  # [25] "E73_Plan_Farmworker_911"
  # E68_Plan_English_Learner_911 E69_Plan_Skills_Deficient_911
  # E392_Q2_Q4_Employer_Match_911
  # E60_Youth_911
  # E400_Secondary_Equivalent_Enrollment_911

  other_binary_cols <- grep("^(E60_|E68_|E69_|E392_|E400_|VR_Case_Type_Flag)(?!.*(?i)_desc)",
                            names(data), value = TRUE, perl = TRUE)

  binary_cols <- c(prov_purch_cols, demographic_cols, other_binary_cols)

  data[, (binary_cols) := lapply(.SD, function(x) handle_nines(x, unidentified_to_0)),
       .SDcols = binary_cols]


  # SEX column
  sex_cols <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                   value = TRUE, perl = TRUE)

  # data[, (sex_cols) := lapply(.SD, function(x) handle_sex(x, convert_sex)),
  #      .SDcols = sex_cols]

  data[, (sex_cols) := lapply(.SD, function(x) {
    handle_values(x, c(1, 2, 3, 9), blank_value = 9)
  }), .SDcols = sex_cols]


  # E45_Disability_Priority_911 - 0, 1, 2, NULL (null means something different
  #                                               than 0)
  # E78_Secondary_Enrollment_911 - 0, 1,2, NULL (Nulls --> 0)

  other_factor_cols <- grep("((?i)_disability_priority|(?i)_secondary_enrollment)(?!.*(?i)_desc)",
                            names(data),
                            value = TRUE, perl = TRUE)

  data[, (other_factor_cols) := lapply(.SD, function(x){
    handle_values(x, c(0, 1, 2))
  }),
  .SDcols = other_factor_cols]

  # E355_Exit_Reason_911 - 2-19, NULL -- 02, 03, 04, 06, 07, 08, 13-22

  exit_reason_cols <- grep("(?i)_exit_reason(?!.*(?i)_desc)", names(data),
                           value = TRUE, perl = TRUE)

  data[, (exit_reason_cols) := lapply(.SD, function(x){
    handle_values(x, c(02, 03, 04, 06, 07, 08, 13:22))
  }),
  .SDcols = exit_reason_cols]


  ######################
  ## FACTORS: ordinal ##
  ######################

  # E84_PostSecondary_Enrollment_911
  post_sec_cols <- grep("(?i)_postsecondary_enroll(?!.*(?i)_desc|(?i)_date)",
                        names(data), value = TRUE, perl = TRUE)

  data[, (post_sec_cols) := lapply(.SD, function(x){
    handle_values(x, 0:3, blank_value = 0)
  }),
  .SDcols = post_sec_cols]

  # E354_Exit_Type_911
  # E356_Exit_Work_Status_911
  exit_var_cols <- grep("((?i)_exit_type_|(?i)_exit_work_)(?!.*(?i)_desc|(?i)_date)",
                        names(data), value = TRUE, perl = TRUE)

  data[, (exit_var_cols) := lapply(.SD, function(x){
    handle_values(x, 0:7, blank_value = 0)
  }),
  .SDcols = exit_var_cols]

  # E378_PostExit_Credential_911
  exit_cred_cols <- grep("(?i)_postexit_credential(?!.*(?i)_desc|(?i)_date)",
                         names(data), value = TRUE, perl = TRUE)

  data[, (exit_cred_cols) := lapply(.SD, function(x){
    handle_values(x, 1:8, blank_value = 0)
  }),
  .SDcols = exit_cred_cols]


  # EMPLOYMENT columns
  # 1-4, 9, 0 --factors--potentially ordinal...
  employ_cols <- grep("(?i)_employ", names(data), value = TRUE, perl = TRUE)
  employ_cols <- employ_cols[!grepl("wage|match|desc", employ_cols,
                                    ignore.case = TRUE)]
  # this is our exit work status--this is the important variable
  exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                        names(data), value = TRUE, perl = TRUE)

  data[, (employ_cols) := lapply(.SD, function(x){
    handle_values(x, c(0, 1, 2, 3, 4, 9), blank_value = 0)
  }),
  .SDcols = employ_cols]

  data[, (employ_cols) := lapply(.SD, function(x){
    handle_nines(x, unidentified_to_0)
  }),
  .SDcols = employ_cols]


  data[, (exit_work_col) := lapply(.SD, function(x){
    handle_values(x, c(0:5, 7, 9), blank_value = 0)
  }),
  .SDcols = exit_work_col]


  # if (convert_employ == TRUE){
  #   data[, (employ_cols) := lapply(.SD, function(x) ifelse(x == 4, 1, 0)),
  #        .SDcols = employ_cols]
  #
  #   data[, (exit_work_col) := lapply(.SD, function(x) ifelse(x == 1, 1, 0)),
  #        .SDcols = exit_work_col]
  # }

  # create a new variable for employment status with a name that's easier to
  #    reference
  data[, Final_Employment := lapply(.SD, function(x) ifelse(x == 1, 1, 0)),
       .SDcols = exit_work_col]


  # GRADE LEVEL - based on age
  data[, Age_Group := fifelse(Age_At_Application < 5, "<5",
                              fifelse((Age_At_Application >= 5 &
                                         Age_At_Application < 8), "5-7",
                                      fifelse((Age_At_Application >= 8 &
                                                 Age_At_Application < 11), "8-10",
                                              fifelse((Age_At_Application >= 11 &
                                                         Age_At_Application < 14), "11-13",
                                                      fifelse((Age_At_Application >= 14 &
                                                                 Age_At_Application < 17), "14-16",
                                                              fifelse((Age_At_Application >= 17 &
                                                                         Age_At_Application < 20), "17-19",
                                                                      fifelse((Age_At_Application >= 20 &
                                                                                 Age_At_Application < 23), "20-22",
                                                                              fifelse((Age_At_Application >= 23 &
                                                                                         Age_At_Application < 26), "23-25",
                                                                                      fifelse((Age_At_Application >= 26 &
                                                                                                 Age_At_Application < 31), "26-30",
                                                                                              fifelse((Age_At_Application >= 31 &
                                                                                                         Age_At_Application <= 50), "31-50",
                                                                                                      fifelse(Age_At_Application > 50, "40+",
                                                                                                              NA_character_)))))))))))]

  # Convert Age_Group to an ordered factor
  data[, Age_Group := factor(Age_Group,
                             levels = c("<5", "5-7", "8-10", "11-13",
                                        "14-16", "17-19", "20-22",
                                        "23-25", "26-30", "31-50",
                                        "40+"),
                             ordered = TRUE)]


  ########################
  ## SPECIAL CHARACTERS ##
  ########################
  # E394_App_Public_Support_911 - 0, 1-4
  # E395_App_Medical_911 - 0, 1-7 - limit of 3 types
  # E396_Exit_Public_Support_911 - 0, 1-4
  # E397_Exit_Medical_911 - 0, 1-7 - limit of 3 types
  # E74_SWD_Age_911 - two values, ages

  special_cols <- grep("((?i)_app_pub|(?i)_app_med|(?i)_exit_pub|(?i)_exit_med|(?i)_swd_age_)(?!.*(?i)_Desc)",
                       names(data), value = TRUE, perl = TRUE)

  # COMP columns - can enter a max of 3 values
  comp_cols <- grep("(?i)_comp_(?!.*(?i)_desc)", names(data), value = TRUE,
                    perl = TRUE)
  comp_cols <- comp_cols[!grepl("provide|amt|date", comp_cols,
                                ignore.case = TRUE)]

  all_special_cols <- c(special_cols, comp_cols)

  # uncomment this once I've got the function to stop crashing the computer
  # data <- handle_splits(data, all_special_cols)

  # this is too slow!
  # if (clean_specials == TRUE){
  #   data <- apply_handle_splits(data, all_special_cols, sep = ";")
  # }

  if (!is.null(clean_specials) & length(all_special_cols) > 0) {
    data <- apply_handle_splits(data, clean_specials, sep = ";")
  }


  # DISABILITY columns
  disability_cols <- grep("(?i)(primary|secondary).*disability(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)
  data <- separate_disability(data)

  impairment_vars <- c("Primary_Impairment", "Secondary_Impairment")
  group_vars <- paste0(impairment_vars, "_Group")

  # Apply the same logic to both columns at once using lapply
  data[, (group_vars) := lapply(.SD, function(x) fifelse(x == 0, "None",
                                                         fifelse(x %in% c(1, 2, 8),
                                                                 "Visual",
                                                                 fifelse(x %in% c(3, 4, 5, 6, 7,
                                                                                  9),
                                                                         "Auditory/Commun.",
                                                                         fifelse(x %in% c(10, 11, 12,
                                                                                          13, 14, 15,
                                                                                          16),
                                                                                 "Physical",
                                                                                 fifelse(x == 17,
                                                                                         "Intellectual/Learn.",
                                                                                         fifelse(x %in% c(18, 19),
                                                                                                 "Psychological",
                                                                                                 NA_character_))))))),
       .SDcols = impairment_vars]

  # Set factor levels for both columns
  data[, (group_vars) := lapply(.SD, factor,
                                levels = c("None",
                                           "Visual",
                                           "Auditory/Commun.",
                                           "Physical",
                                           "Intellectual/Learn.",
                                           "Psychological")),
       .SDcols = group_vars]


  # # Primary_Impairment
  # data[, Primary_Impairment_Group := fifelse(Primary_Impairment == 0,
  #                                            "None",
  #                               fifelse(Primary_Impairment %in% c(1, 2, 8),
  #                                       "Visual",
  #                               fifelse(Primary_Impairment %in%
  #                                         c(3, 4, 5, 6, 7, 9),
  #                                       "Auditory/Commun.",
  #                               fifelse(Primary_Impairment %in%
  #                                         c(10, 11, 12, 13, 14, 15, 16),
  #                                       "Physical",
  #                               fifelse(Primary_Impairment == 17,
  #                                       "Intellectual/Learn.",
  #                               fifelse(Primary_Impairment %in% c(18, 19),
  #                                       "Psychological", NA_character_))))))]
  #
  # data[, Primary_Impairment_Group := factor(Primary_Impairment_Group,
  #                                           levels = c("None", "Visual",
  #                                                    "Auditory/Commun.",
  #                                                    "Physical",
  #                                                    "Intellectual/Learn.",
  #                                                    "Psychological"))]
  #




  ##############################################################################
  ########################
  ## Type conversion    ##
  ########################

  # for now, convert everything to factors except numeric_cols

  # Convert all other columns to factors
  factor_cols <- unique(setdiff(names(data), c(numeric_cols, date_cols)))
  data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

  # DATA AGGREGATION
  if (aggregate) {
    data <- data[!is.na(E7_Application_Date_911)]
    # Order the data by Participant_ID, E1_Year_911, E2_Quarter_911,
    #    and E7_Application_Date_911
    setorder(data, Participant_ID, E1_Year_911, E2_Quarter_911,
             -E7_Application_Date_911)

    # Create a helper column to identify the first occurrence within each group
    data[, Occurrences_Per_Quarter := .N, by = .(Participant_ID, E1_Year_911,
                                                 E2_Quarter_911)]
    data <- data[, .SD[1], by = .(Participant_ID, E1_Year_911, E2_Quarter_911)]

    # Sort by year and quarter
    setorder(data, E1_Year_911, E2_Quarter_911)
  }

  # REMOVE COLUMNS WITH ONLY NAs
  if (remove_strictly_na){
    data <- data[, which(unlist(lapply(data, function(x)!all(is.na(x))))),
                 with = FALSE]
  }

  # return the cleaned dataset
  return(data)

}

