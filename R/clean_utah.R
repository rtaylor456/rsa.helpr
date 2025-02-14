#' Clean Utah RSA-911 Data
#'
#' This function cleans a Utah RSA-911 dataset, based on the standard data
#'   structure.
#'
#' @param data The Utah RSA-911 dataset.
#' @param aggregate TRUE or FALSE. Defaults to TRUE, when TRUE, rows are
#'   aggregated to include only unique combinations of participants, year,
#'   and quarter.
#' @param unidentified_to_O TRUE or FALSE. Defaults to TRUE, when TRUE,
#'   variables where unidentified is represented by 9,
#'   values are converted to 0.
#' @param remove_desc TRUE or FALSE. Defaults to TRUE, when TRUE, description
#'   variables are removed to minimize redundancy.
#' @param remove_strictly_na TRUE or FALSE. Defaults to TRUE, when TRUE,
#'   variables that contain only NA values are removed.
#' @param clean_specials A character vector of name(s) of variables with special
#'   characters to be cleaned. New, separate variables for each value space will
#'   be appended to end of dataset with the the following naming convention:
#'   original_variable_name_Place1, original_variable_name_Place2, etc.
#'   Defaults to NULL. Note that if many are listed, cleaning process will be
#'   very slow.
#'
#' @returns A cleaned data frame, with incorrect, blank, messy values replaced,
#'   additional, helpful variables created, and unnecessary variables removed.
#'
#' @export
#' @import data.table

clean_utah <- function(data,
                       aggregate = TRUE,
                       unidentified_to_0 = TRUE,
                       remove_desc = TRUE,
                       remove_strictly_na = TRUE,
                       clean_specials = NULL) {

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
  # remove the extra administrative columns, not needed for analysis
  extra_cols <- grep("(?i)_data_|(?i)VR_Case_Type_Flag(?!.*(?i)_desc)",
                     names(data),
                     value = TRUE, perl = TRUE)
  data[, (extra_cols) := NULL]

  # Additional miscellaneous columns to remove
  columns_to_remove <- c("X", "x", "V1")

  # Check if the columns exist in the data before removing them
  existing_columns <- columns_to_remove[columns_to_remove %in% names(data)]

  # Remove only the columns that exist
  if (length(existing_columns) > 0) {
    data[, (existing_columns) := NULL]
  }


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
  age_cols <- grep("(?i)^(?=.*age)(?=.*app)(?!.*(desc|amt))", names(data),
                   value = TRUE, perl = TRUE)

  # Rename
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

  # Convert to numeric, ignoring warnings about NAs--this is what we want.
  data[, (names(data)) :=
         lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
       .SDcols = names(data)]


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
  race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                    names(data),
                    value = TRUE, perl = TRUE)
  # [1] "E10_Indian_Alaskan_911"            "E11_Asian_911"
  # [3] "E12_Black_African_911"             "E13_Hawaiian_Pacific_Islander_911"
  # [5] "E14_White_911"                     "E15_Hispanic_Latino_911"

  veteran_col <- grep("(?i)veteran(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp))",
                       names(data), value = TRUE, perl = TRUE)
  # [1] "E16_Veteran_Status_911"

  has_disability_col <- grep("(?i)(has_disability)(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                          names(data), value = TRUE, perl = TRUE)
  # [1] "E42_Has_Disability_911"

  q1_q1_employ_col <- grep("(?i)(employer_match|q1_q2_match|q1_q2_employ)(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                           names(data), value = TRUE, perl = TRUE)
  # [1] "E392_Q2_Q4_Employer_Match_911"

  adult_cols <- grep("(?i)adult(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                     names(data), value = TRUE, perl = TRUE)
  # [1] "E54_Adult_911"           "E55_Adult_Education_911"

  service_work_cols <- grep("(?i)(dislocated_worker|job_corps|voc_rehab|wagner_peyser|youth|equivalent|se_goal)(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                         names(data), value = TRUE, perl = TRUE)
  # [1] "E56_Dislocated_Worker_911"                "E57_Job_Corps_911"
  # [3] "E58_Voc_Rehab_911"                        "E59_Wagner_Peyser_911"
  # [5] "E60_Youth_911"                            "E61_YouthBuild_911"
  # [7] "E400_Secondary_Equivalent_Enrollment_911"

  plan_cols <- grep("(?i)plan_(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage|status|occ|grade|farm))",
                    names(data), value = TRUE, perl = TRUE)
  # [1] "E62_Plan_Long_Term_Unemployment_911" "E63_Plan_Exhaust_TANF_911"
  # [3] "E64_Plan_Foster_Care_911"            "E65_Plan_Homeless_911"
  # [5] "E66_Plan_Offender_911"               "E67_Plan_Low_Income_911"
  # [7] "E68_Plan_English_Learner_911"        "E69_Plan_Skills_Deficient_911"
  # [9] "E70_Plan_Cultural_Barriers_911"      "E71_Plan_Single_Parent_911"
  # [11] "E72_Plan_Displaced_Homemaker_911"

  demographic_cols <- c(race_cols, veteran_col, has_disability_col,
                        q1_q1_employ_col, adult_cols, service_work_cols,
                        plan_cols)

  binary_cols <- c(prov_purch_cols, demographic_cols)

  data[, (binary_cols) :=
         lapply(.SD, function(x) handle_nines(x, unidentified_to_0)),
       .SDcols = binary_cols]


  # SEX column
  sex_cols <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                   value = TRUE, perl = TRUE)

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


  # Create a new variable for employment status with a name that's easier to
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

  # There are too many special character variables, so it's best to let the
  #   user decide which variables they want to clean
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
                                                         "Aud./Comm.",
                                                 fifelse(x %in% c(10, 11, 12,
                                                                  13, 14, 15,
                                                                  16),
                                                         "Physical",
                                                 fifelse(x == 17,
                                                         "Intell./Learn.",
                                                 fifelse(x %in% c(18, 19),
                                                         "Psych.",
                                                         NA_character_))))))),
       .SDcols = impairment_vars]

  # Set factor levels for both columns
  data[, (group_vars) := lapply(.SD, factor,
                                levels = c("None",
                                           "Visual",
                                           "Aud./Comm.",
                                           "Physical",
                                           "Intell./Learn.",
                                           "Psych.")),
       .SDcols = group_vars]


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

    ## Initial checks
    # Make sure we have necessary columns for cleaning processes

    participant_col <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
                        names(data), value = TRUE, perl = TRUE)

    year_col <- grep("(?i)_year|(?i)_yr_(?!.*(?i)_desc)", names(data),
                     value = TRUE, perl = TRUE)

    quarter_col <- grep("(?i)_quarter|(?i)_qt_(?!.*(?i)_desc)", names(data),
                        value = TRUE, perl = TRUE)

    app_date_col <- grep("(?i)_app.*?(date)(?!.*(?i)_desc)", names(data),
                         value = TRUE, perl = TRUE)

    required_cols <- c(participant_col, year_col, quarter_col, app_date_col)


    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Check if essential columns have only NA values
    empty_cols <- sapply(data[, required_cols, with = FALSE],
                         function(x) all(is.na(x)))

    if (any(empty_cols)) {
      stop("Some required columns have no data: ",
           paste(names(empty_cols[empty_cols]), collapse = ", "))
    }


    names(data)[names(data) %in% participant_col] <- "Participant_ID"
    names(data)[names(data) %in% year_col] <- "Year"
    names(data)[names(data) %in% quarter_col] <- "Quarter"
    names(data)[names(data) %in% app_date_col] <- "Application_Date"

    # Remove rows where application date is missing --these are typically fully
    #   missing data rows anyway.
    data <- data[!is.na(Application_Date)]

    # Order the data by Participant_ID, year, quarter, and reverse order
    #   application date
    setorder(data, Participant_ID, Year, Quarter, -Application_Date)


    # Create a helper column to identify the first occurrence within each group
    data[, Occurrences_Per_Quarter := .N, by = .(Participant_ID,
                                                 Year,
                                                 Quarter)]
    # Save only the first occurrence
    data <- data[, .SD[1], by = .(Participant_ID,
                                  Year,
                                  Quarter)]

    # Sort by year and quarter
    setorder(data, Year, Quarter)
  }

  # REMOVE COLUMNS WITH ONLY NAs
  if (remove_strictly_na){
    data <- data[, which(unlist(lapply(data, function(x)!all(is.na(x))))),
                 with = FALSE]
  }

  # return the cleaned dataset
  return(data)

}

