################################################################################
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
extra_cols <- grep("(?i)_data_", names(data), value = TRUE, perl = TRUE)
data[, (extra_cols) := NULL]

################################################################################
######################
## FACTORS: nominal ##
######################
# CODE columns

# Participant_ID -- codes, factors - nominal
# E51_Plan_Occupation_911 - codes, NA, factor, nominal
# E21_Referral_Source_911 - codes with Nulls, factor nominal - values 01-32
# E357_Exit_Occupation_911 - codes with NULLs and NA - factor nominal
#   special values: 899999 and 999999 -- randolph shephard employees

# CODE columns
# different kinds of values--zip code, agency code, etc. -- all factors
code_cols <- grep("(?i)_code_|(?i)_plan_occ|(?i)_referral_source|(?i)_exit_occ",
                  names(data), value = TRUE, perl = TRUE)

data[, (code_cols) := lapply(.SD, handle_blanks),
     .SDcols = code_cols]


# VENDOR columns - should be values 1,2,3,4 or blank
vendor_cols <- grep("(?i)_vendor", names(data), value = TRUE, perl = TRUE)

data[, (vendor_cols) := lapply(.SD, function(x){
  handle_values(x, c(1, 2, 3, 4))
  }),
     .SDcols = vendor_cols]

# 1, 0, 9 columns:

# PROVIDER and PURCHASE columns - values 0 or 1
prov_purch_cols <- grep("(?i)_provide|(?i)_purchase", names(data),
                        value = TRUE, perl = TRUE)


# DEMOGRAPHIC columns - 0, 1, 9
# e10-e16, e42, e49, e54-e60, e62-e73,
demographic_cols <- grep("^(E1[0-6]_|(E42|E49|E5[4-9]|E6[2-7]|E7[0-3])_)",
                         names(data), value = TRUE, perl = TRUE)

# E60_Youth_911 - 0, 1, NULL
# E68_Plan_English_Learner_911 - 0, 1, NULL (should be 9)
# E69_Plan_Skills_Deficient_911 - 0, 1, 9
# E392_Q2_Q4_Employer_Match_911 - 0, 1, NULL (should just be 0 or 1)
# E400_Secondary_Equivalent_Enrollment_911 - 0, 1, NULL (should just be 0 or 1)
# E392_Q2_Q4_Employer_Match_911 - 0, 1, NULL

# VR_Case_Type_Flag -- 1, 0 -- not sure, probably nominal factor

other_binary_cols <- grep("^(E60_|E68_|E69_|E392_|E400_|VR_Case_Type_Flag|Employer_Match)",
                          names(data), value = TRUE, perl = TRUE)

binary_cols <- c(prov_purch_cols, demographic_cols, other_binary_cols)

data[, (binary_cols) := lapply(.SD, handle_nines),
     .SDcols = binary_cols]


# SEX column
# E9_Gender_911 - 1,2,9, NULL - factor
# SEX column
sex_cols <- grep("(?i)_sex|(?i)_gender", names(data),
                 value = TRUE, perl = TRUE)

data[, (sex_cols) := lapply(.SD, function(x) handle_sex(x, convert_sex)),
     .SDcols = sex_cols]


# E45_Disability_Priority_911 - 0, 1, 2, NULL (null means something different
#                                               than 0)
# E78_Secondary_Enrollment_911 - 0, 1,2, NULL (Nulls --> 0)

other_factor_cols <- grep("(?i)_disability_priority|(?i)_secondary_enrollment",
                      names(data),
                      value = TRUE, perl = TRUE)

data[, (other_factor_cols) := lapply(.SD, function(x){
  handle_values(x, c(0, 1, 2))
  }),
  .SDcols = other_factor_cols]

# E355_Exit_Reason_911 - 2-19, NULL -- 02, 03, 04, 06, 07, 08, 13-22

exit_reason_col <- grep("(?i)_exit_reason", names(data),
                        value = TRUE, perl = TRUE)

data[, (exit_reason_col) := lapply(.SD, function(x){
  handle_values(x, c(02, 03, 04, 06, 07, 08, 13-22))
  }),
  .SDcols = exit_reason_col]


################################################################################
######################
## FACTORS: ordinal ##
######################

# E84_PostSecondary_Enrollment_911 - 0, 1, 2, 3, NULL (null -> 0)
#   factor, possibly ordinal 0, 3, 2, 1 --> better
factor(data$E84_PostSecondary_Enrollment_911,
       levels = c(0, 3, 2, 1), ordered = TRUE)



# E354_Exit_Type_911 - 0, 3, 4, 6, 7, NULL -- 1, 2, 3, 4, 5, 6, 7, 0
#   potentially ordinal factor
factor(data$E354_Exit_Type_911,
       levels = c())

# E356_Exit_Work_Status_911 - 1, 2, 4, 5, NULL -- 1, 2, 3, 4, 5, 7
#   1: competitive employment


# E378_PostExit_Credential_911 - 1-7, NULL -- 1-8 factor, potentially ordinal


# EMPLOYMENT columns
# 1-4, 9, 0 --factors--potentially ordinal...
employ_cols <- grep("(?i)_employ", names(data), value = TRUE, perl = TRUE)
employ_cols <- comp_cols[!grepl("wage|match", employ_cols,
                              ignore.case = TRUE)]

################################################################################
######################
## NUMERIC          ##
######################
# E1_Year_911
year_cols <- grep("(?i)_year|(?i)_yr_", names(data), value = TRUE, perl = TRUE)


# E2_Quarter_911
quarter <- grep("(?i)_quarter|(?i)_qt_", names(data), value = TRUE, perl = TRUE)

# AGE column
# Rename the column and convert to numeric
setnames(data, "Age at Application", "Age_at_Application")
data[, Age_at_Application := as.numeric(Age_at_Application)]


# AMT and TITLE columns (TITLEI columns are funds expended for different
#   services)
# WAGE columns
amt_cols <- grep("(?i)_amt|(?i)_title|(?i)_wage|(?i)_amount|(?i)_amnt",
                 names(data), value = TRUE, perl = TRUE)

# HOURS columns
hours_cols <- grep("(?i)_hour|(?i)_hr", names(data),
                   value = TRUE, perl = TRUE)



################################################################################
######################
## DATE             ##
######################
# DATE columns - in excel date format
# start, extension, end
date_cols <- grep("(?i)_date|(?i)_skill_gain|(?i)_start|(?i)_end_|(?i)_extension",
                  names(data), value = TRUE,
                  perl = TRUE)

data[, (date_cols) := lapply(.SD, handle_excel_date), .SDcols = date_cols]

################################################################################
########################
## SPECIAL CHARACTERS ##
########################
# E394_App_Public_Support_911 - 0, 1-4
# E395_App_Medical_911 - 0, 1-7 - limit of 3 types
# E396_Exit_Public_Support_911 - 0, 1-4
# E397_Exit_Medical_911 - 0, 1-7 - limit of 3 types
# E74_SWD_Age_911

# COMP columns - can enter a max of 3 values
comp_cols <- grep("(?i)_comp_", names(data), value = TRUE, perl = TRUE)
comp_cols <- comp_cols[!grepl("provide|amt|desc|date", comp_cols,
                              ignore.case = TRUE)]



# HANDLE DISABILITY columns
# call separate_disability function
data <- separate_disability(data)


