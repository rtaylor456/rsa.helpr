# The following code provides examples of using regular expressions to extract
#   and group variable names of similar variable type. This is not an extensive
#   script of all variable names/groupings.

######################
## DATE             ##
######################
# DATE columns - in excel date format
# start, extension, end
date_cols <- grep("(?i)_date|(?i)_skill_gain|(?i)_start|(?i)_end_|(?i)_extension(?!.*(?i)_desc)",
                  names(data), value = TRUE,
                  perl = TRUE)


######################
## FACTORS: nominal ##
######################
# CODE columns
# different kinds of values--zip code, agency code, etc. -- all factors
code_cols <- grep("((?i)_code_|(?i)_plan_occ|(?i)_referral_source|(?i)_exit_occ)(?!.*(?i)_desc)",
                  names(data), value = TRUE, perl = TRUE)

# VENDOR columns - should be values 1,2,3,4 or blank
vendor_cols <- grep("(?i)_vendor(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

### 1, 0, 9 columns: ###
# PROVIDER and PURCHASE columns
prov_purch_cols <- grep("((?i)_provide|(?i)_purchase)(?!.*(?i)_desc)",
                        names(data), value = TRUE, perl = TRUE)

# DEMOGRAPHIC columns - 0, 1, 9
race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                  names(data),
                  value = TRUE, perl = TRUE)

veteran_col <- grep("(?i)veteran(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp))",
                    names(data), value = TRUE, perl = TRUE)

has_disability_col <- grep("(?i)(has_disability)(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                           names(data), value = TRUE, perl = TRUE)

q1_q1_employ_col <- grep("(?i)(employer_match|q1_q2_match|q1_q2_employ)(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                         names(data), value = TRUE, perl = TRUE)

adult_cols <- grep("(?i)adult(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                   names(data), value = TRUE, perl = TRUE)

service_work_cols <- grep("(?i)(dislocated_worker|job_corps|voc_rehab|wagner_peyser|youth|equivalent|se_goal)(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage))",
                          names(data), value = TRUE, perl = TRUE)


plan_cols <- grep("(?i)plan_(?!.*(_desc|description|_amt|amount|amnt|vendor|title|comp|hours|date|ext|wage|status|occ|grade|farm))",
                  names(data), value = TRUE, perl = TRUE)

# Combine all demographic-related column names
demographic_cols <- c(race_cols, veteran_col, has_disability_col,
                      q1_q1_employ_col, adult_cols, service_work_cols,
                      plan_cols)

# Combine all binary column names
binary_cols <- c(prov_purch_cols, demographic_cols)


# SEX column
sex_cols <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                 value = TRUE, perl = TRUE)


# EMPLOYMENT columns
# 1-4, 9, 0 --factors--potentially ordinal...
employ_cols <- grep("(?i)_employ", names(data), value = TRUE, perl = TRUE)
employ_cols <- employ_cols[!grepl("wage|match|desc", employ_cols,
                                  ignore.case = TRUE)]

# E356_Exit_Work_Status_911
# this is our exit work status--this is an important variable
exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                      names(data), value = TRUE, perl = TRUE)


special_cols <- grep("((?i)_app_pub|(?i)_app_med|(?i)_exit_pub|(?i)_exit_med|(?i)_swd_age_)(?!.*(?i)_Desc)",
                     names(data), value = TRUE, perl = TRUE)


# COMP columns - can enter a max of 3 values
comp_cols <- grep("(?i)_comp_(?!.*(?i)_desc)", names(data), value = TRUE,
                  perl = TRUE)
comp_cols <- comp_cols[!grepl("provide|amt|date", comp_cols,
                              ignore.case = TRUE)]


other_factor_cols <- grep("((?i)_disability_priority|(?i)_secondary_enrollment)(?!.*(?i)_desc)",
                          names(data),
                          value = TRUE, perl = TRUE)


# E84_PostSecondary_Enrollment_911
post_sec_cols <- grep("(?i)_postsecondary_enroll(?!.*(?i)_desc|(?i)_date)",
                      names(data), value = TRUE, perl = TRUE)


# E354_Exit_Type_911
exit_var_cols <- grep("((?i)_exit_type)(?!.*(?i)_desc|(?i)_date)",
                      names(data), value = TRUE, perl = TRUE)


# E378_PostExit_Credential_911
exit_cred_cols <- grep("(?i)_postexit_credential(?!.*(?i)_desc|(?i)_date)",
                       names(data), value = TRUE, perl = TRUE)
