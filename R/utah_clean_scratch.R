
data <- readRDS("data-raw/data_merged.rds")

library(data.table)

setDT(data)
grouped_data <- data[, .(occurrences_per_quarter = .N),
                     by = .(Participant_ID, E1_Year_911, E2_Quarter_911)]

names(grouped_data)

View(grouped_data)

# OLD function:

utah_clean <- function(data, na_check = TRUE, na_file = FALSE,
                       full_table_print = FALSE,
                       unidentified_to_0 = FALSE,
                       convert_sex = FALSE,
                       remove_desc = TRUE,
                       remove_strictly_na = FALSE){
  # remove unnecessary variables - description variables
  # convert date variables
  # remove redundancies in ids
  # add in a new variable that counts redundancies
  # separate variables with semicolons-disability
  # convert NAs to 0s
  # save variables as appropriate variable types - factor, numeric, etc.

  # Ensure data is a data.table
  setDT(data)

  # DESCRIPTION columns - unnecessary for analysis
  if (remove_desc){
    desc_cols <- grep("(?i)_desc", names(data), value = TRUE, perl = TRUE)

    # data_selected <- data[, setdiff(names(data), desc_cols), with = FALSE]
    data[, (desc_cols) := NULL]
  }

  # remove the extra administrative columns
  extra_cols <- grep("(?i)_data_", names(data), value = TRUE, perl = TRUE)
  data[, (extra_cols) := NULL]

  # DATE columns - in excel date format
  # start, extension, end
  date_cols <- grep("(?i)_date|(?i)_skill_gain|(?i)_start|(?i)_end_|(?i)_extension",
                    names(data), value = TRUE,
                    perl = TRUE)

  data[, (date_cols) := lapply(.SD, handle_excel_date), .SDcols = date_cols]

  # PROVIDER and PURCHASE columns - values 0 or 1
  prov_purch_cols <- grep("(?i)_provide|(?i)_purchase", names(data),
                          value = TRUE, perl = TRUE)

  data[, (prov_purch_cols) := lapply(.SD, handle_blanks),
       .SDcols = prov_purch_cols]

  # AMT and TITLE columns (TITLEI columns are funds expended for different
  #   services)
  # WAGE columns
  amt_cols <- grep("(?i)_amt|(?i)_title|(?i)_wage|(?i)_amount|(?i)_amnt",
                   names(data), value = TRUE, perl = TRUE)

  # VENDOR columns - should be values 1,2,3,4 or blank
  vendor_cols <- grep("(?i)_vendor", names(data), value = TRUE, perl = TRUE)

  # HOURS columns
  hours_cols <- grep("(?i)_hour|(?i)_hr", names(data),
                     value = TRUE, perl = TRUE)
  # COMP columns
  comp_cols <- grep("(?i)_comp_", names(data), value = TRUE, perl = TRUE)
  comp_cols <- comp_cols[!grepl("provide|amt|desc|date", comp_cols,
                                ignore.case = TRUE)]
  # come back to later

  # AGE column
  # Rename the column and convert to numeric
  setnames(data, "Age at Application", "Age_at_Application")
  # data[, Age_at_Application := as.numeric(Age_at_Application)]

  # DEMOGRAPHIC columns
  # e10-e16, e42, e49, e54-e60, e62-e73,
  demographic_cols <- grep("^(E1[0-6]_|(E42|E49|E5[4-9]|E6[2-7]|E7[0-3])_)",
                           names(data), value = TRUE, perl = TRUE)
  # E60_Youth_911 - 0, 1, NULL
  # E68_Plan_English_Learner_911 - 0, 1, NULL (should be 9)
  # E69_Plan_Skills_Deficient_911 - 0, 1, 9
  # E392_Q2_Q4_Employer_Match_911 - 0, 1, NULL (should just be 0 or 1)
  # E400_Secondary_Equivalent_Enrollment_911 - 0, 1, NULL (should just be 0 or 1)

  # more_demo_cols <-

  data[, (demographic_cols) := lapply(.SD, function(x) {
    handle_nines(x, unidentified_to_0)
  }), .SDcols = demographic_cols]

  # SEX column
  # E9_Gender_911 - 1,2,9, NULL - factor

  # CODE columns
  # different kinds of values--zip code, agency code, etc. -- all factors
  code_cols <- grep("(?i)_code_", names(data), value = TRUE, perl = TRUE)

  # EMPLOYMENT columns
  # 1-4, 9, 0 --factors--potentially ordinal...
  employ_cols <- grep("(?i)_employ", names(data), value = TRUE, perl = TRUE)



  # HANDLE DISABILITY columns
  # call separate_disability function
  data <- separate_disability(data)


  # HANDLE DUPLICATES IN PARTICIPANTS
  # figure out if I want it structured differently for a single quarter dataset

  # Convert variables to correct types
  more_numerics <- grep("(?i)_age_|(?i)_year|(?i)_yr_|(?i)_quarter|(?i)_qt_",
                        names(data), value = TRUE, perl = TRUE)
  numeric_cols <- unique(c(amt_cols, hours_cols, more_numerics))
  data[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

  ## Convert all other columns to factors
  factor_cols <- setdiff(names(data), c(numeric_cols, date_cols))
  data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]



  # return the cleaned dataset
  return(data)
}
