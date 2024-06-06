#' Clean the data! Clean and reorganize a PYexit RSA-911 dataset.
#'
#' This function is a one-stop for cleaning and restructuring an RSA-911 dataset
#'   in order to prepare it for analysis and visualization. Processed include
#'   tailored NA imputation, variable type conversion, and reorganization of
#'   variables.
#'
#' @param data An RSA-911 dataset.
#' @param na_check TRUE or FALSE, defaults to TRUE. If TRUE, runs analyze_nas
#'    function on dataset, after cleaning, to output information on remaining NA
#'    values.
#' @param na_file TRUE or FALSE, defaults to FALSE. If TRUE, this will write a
#'     csv file with the table of NA counts and proportions. If set to FALSE, it
#'     will print the table but not generate a csv file.
#' @param full_table_print TRUE or FALSE, defaults to FALSE. If FALSE, the NAs
#'     table will only print the head of the resulting table. If TRUE, the NAs
#'     table will print the entire table of NAs.
#' @param unidentified_to_0 TRUE or FALSE, defaults to FALSE. If TRUE, converts
#'     values of 9 to 0 within variables with possible values of 0, 1, 9 (where
#'     9 represents "did not identify").
#' @param convert_sex TRUE or FALSE, defaults to FALSE. If TRUE, converts
#'     variable Sex' values of 2 to values of 0, providing a more traditional
#'     binary variable format.
#' @param remove_strictly_na TRUE or FALSE, defaults to FALSE. If TRUE,
#'     identifies and removes variables with exclusively NA values after the
#'     data have been cleaned.
#'
#' @returns The cleaned dataset (additionally, a table of NA information if
#'     na_check param is set to TRUE. See analyze_nas documentation for info.)
#' @export
#' @import data.table
#' @import tidyverse
py_clean <- function(data, na_check = TRUE, na_file = FALSE,
                  full_table_print = FALSE,
                  unidentified_to_0 = FALSE,
                  convert_sex = FALSE,
                  remove_strictly_na = FALSE) {

  if (!na_check && (na_file || full_table_print)) {
    stop(paste("na_check must be set equal to TRUE in order to use",
               "na_file and full_table_print"))
  }

  # Ensure data is a data.table
  setDT(data)

  # Lowercase column names
  setnames(data, tolower(names(data)))

  # Helper function to replace NA with specified value using data.table syntax
  replace_na <- function(cols, value) {
    for (col in cols) {
      column_type <- mode(data[[col]])
      if (column_type == "numeric" || column_type == "integer") {
        typed_value <- as.numeric(value)
      } else if (column_type == "logical") {
        typed_value <- as.logical(value)
      } else {
        typed_value <- as.character(value)
      }
      data[, (col) := fifelse(is.na(get(col)), typed_value, get(col))]
    }
  }


  # Replace NAs in specified columns
  # AGENCY columns
  agencystaff_cols <- grep("agencystaff", names(data), value = TRUE)
  agencypurchase_cols <- grep("agencypurchase", names(data), value = TRUE)
  replace_na(c(agencystaff_cols, agencypurchase_cols), 0)

  # PROVIDER (without "type") columns
  compserviceprovider_cols <- grep("compserviceprovider(?!type)", names(data),
                                   value = TRUE, perl = TRUE)
  purchaseprovider_cols <- grep("purchaseprovider", names(data),
                                value = TRUE)
  replace_na(c(compserviceprovider_cols, purchaseprovider_cols), 0)

  # PROGRAMYEAR columns
  programyear_cols <- grep("programyear", names(data), value = TRUE)
  data[, (programyear_cols) := lapply(.SD, handle_year),
       .SDcols = programyear_cols]

  # DATE columns
  date_cols <- grep("date|eligibilityext|compdisenrollmsg", names(data),
                    value = TRUE)

  # SEX columns
  sex_cols <- grep("sex", names(data), value = TRUE)
  # oosexitdate will be mishandled without the following line of code:
  sex_cols <- setdiff(sex_cols, date_cols)
  data[, (sex_cols) := lapply(.SD, function(x) handle_sex(x, convert_sex)),
       .SDcols = sex_cols]

  # DEMOGRAPHIC columns
  demographic_cols <- c("amerindian", "asian", "black", "hawaiian",
                        "white", "hispanic", "veteran", "disability", "adult",
                        "adulted", "dislocatedworker", "jobcorps",
                        "wpempservice", "youth", "longtermunemp", "exhausttanf",
                        "fostercareyouth", "homelessorrunaway",
                        "exoffenderstatus", "lowincomestatus", "englishlearner",
                        "basicskillsdeficient",
                        "culturalbarriers", "singleparent", "dishomemaker")
  add_demographic_cols <- c("insecondaryed", "completedsomepostseced",
                            "enrolledinsecequiv")
  all_demographic_cols <- c(demographic_cols, add_demographic_cols)

  data[, (all_demographic_cols) := lapply(.SD, function(x) {
    handle_nines(x, unidentified_to_0)
  }),
  .SDcols = all_demographic_cols]


  # PURCHASE TYPE columns -- already handled in purchase provider columns code
  # purchase_type_cols <- grep("purchase.*type", names(data),
  #                                  value = TRUE, perl = TRUE)
  # replace_na(purchase_type_cols, 0)

  # COMPSERVICE TYPE columns and
  # comp_type_cols <- grep("comp.*type", names(data), value = TRUE, perl = TRUE)
  # # replace blanks " " and "NULL" values to "0"s --leave as characters because
  # #   we have values with numbers separated by ";"
  # data[, (comp_type_cols) := lapply(.SD, handle_blanks),
  #      .SDcols = comp_type_cols]
  # # if we don't care about the specific values of providers, we will just
  # #   convert the variable values to 1 if there is a value other than "0"
  # data[, (comp_type_cols) := lapply(.SD, function(x) ifelse(x != "0", "1", x)),
  #      .SDcols = comp_type_cols]

  # remaining TYPE columns
  # Extract column names containing "type" but not "purchase" or "comp"
  other_type_cols <- grep("type", names(data), value = TRUE, perl = TRUE)
  other_type_cols <- other_type_cols[!grepl("purchase|comp", other_type_cols)]
  replace_na(other_type_cols, 0)


  # SPECIAL CHARACTER columns
  comp_type_cols <- grep("comp.*type", names(data), value = TRUE, perl = TRUE)
  special_cols <- c(comp_type_cols, "appmonthlypubsup", "exitmonthlypubsup",
                    "appmedinscov", "exitmedinscov", "youthbuild")
  # replace blanks " " and "NULL" values to "0"s --leave as characters because
  #   we have values with numbers separated by ";"
  data[, (special_cols) := lapply(.SD, handle_blanks),
       .SDcols = special_cols]
  # if we don't care about the specific values, we will just
  #   convert the variable values to 1 if there is a value other than "0"
  data[, (special_cols) := lapply(.SD, function(x) ifelse(x != "0", "1", x)),
       .SDcols = special_cols]

  # CONVERT TO CORRECT VARIABLE TYPES:
  # convert everything to factors, except the numeric and date variables
  # variables with "date" -- already extracted: date_cols
  # variables with "expend"
  # variables with "age_"
  # variables with "wage"
  # variables with "hours"
  # specific variables:
  #  ProgramYear

  # Numeric columns
  numeric_cols <- grep("expend|wage|hours|age_|year", names(data),
                       value = TRUE)
  data[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]


  # additional numeric columns cleaning


  # DATE columns conversion
  # date_cols <- grep("date|eligibilityext|compdisenrollmsg", names(data),
  #                   value = TRUE)
  data[, (date_cols) := lapply(.SD, handle_date), .SDcols = date_cols]

  # Convert all other columns to factors
  factor_cols <- setdiff(names(data), c(numeric_cols, date_cols))
  data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]


  # Remove strictly NA (or strictly 0) columns if specified
  if (remove_strictly_na) {
    # na_summary <- sapply(data, function(x) sum(is.na(x)))
    # cols_to_remove <- names(na_summary)[na_summary == nrow(data)]

    # na_variables <- names(data)[sapply(data, function(x) all(is.na(x)))]
    # zero_variables <- names(data)[sapply(data, function(x) all(x == 0))]

    cols_to_remove <- names(data)[sapply(data, function(x) {
      all(is.numeric(x) & x == 0) || all(is.na(x))
    })]

    if (length(cols_to_remove) > 0) {
      print(cols_to_remove)
      data[, (cols_to_remove) := NULL]  # Correct use of `:=` to remove columns
      cat("Columns where all values are NAs have been removed from the data.\n")
    }
  }

  # NA Check and analysis
  if (na_check) {
    analyze_nas(data, na_file = na_file, full_table_print = full_table_print)
  }

  cat("Dataset has been cleaned\n")
  return(data)
}


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
  comp_cols <- grep("(?i)_comp", names(data), value = TRUE, perl = TRUE)
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
