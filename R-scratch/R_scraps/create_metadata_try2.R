#' Create Metadata
#'
#' This function aggregates a merged or RSA-911 dataset to contain only one row
#'   per unique participant, to make analysis and modeling more intuitive. The
#'   aggregation process includes condensing rows by taking medians for numeric
#'   variables, keeping most common values for character and factor variables,
#'   and keeping most recent values for date-related variables.
#'
#' @param data The merged or RSA-911 dataset.
#' @param includes_scores TRUE or FALSE. Defaults to TRUE, when TRUE, metadata
#'   creation includes process for scores data variables--inputted data frame is
#'   treated as a merged dataset. When FALSE, metadata process only includes
#'   steps relevant for an RSA-911 dataset.
#'
#' @returns A data frame with one row per participant.
#'
#' @export
#' @import data.table

create_metadata <- function(data, includes_scores = TRUE) {
  # Convert data to data.table if it's not already
  setDT(data)

  ## INITIAL FACTOR CONVERSION
  data[, lapply(.SD, function(col) {
    if (any(names(col) %in% c("Provider", grep("Has_Multiple_Scores",
                                               names(data), value = TRUE)))) {
      as.factor(col)
    } else {
      col
    }
  }), .SDcols = names(data)]


  ## CREATE NEW VARIABLES
  # Create enrollment length variable
  data[, Overall_Quarter := ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)]

  data[, `:=`(Min_Overall_Quarter = min(Overall_Quarter),
              Max_Overall_Quarter = max(Overall_Quarter)),
       by = Participant_ID]

  # Compute Enroll Length
  data[, Enroll_Length := Max_Overall_Quarter - Min_Overall_Quarter + 1]

  # group enrollment length
  data[, Enroll_Length_Grp := fifelse(Enroll_Length < 5, "<5",
                                      fifelse(Enroll_Length >= 5 &
                                                Enroll_Length < 11, "5-10",
                                              "11+"))]

  # Convert Age_Group to an ordered factor
  data[, Enroll_Length_Grp := factor(Enroll_Length_Grp,
                                     levels = c("<5", "5-10", "11+"),
                                     ordered = TRUE)]


  # Create variables that count the number of years and quarters
  data[, `:=`(Total_Years = uniqueN(E1_Year_911),
              Total_Quarters = uniqueN(E2_Quarter_911)),
       by = Participant_ID]


  # Remove the year and quarter columns
  data[, c("E1_Year_911", "E2_Quarter_911") := NULL]


  ## NUMERIC VARIABLES
  # Convert numeric variables to medians
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # to handle different types of numeric variables
  integer_cols <- names(data)[sapply(data, is.integer)]
  double_cols <- setdiff(numeric_cols, integer_cols)


  # to handle different types of numeric variables
  data[, (integer_cols) := lapply(.SD, function(x) {
    as.integer(median(x, na.rm = TRUE))
  }), .SDcols = integer_cols, by = Participant_ID]

  data[, (double_cols) := lapply(.SD, function(x) median(x, na.rm = TRUE)),
       .SDcols = double_cols, by = Participant_ID]


  # # Ensure Application Date column exists and is of Date type
  # app_date_col <- grep("(?i)_app.*?(date)(?!.*(?i)_desc)", names(data),
  #                      value = TRUE, perl = TRUE)
  #
  # # Identify numeric columns (excluding Participant_ID)
  # numeric_cols <- names(data)[sapply(data, is.numeric)]
  # numeric_cols <- setdiff(numeric_cols, "Participant_ID")
  #
  # # Select the row with the most recent non-missing value for each numeric column
  # # data <- data[, lapply(.SD, function(col) {
  # #   idx <- which(!is.na(col))  # Find indices of non-missing values
  # #   if (length(idx) == 0) return(as(NA, class(col)))  # Ensure correct type for NA
  # #   col[idx[which.max(get(app_date_col)[idx])]]  # Get value from the most recent date
  # # }), by = Participant_ID, .SDcols = c(app_date_col, numeric_cols)]
  #
  # # Update numeric columns in place without removing rows
  # data[, (numeric_cols) := lapply(.SD, function(col) {
  #   idx <- which(!is.na(col))  # Find indices of non-missing values
  #   if (length(idx) == 0) return(col)  # Keep original column if all values are NA
  #   col[which.max(get(app_date_col))]  # Assign most recent non-missing value
  # }), by = Participant_ID, .SDcols = numeric_cols]


  ## DATE VARIABLES
  # Handle date variables
  date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date"))]

  data[, (date_cols) := lapply(.SD,
                               function(x) {
                                 as.Date(ifelse(all(is.na(unique(x))), NA,
                                                max(x, na.rm = TRUE)))
                               }),
  .SDcols = date_cols, by = Participant_ID]


  ## FACTOR VARIABLES
  factor_cols <- names(data)[sapply(data, is.factor)]

  get_mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }

  # data[, (factor_cols) := lapply(.SD, get_mode), .SDcols = factor_cols,
  #      by = Participant_ID]


  ### NEW CODE ####
  ## BINARY columns
  prov_purch_cols <- grep("((?i)_provide|(?i)_purchase)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

  race_cols <- grep(paste0("(?i)(_indian|_asian|_black|_hawaiian|_islander|",
                           "_white|hispanic)(?!.*(?i)_desc)"),
                    names(data),
                    value = TRUE, perl = TRUE)

  veteran_col <- grep(paste0("(?i)veteran(?!.*(_desc|description|_amt|amount|",
                             "amnt|vendor|title|comp))"),
                      names(data), value = TRUE, perl = TRUE)

  has_disability_col <- grep(paste0("(?i)(has_disability)(?!.*(_desc|",
                                    "description|_amt|amount|amnt|vendor|title",
                                    "|comp|hours|date|ext|wage))"),
                             names(data), value = TRUE, perl = TRUE)

  q1_q1_employ_col <- grep(paste0("(?i)(employer_match|q1_q2_match|",
                                  "q1_q2_employ)(?!.*(_desc|description|_amt|",
                                  "amount|amnt|vendor|title|comp|hours|date|",
                                  "ext|wage))"),
                           names(data), value = TRUE, perl = TRUE)

  adult_cols <- grep(paste0("(?i)adult(?!.*(_desc|description|_amt|amount|amnt",
                            "|vendor|title|comp|hours|date|ext|wage))"),
                     names(data), value = TRUE, perl = TRUE)

  service_work_cols <- grep(paste0("(?i)(dislocated_worker|job_corps|voc_rehab",
                                   "|wagner_peyser|youth|equivalent|se_goal)",
                                   "(?!.*(_desc|description|_amt|amount|amnt|",
                                   "vendor|title|comp|hours|date|ext|wage))"),
                            names(data), value = TRUE, perl = TRUE)

  plan_cols <- grep(paste0("(?i)plan_(?!.*(_desc|description|_amt|amount|amnt|",
                           "vendor|title|comp|hours|date|ext|wage|status|occ|",
                           "grade|farm))"),
                    names(data), value = TRUE, perl = TRUE)

  demographic_cols <- c(race_cols, veteran_col, has_disability_col,
                        q1_q1_employ_col, adult_cols, service_work_cols,
                        plan_cols)

  binary_cols <- c(prov_purch_cols, demographic_cols)

  # Define the columns for which the presence of '1' should be prioritized

  # Function to prioritize 1 if present, otherwise use mode
  get_mode_with_binary <- function(x) {
    if (1 %in% x) return(1)
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }

  # Ensure factor_cols and binary_cols do not overlap
  binary_cols <- intersect(binary_cols, names(data))  # Only keep existing columns
  factor_cols <- setdiff(factor_cols, binary_cols)    # Remove binary cols from factor cols

  # Apply mode to factor columns
  if (length(factor_cols) > 0) {
    data[, (factor_cols) := lapply(.SD, get_mode),
         .SDcols = factor_cols, by = Participant_ID]
  }

  # Apply binary logic
  if (length(binary_cols) > 0) {
    data[, (binary_cols) := lapply(.SD, get_mode_with_binary),
         .SDcols = binary_cols, by = Participant_ID]
  }


  ### END OF NEW CODE ###



  if (includes_scores == TRUE) {

    ## CREATE NEW VARIABLES--new MEDIANS and DIFFERENCES
    # Calculate differences and medians
    difference_cols <- grep("^Difference_", names(data), value = TRUE)
    time_cols <- grep("^Time_Passed_Days", names(data), value = TRUE)

    # Calculate Differences_Available
    data[, Differences_Available := rowSums(!is.na(.SD)),
         .SDcols = difference_cols, by = Participant_ID]

    # Calculate Median_Difference_Score
    if (length(difference_cols) > 0) {
      data[, Median_Difference_Score := median(unlist(.SD), na.rm = TRUE),
           .SDcols = difference_cols, by = Participant_ID]
    } else {
      data[, Median_Difference_Score := NA_real_]
    }

    # Calculate Median_Time_Passed_Days
    if (length(time_cols) > 0) {
      data[, Median_Time_Passed_Days := median(unlist(.SD), na.rm = TRUE),
           .SDcols = time_cols, by = Participant_ID]
    } else {
      data[, Median_Time_Passed_Days := NA_real_]
    }

  }

  ## CONDENSE! CREATE METADATA
  # Summarise to condense rows, keeping one row per participant
  metadata <- data[, lapply(.SD, first), by = Participant_ID]

  return(metadata)
}
