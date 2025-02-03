library(data.table)

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
  data <- as.data.table(data)

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
                          fifelse(Enroll_Length >= 5 & Enroll_Length < 11,
                                  "5-10",
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

  # original code
  # data[, (numeric_cols) := lapply(.SD, median, na.rm = TRUE),
  #      .SDcols = numeric_cols, by = Participant_ID]

  # to handle different types of numeric variables
  data[, (integer_cols) := lapply(.SD, function(x) as.integer(median(x,
                                                           na.rm = TRUE))),
       .SDcols = integer_cols, by = Participant_ID]

  data[, (double_cols) := lapply(.SD, function(x) median(x, na.rm = TRUE)),
       .SDcols = double_cols, by = Participant_ID]


  ## DATE VARIABLES
  # Handle date variables
  # date_cols <- names(data)[sapply(data, lubridate::is.Date)]
  date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date"))]

  data[, (date_cols) := lapply(.SD,
                               function(x) as.Date(ifelse(all(is.na(unique(x))),
                                                          NA,
                                                        max(x, na.rm = TRUE)))),
       .SDcols = date_cols,
       by = Participant_ID]


  ## FACTOR VARIABLES
  factor_cols <- names(data)[sapply(data, is.factor)]

  get_mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }
  data[, (factor_cols) := lapply(.SD, get_mode), .SDcols = factor_cols,
       by = Participant_ID]

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

  # ## CREATE NEW VARIABLES--new MEDIANS and DIFFERENCES
  # # Calculate differences and medians
  # difference_cols <- grep("^Difference_", names(data), value = TRUE)
  # time_cols <- grep("^Time_Passed_Days", names(data), value = TRUE)
  #
  # # Calculate Differences_Available
  # data[, Differences_Available := rowSums(!is.na(.SD)),
  #      .SDcols = difference_cols, by = Participant_ID]
  #
  # # Calculate Median_Difference_Score
  # if (length(difference_cols) > 0) {
  #   data[, Median_Difference_Score := median(unlist(.SD), na.rm = TRUE),
  #        .SDcols = difference_cols, by = Participant_ID]
  # } else {
  #   data[, Median_Difference_Score := NA_real_]
  # }
  #
  # # Calculate Median_Time_Passed_Days
  # if (length(time_cols) > 0) {
  #   data[, Median_Time_Passed_Days := median(unlist(.SD), na.rm = TRUE),
  #        .SDcols = time_cols, by = Participant_ID]
  # } else {
  #   data[, Median_Time_Passed_Days := NA_real_]
  # }


  ## CONDENSE! CREATE METADATA
  # Summarise to condense rows, keeping one row per participant
  metadata <- data[, lapply(.SD, first), by = Participant_ID]

  return(metadata)
}

