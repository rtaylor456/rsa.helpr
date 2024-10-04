create_metadata <- function(data, includes_scores = TRUE) {
  # Convert data to data.table if it's not already
  data <- as.data.table(data)

  # Set key for the data
  setkey(data, Participant_ID)

  ## INITIAL FACTOR CONVERSION
  data[, (names(data)) := lapply(.SD, function(col) {
    if (any(names(col) %in% c("Provider", grep("Has_Multiple_Scores", names(data), value = TRUE)))) {
      as.factor(col)
    } else {
      col
    }
  }), .SDcols = names(data)]

  ## CREATE NEW VARIABLES
  # Create enrollment length variable
  data[, Overall_Quarter := ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)]

  # Calculate Min and Max Overall Quarter using keyed groups
  data[, `:=`(Min_Overall_Quarter = min(Overall_Quarter, na.rm = TRUE),
              Max_Overall_Quarter = max(Overall_Quarter, na.rm = TRUE)), by = Participant_ID]

  # Compute Enrollment Length
  data[, Enroll_Length := Max_Overall_Quarter - Min_Overall_Quarter + 1]

  # Group enrollment length
  data[, Enroll_Length_Grp := fifelse(Enroll_Length < 5, "<5",
                                      fifelse(Enroll_Length >= 5 & Enroll_Length < 11, "5-10", "11+"))]

  # Convert Enroll_Length_Grp to an ordered factor
  data[, Enroll_Length_Grp := factor(Enroll_Length_Grp, levels = c("<5", "5-10", "11+"), ordered = TRUE)]

  # Create variables that count the number of years and quarters
  data[, `:=`(Total_Years = uniqueN(E1_Year_911),
              Total_Quarters = uniqueN(E2_Quarter_911)), by = Participant_ID]

  # Remove the year and quarter columns
  data[, c("E1_Year_911", "E2_Quarter_911") := NULL]

  ## NUMERIC VARIABLES
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  integer_cols <- names(data)[sapply(data, is.integer)]
  double_cols <- setdiff(numeric_cols, integer_cols)

  # Handle different types of numeric variables
  data[, (integer_cols) := lapply(.SD, function(x) as.integer(median(x, na.rm = TRUE))),
       .SDcols = integer_cols]

  data[, (double_cols) := lapply(.SD, function(x) median(x, na.rm = TRUE)),
       .SDcols = double_cols]

  ## DATE VARIABLES
  date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date"))]

  data[, (date_cols) := lapply(.SD, function(x) as.Date(ifelse(all(is.na(unique(x))), NA, max(x, na.rm = TRUE)))),
       .SDcols = date_cols]

  ## FACTOR VARIABLES
  factor_cols <- names(data)[sapply(data, is.factor)]
  factor_cols <- setdiff(factor_cols, "Participant_ID")

  get_mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }

  data[, (factor_cols) := lapply(.SD, get_mode), .SDcols = factor_cols]

  if (includes_scores == TRUE) {
    ## CREATE NEW VARIABLES--new MEDIANS and DIFFERENCES
    difference_cols <- grep("^Difference_", names(data), value = TRUE)
    time_cols <- grep("^Time_Passed_Days", names(data), value = TRUE)

    # Calculate Differences_Available
    data[, Differences_Available := rowSums(!is.na(.SD)), .SDcols = difference_cols]

    # Calculate Median_Difference_Score
    if (length(difference_cols) > 0) {
      data[, Median_Difference_Score := median(unlist(.SD), na.rm = TRUE), .SDcols = difference_cols]
    } else {
      data[, Median_Difference_Score := NA_real_]
    }

    # Calculate Median_Time_Passed_Days
    if (length(time_cols) > 0) {
      data[, Median_Time_Passed_Days := median(unlist(.SD), na.rm = TRUE), .SDcols = time_cols]
    } else {
      data[, Median_Time_Passed_Days := NA_real_]
    }
  }

  ## CONDENSE! CREATE METADATA
  # Summarise to condense rows, keeping one row per participant
  metadata <- data[, lapply(.SD, first), by = Participant_ID]

  return(metadata)
}
