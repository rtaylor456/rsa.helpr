library(lubridate)

# create a function to get the most common values (modes) for my factor
#   variables
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

create_metadata <- function(data) {
  metadata <- data |>
    mutate(across(c(Provider, contains("Has_Multiple_Scores")),
                  ~ as.factor(.))) |>
    group_by(Participant_ID) |>
    # create enrollment length variable
    mutate(Overall_Quarter = ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)) |>
    mutate(Min_Overall_Quarter = min(Overall_Quarter),
           Max_Overall_Quarter = max(Overall_Quarter)) |>
    mutate(Enroll_Length = Max_Overall_Quarter -
             Min_Overall_Quarter + 1) |>
    # create variables that count the number of years and quarters and then remove
    #   the year and quarter columns so we can condense the data
    arrange(Participant_ID, E1_Year_911, E2_Quarter_911) |>
    mutate(
      Total_Years = n_distinct(E1_Year_911),
      Total_Quarters = n_distinct(E2_Quarter_911)
    ) |>
    select(-c(E1_Year_911, E2_Quarter_911)) |>
    # convert the rest of numeric variables to medians
    mutate(across(where(is.numeric), ~ median(., na.rm = TRUE))) |>

    # handle date variables
    mutate(across(where(lubridate::is.Date),
                  ~ as.Date(ifelse(all(is.na(unique(.))), NA,
                                   max(., na.rm = TRUE)))
    )) |>
    # handle factor variables--keep only the most common values for each
    #   participant
    mutate(across(where(is.factor), ~ as.factor(get_mode(.)))) |>

    # calculate
    rowwise() |>
    mutate(
      Differences_Available = sum(!is.na(c_across(starts_with("Difference_") ))),
      Median_Difference_Score = median(c_across(starts_with("Difference_")),
                                       na.rm = TRUE),
      Median_Time_Passed_Days = median(c_across(starts_with("Time_Passed_Days")),
                                       na.rm = TRUE)
    ) |>
    ungroup() |>

    # Summarise to condense rows, keeping one row per participant
    group_by(Participant_ID) |>
    summarise(across(everything(), first)) |>

    ungroup()

  return(metadata)
}


## USING DATA.TABLE

library(data.table)
library(lubridate)

create_metadata2 <- function(data) {
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
  data[, (integer_cols) := lapply(.SD, function(x) as.integer(median(x, na.rm = TRUE))),
       .SDcols = integer_cols, by = Participant_ID]

  data[, (double_cols) := lapply(.SD, function(x) median(x, na.rm = TRUE)),
       .SDcols = double_cols, by = Participant_ID]


  ## DATE VARIABLES
  # Handle date variables
  date_cols <- names(data)[sapply(data, lubridate::is.Date)]
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

  ## CONDENSE! CREATE METADATA
  # Summarise to condense rows, keeping one row per participant
  metadata <- data[, lapply(.SD, first), by = Participant_ID]

  return(metadata)
}

