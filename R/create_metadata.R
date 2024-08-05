library(lubridate)

# create a function to get the most common values (modes) for my factor
#   variables
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

create_metadata <- function(data) {
  metadata <- data |>
    mutate(across(c(Provider, Has_Multiple_Scores), ~ as.factor(.))) |>
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

create_metadata <- function(data) {
  # Convert data to data.table if it's not already
  data <- as.data.table(data)

  # Ensure that factors and dates are treated correctly
  data[, c("Provider", "Has_Multiple_Scores") := lapply(.SD, as.factor), .SDcols = c("Provider", "Has_Multiple_Scores")]

  # Create enrollment length variable
  data[, Overall_Quarter := ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)]
  data[, `:=`(Min_Overall_Quarter = min(Overall_Quarter),
              Max_Overall_Quarter = max(Overall_Quarter),
              Enroll_Length = Max_Overall_Quarter - Min_Overall_Quarter + 1),
       by = Participant_ID]

  # Create variables that count the number of years and quarters
  data[, `:=`(Total_Years = uniqueN(E1_Year_911),
              Total_Quarters = uniqueN(E2_Quarter_911)),
       by = Participant_ID]

  # Remove the year and quarter columns
  data[, c("E1_Year_911", "E2_Quarter_911") := NULL]

  # Convert numeric variables to medians
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  data[, (numeric_cols) := lapply(.SD, median, na.rm = TRUE), .SDcols = numeric_cols]

  # Handle date variables
  date_cols <- names(data)[sapply(data, lubridate::is.Date)]
  data[, (date_cols) := lapply(.SD, function(x) as.Date(ifelse(all(is.na(unique(x))), NA, max(x, na.rm = TRUE)))), .SDcols = date_cols]

  # Handle factor variables
  factor_cols <- names(data)[sapply(data, is.factor)]
  get_mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }
  data[, (factor_cols) := lapply(.SD, get_mode), .SDcols = factor_cols]

  # Calculate differences and medians
  difference_cols <- grep("^Difference_", names(data), value = TRUE)
  time_cols <- grep("^Time_Passed_Days", names(data), value = TRUE)

  data[, `:=`(Differences_Available = sum(!is.na(.SD)),
              Median_Difference_Score = median(.SD, na.rm = TRUE),
              Median_Time_Passed_Days = median(.SD, na.rm = TRUE)),
       .SDcols = c(difference_cols, time_cols), by = Participant_ID]

  # Summarise to condense rows, keeping one row per participant
  metadata <- data[, lapply(.SD, first), by = Participant_ID]

  return(metadata)
}




# metadata <- data_merged |>
#   # mutate(Provider = as.factor(Provider)) |>
#   mutate(across(c(Provider, Has_Multiple_Scores), ~ as.factor(.))) |>
#   group_by(Participant_ID) |>
#   # create enrollment length variable
#   mutate(Overall_Quarter = ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)) |>
#   mutate(Min_Overall_Quarter = min(Overall_Quarter),
#          Max_Overall_Quarter = max(Overall_Quarter)) |>
#   mutate(Enroll_Length = Max_Overall_Quarter -
#            Min_Overall_Quarter + 1) |>
#   # create variables that count the number of years and quarters and then remove
#   #   the year and quarter columns so we can condense the data
#   arrange(Participant_ID, E1_Year_911, E2_Quarter_911) |>
#   mutate(
#     Total_Years = n_distinct(E1_Year_911),
#     Total_Quarters = n_distinct(E2_Quarter_911)
#   ) |>
#   select(-c(E1_Year_911, E2_Quarter_911)) |>
#   # convert the rest of numeric variables to medians
#   mutate(across(where(is.numeric), ~ median(., na.rm = TRUE))) |>
#
#   # handle date variables
#   mutate(across(where(lubridate::is.Date),
#                 ~ as.Date(ifelse(all(is.na(unique(.))), NA,
#                                  max(., na.rm = TRUE)))
#   )) |>
#   # handle factor variables--keep only the most common values for each
#   #   participant
#   mutate(across(where(is.factor), ~ as.factor(get_mode(.)))) |>
#
#   # calculate
#   rowwise() |>
#   mutate(
#     Differences_Available = sum(!is.na(c_across(starts_with("Difference_") ))),
#     Median_Difference_Score = median(c_across(starts_with("Difference_")),
#                                      na.rm = TRUE),
#     Median_Time_Passed_Days = median(c_across(starts_with("Time_Passed_Days")),
#                                      na.rm = TRUE)
#   ) |>
#   ungroup() |>
#
#   # Summarise to condense rows, keeping one row per participant
#   group_by(Participant_ID) |>
#   summarise(across(everything(), first)) |>
#
#   ungroup() |>
#   # remove columns where all values are NA
#   select(where(~ !all(is.na(.)))) |>
#   # apply my handmade function to handle disability columns
#   separate_disability()
#
#
# nrow(metadata) # 329
# length(unique(metadata$Participant_ID)) # 329
#
# View(metadata)
# names(metadata)
#
# # scores2 <- read.csv("data-raw/trt_data_4_8_2024.csv")
#
#
