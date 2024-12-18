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
