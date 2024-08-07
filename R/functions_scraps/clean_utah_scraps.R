## FASTEST -- currently using this as of 8/6/24
# using...
# if (aggregate) {
#   data <- data[!is.na(E7_Application_Date_911)]
#   # Order the data by Participant_ID, E1_Year_911, E2_Quarter_911, and E7_Application_Date_911
#   setorder(data, Participant_ID, E1_Year_911, E2_Quarter_911, -E7_Application_Date_911)
#
#   # Create a helper column to identify the first occurrence within each group
#   data[, Occurrences_Per_Quarter := .N, by = .(Participant_ID, E1_Year_911, E2_Quarter_911)]
#   data <- data[, .SD[1], by = .(Participant_ID, E1_Year_911, E2_Quarter_911)]
#
#   # Sort by year and quarter
#   setorder(data, E1_Year_911, E2_Quarter_911)
# }

start <- Sys.time()
try_clean <- clean_utah(try, aggregate = TRUE)
end <- Sys.time()
time <- end - start
time # Time difference of 42.31769 secs


## SLOWEST
# using...
# if (aggregate == TRUE) {
#   data <- data[!is.na(E7_Application_Date_911)]
#   # Group by Participant_ID, E1_Year_911, and E2_Quarter_911
#   data <- data[, {
#     Occurrences_Per_Quarter <- .N
#     most_recent <- .SD[which.max(E7_Application_Date_911)]
#     most_recent[, Occurrences_Per_Quarter := Occurrences_Per_Quarter]
#     most_recent
#   }, by = .(Participant_ID, E1_Year_911, E2_Quarter_911)]
# }
start <- Sys.time()
try_clean <- clean_utah(try, aggregate = TRUE)
end <- Sys.time()
time <- end - start
time # Time difference of 2.64243 mins


# using...
# if (aggregate == TRUE){
#   data <- data |>
#     filter(!is.na(E7_Application_Date_911)) |>
#     group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
#     mutate(Occurrences_Per_Quarter = n()) |>
#     arrange(E7_Application_Date_911) |>
#     slice(1) |>
#     ungroup()
# }
start <- Sys.time()
try_clean <- clean_utah(try, aggregate = TRUE)
end <- Sys.time()
time <- end - start
time # Time difference of 58.10883 secs


# tidyverse version for removing COLUMNS WITH STRICTLY NAs
# if (remove_strictly_na == TRUE){
#   data <- data |>
#     select(where(~ !all(is.na(.))))
# }


