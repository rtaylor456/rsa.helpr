clean_scores2 <- function(data, state_filter = NULL, aggregate = TRUE) {
  # Convert to data.table
  setDT(data)

  # Identify dynamic column names
  participant <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
                      names(data), value = TRUE, perl = TRUE)
  state <- grep("(?i)^(?=.*state)|(?=.*\\bst\\b)(?!.*\\bst\\B)",
                    names(data), value = TRUE, perl = TRUE)
  provider <- grep("(?i)provider", names(data), value = TRUE, perl = TRUE)
  mode <- grep("(?i)mode", names(data), value = TRUE, perl = TRUE)
  pre_post <- grep("(?i)^(?=.*pre)(?=.*post)",
                   names(data), value = TRUE, perl = TRUE)

  # Rename columns for consistency
  names(data)[names(data) %in% participant] <- "Participant_ID"
  names(data)[names(data) %in% state] <- "State"
  names(data)[names(data) %in% provider] <- "Provider"
  names(data)[names(data) %in% mode] <- "Mode"
  names(data)[names(data) %in% pre_post] <- "Pre_Post"

  # Order data by participant
  data <- data[order(Participant_ID)]

  # Apply state filter, if provided
  if (!is.null(state_filter)) {
    data <- data[State %in% state_filter]
  }

  # Clean Provider names
  data[, Provider := sub("\\s*\\([^\\)]+\\)", "", Provider)]

  # Convert `Completed` to POSIXct
  data[, Completed := as.POSIXct(gsub(" \\(MST\\)", "", Completed),
                                 format = "%m/%d/%Y %H:%M:%S", tz = "UTC")]

  # Group by Participant_ID, Service, Pre_Post, and calculate the count
  data[, count := .N, by = .(Participant_ID, Service, Pre_Post)]

  # Create Has_Multiple_Scores column
  data[, Has_Multiple_Scores := as.integer(count > 1)]
  data[, count := NULL]  # Remove count column

  # Calculate Time_Passed_Days for each participant and service
  data[, Time_Passed_Days := as.numeric(difftime(max(Completed),
                                                 min(Completed),
                                                 units = "days")),
       by = .(Participant_ID, Service)]
  data[, Time_Passed_Days := round(Time_Passed_Days)]

  # Split into Pre and Post data
  pre_data <- data[Pre_Post == "Pre"]
  post_data <- data[Pre_Post == "Post"]

  if (aggregate) {
    # Aggregate Pre and Post data
    pre_data <- pre_data[order(Completed), .SD[1],
                         by = .(Participant_ID, Provider, Service)]
    post_data <- post_data[order(-Completed), .SD[1],
                           by = .(Participant_ID, Provider, Service)]
  }

  # Select columns for Pre and Post datasets
  pre_selected <- pre_data[, .(Participant_ID, Provider, State, Mode,
                               Service, Time_Passed_Days, Pre_Score = Score)]
  post_selected <- post_data[, .(Participant_ID, Provider, State, Mode,
                                 Service, Time_Passed_Days, Post_Score = Score,
                                 Difference)]

  # Merge Pre and Post data
  merged_data <- merge(pre_selected, post_selected,
                       by = c("Participant_ID", "Service", "Provider",
                              "State", "Mode", "Time_Passed_Days"),
                       all = TRUE)

  # COME BACK TO THIS
  ## Summarize State and Mode per Participant_ID
  # state_mode_summary <- data[, .(
  #   State = ifelse(
  #     all(is.na(State)),
  #     first(State),
  #     names(which.max(table(State, useNA = "ifany")))[1]
  #   ),
  #   Mode = ifelse(
  #     all(is.na(Mode)),
  #     first(Mode),
  #     names(which.max(table(Mode, useNA = "ifany")))[1]
  #   )
  # ), by = Participant_ID]

  ## Merge summarized State and Mode back into the dataset
  # merged_data <- merge(merged_data, state_mode_summary, by = "Participant_ID")


  # Reshape to wide format
  scores_final <- dcast(
    merged_data,
    Participant_ID + Provider + State + Mode ~ Service,
    value.var = c("Pre_Score", "Post_Score", "Difference", "Time_Passed_Days"),
    sep = "_"
  )

  # Add additional summary calculations
  difference_cols <- grep("^Difference_", names(scores_final), value = TRUE)
  time_cols <- grep("^Time_Passed_Days", names(scores_final), value = TRUE)

  # Calculate Differences_Available
  scores_final[, Differences_Available := rowSums(!is.na(.SD)),
               .SDcols = difference_cols]

  # Calculate Median_Difference_Score
  if (length(difference_cols) > 0) {
    scores_final[, Median_Difference_Score := median(unlist(.SD), na.rm = TRUE),
                 .SDcols = difference_cols]
  } else {
    scores_final[, Median_Difference_Score := NA_real_]
  }

  # Calculate Median_Time_Passed_Days
  if (length(time_cols) > 0) {
    scores_final[, Median_Time_Passed_Days := median(unlist(.SD), na.rm = TRUE),
                 .SDcols = time_cols]
  } else {
    scores_final[, Median_Time_Passed_Days := NA_real_]
  }

  return(scores_final)
}
