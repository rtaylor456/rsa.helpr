clean_check <- function(data, aggregate = TRUE) {

  # Convert to data.table format
  setDT(data)

  # do some necessary renaming of variables
  participant <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
                      names(data), value = TRUE, perl = TRUE)

  pre_post <- grep("(?i)^(?=.*pre)(?=.*post)",
                   names(data), value = TRUE, perl = TRUE)

  names(data)[names(data) %in% participant] <- "Participant_ID"
  names(data)[names(data) %in% pre_post] <- "Pre_Post"

  # remove any unnecessary info from Provider
  provider <- grep("(?i)provider", names(data), value = TRUE, perl = TRUE)
  # remove stuff in parentheses
  data[, (provider) := lapply(.SD, function(x) sub("\\s*\\([^\\)]+\\)", "", x)),
       .SDcols = provider]

  # Remove "(MST)" and convert 'Completed' to POSIXct
  # data[, Completed := mdy_hms(gsub(" \\(MST\\)", "", Completed))]

  data[, Completed := as.POSIXct(gsub(" \\(MST\\)", "", Completed),
                                 format = "%m/%d/%Y %H:%M:%S", tz = "UTC")]


  # Group by Participant_ID, Service, Pre_Post and calculate the count
  data[, count := .N, by = .(Participant_ID, Service, Pre_Post)]

  # Create Has_Multiple_Scores column, based on count variable
  data[, Has_Multiple_Scores := as.integer(count > 1)]

  # Remove the count column
  data[, count := NULL]

  # Calculate overall Has_Multiple_Scores per participant
  overall_scores <- data[, .(Has_Multiple_Scores = max(Has_Multiple_Scores)),
                         by = Participant_ID]

  # Convert certain variables to factors
  data[, c("Participant_ID", "Service", "Provider") := lapply(.SD, as.factor),
       .SDcols = c("Participant_ID", "Service", "Provider")]

  # Calculate Time_Passed_Days
  data[, Time_Passed_Days := as.numeric(difftime(max(Completed),
                                                 min(Completed),
                                                 units = "days")),
       by = .(Participant_ID, Service)]
  data[, Time_Passed_Days := round(Time_Passed_Days)]

  # Filter Pre and Post data
  pre_data <- data[Pre_Post == "Pre"]
  post_data <- data[Pre_Post == "Post"]

  if (aggregate) {
    # Aggregate Pre and Post data
    pre_data <- pre_data[order(Completed), .SD[1],
                         by = .(Participant_ID, Provider, Service)]

    post_data <- post_data[order(-Completed), .SD[1],
                           by = .(Participant_ID, Provider, Service)]
  }

  # Select and rename columns
  pre_selected <- pre_data[, .(Participant_ID,
                               Service,
                               Provider,
                               Time_Passed_Days,
                               Pre_Score = Score)]

  post_selected <- post_data[, .(Participant_ID,
                                 Service,
                                 Provider,
                                 Time_Passed_Days,
                                 Post_Score = Score,
                                 Difference)]


  # Select and rename columns, including provider-related variables
  # pre_selected <- pre_data[, .(Participant_ID,
  #                              Service,
  #                              Provider,
  #                              Time_Passed_Days,
  #                              Pre_Score = Score,
  #                              Proctor,
  #                              Caseload,
  #                              Group_Frequency = "Group Frequency",
  #                              Online_Frequency = "Online Frequency",
  #                              Rural_Frequency = "Rural Frequency")]
  #             # Add more variables here if needed
  #
  # post_selected <- post_data[, .(Participant_ID,
  #                                Service,
  #                                Provider,
  #                                Time_Passed_Days,
  #                                Post_Score = Score,
  #                                Difference,
  #                                Proctor,
  #                                Caseload,
  #                                Group_Frequency = "Group Frequency",
  #                                Online_Frequency = "Online Frequency",
  #                                Rural_Frequency = "Rural Frequency")]
  #                         # Same as above



  # Merge the pre and post data
  merged_data <- merge(pre_selected, post_selected,
                       by = c("Participant_ID", "Service", "Provider",
                              "Time_Passed_Days"),
                       all = TRUE)

  # Convert Participant_ID to factor in both data.tables to ensure the
  #.  final_data merge works
  merged_data[, Participant_ID := as.factor(Participant_ID)]
  overall_scores[, Participant_ID := as.factor(Participant_ID)]

  # Merge with overall_scores to get correct Has_Multiple_Scores
  final_data <- merge(merged_data, overall_scores, by = "Participant_ID",
                      all.x = TRUE)

  # Reshape the data from long to wide format
  scores_final <- dcast(
    final_data,
    Participant_ID + Provider ~ Service,
    value.var = c("Pre_Score", "Post_Score", "Difference", "Time_Passed_Days",
                  "Has_Multiple_Scores"),
    sep = "_"
  )


  # Aggregating provider-related variables to retain only one value per
  #    participant
  # data[, `:=`(Proctor = first("Proctor"),
  #             Caseload = first("Caseload"),
  #             Group_Frequency = first("Group_Frequency"),
  #             Online_Frequency = first("Online_Frequency"),
  #             Rural_Frequency = first("Rural_Frequency")),
  #      by = Participant_ID]

  # Reshape the data from long to wide format
  # scores_final <- dcast(
  #   final_data,
  #   Participant_ID + Provider ~ Service,
  #   value.var = c("Pre_Score", "Post_Score", "Difference", "Time_Passed_Days",
  #                 "Has_Multiple_Scores", "Proctor", "Caseload",
  #                 "Group_Frequency", "Online_Frequency", "Rural_Frequency"),
  #         # Include provider-related variables here
  #   sep = "_"
  # )


  data <- scores_final

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

  # return(scores_final)
  return(data)
}
