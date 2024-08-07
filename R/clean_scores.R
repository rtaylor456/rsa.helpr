library(data.table)
library(lubridate)

clean_scores <- function(data, aggregate = TRUE) {

  # Convert to data.table format
  setDT(data)

  # Remove "(MST)" and convert 'Completed' to POSIXct
  data[, Completed := mdy_hms(gsub(" \\(MST\\)", "", Completed))]

  # Group by Participant.ID, Service, Pre.Post and calculate the count
  data[, count := .N, by = .(Participant.ID, Service, Pre.Post)]

  # Create Has_Multiple_Scores column, based on count variable
  data[, Has_Multiple_Scores := as.integer(count > 1)]

  # Remove the count column
  data[, count := NULL]

  # Calculate overall Has_Multiple_Scores per participant
  overall_scores <- data[, .(Has_Multiple_Scores = max(Has_Multiple_Scores)), by = Participant.ID]

  # Convert certain variables to factors
  data[, c("Participant.ID", "Service", "Provider") := lapply(.SD, as.factor),
       .SDcols = c("Participant.ID", "Service", "Provider")]

  # Calculate Time_Passed_Days
  data[, Time_Passed_Days := as.numeric(difftime(max(Completed),
                                                 min(Completed),
                                                 units = "days")),
       by = .(Participant.ID, Service)]
  data[, Time_Passed_Days := round(Time_Passed_Days)]

  # Filter Pre and Post data
  pre_data <- data[Pre.Post == "Pre"]
  post_data <- data[Pre.Post == "Post"]

  if (aggregate) {
    # Aggregate Pre and Post data
    pre_data <- pre_data[order(Completed), .SD[1], by = .(Participant.ID, Provider, Service)]
    post_data <- post_data[order(-Completed), .SD[1], by = .(Participant.ID, Provider, Service)]
  }

  # Select and rename columns
  pre_selected <- pre_data[, .(Participant.ID,
                               Service,
                               Provider,
                               Time_Passed_Days,
                               Pre_Score = Score)]

  post_selected <- post_data[, .(Participant.ID,
                                 Service,
                                 Provider,
                                 Time_Passed_Days,
                                 Post_Score = Score,
                                 Difference)]

  # Merge the pre and post data
  merged_data <- merge(pre_selected, post_selected,
                       by = c("Participant.ID", "Service", "Provider", "Time_Passed_Days"),
                       all = TRUE)

  # Merge with overall_scores to get correct Has_Multiple_Scores
  final_data <- merge(merged_data, overall_scores, by = "Participant.ID", all.x = TRUE)

  # Reshape the data from long to wide format
  scores_final <- dcast(
    final_data,
    Participant.ID + Provider ~ Service,
    value.var = c("Pre_Score", "Post_Score", "Difference", "Time_Passed_Days", "Has_Multiple_Scores"),
    sep = "_"
  )

  return(scores_final)
}


