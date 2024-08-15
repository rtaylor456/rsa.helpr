merge_scores <- function(quarterly_data, scores_data,
                         quarterly_ID = "Participant_ID",
                         scores_ID = "Participant_ID") {
  # eventually, add a check to make sure the only unique ID overlaps are kept

  merged_data <- merge(quarterly_data, scores_data,
                       by.x = "Participant_ID",
                       by.y = "Participant_ID",
                       all = FALSE)

}
