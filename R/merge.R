rsa_merge <- function(data, scores,
                  data_id = "Participant_ID",
                  scores_id = "Participant.ID"){
  data_merged <- merge(data, scores,
                       by.x = data_id,
                       by.y = scores_id,
                       all = FALSE)
  # find the number of rows where we have data from both the original dataset and
  #   the new scores dataset
  complete_rows <- data_merged[complete.cases(data_merged$Difference,
                                              data_merged$E7_Application_Date_911), ]
  num_complete_rows <- nrow(complete_rows)

  num_filled_in_ids <- length(unique(complete_rows$Participant_ID))

  # Create the message
  message <- paste("Number of complete rows:", num_complete_rows, "\n",
                   "Number of unique IDs with scores:", num_filled_in_ids)

  # Print the message
  cat(message, "\n")

  # Return the merged data
  return(data_merged)

}

