# original
merge_scores <- function(quarterly_data, scores_data,
                         quarterly_id = "Participant_ID",
                         scores_id = "Participant_ID") {
  # eventually, add a check to make sure the only unique ID overlaps are kept

  merged_data <- merge(quarterly_data, scores_data,
                       by.x = quarterly_id,
                       by.y = scores_id,
                       all = FALSE)
  merged_data

}


merge_scores3 <- function(quarterly_data, scores_data,
                         quarterly_id = "Participant_ID",
                         scores_id = "Participant_ID",
                         join_type = "inner") {

  # Ensure ID columns exist
  if (!(quarterly_id %in% colnames(quarterly_data))) {
    stop(paste("Error: Column", quarterly_id, "not found in quarterly_data."))
  }
  if (!(scores_id %in% colnames(scores_data))) {
    stop(paste("Error: Column", scores_id, "not found in scores_data."))
  }

  # Check for duplicate IDs
  if (anyDuplicated(quarterly_data[[quarterly_id]]) > 0) {
    message <- paste0("Warning: Duplicate IDs found in quarterly data.",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }
  if (anyDuplicated(scores_data[[scores_id]]) > 0) {
    message <- paste0("Warning: Duplicate IDs found in scores data.",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }

  # Determine merge type
  merge_all <- switch(join_type,
                      "inner" = FALSE,
                      "left" = TRUE,
                      "right" = TRUE,
                      "full" = TRUE,
                      stop("Invalid join_type. Choose from 'inner', 'left', 'right', 'full'."))

  merged_data <- merge(quarterly_data, scores_data,
                       by.x = quarterly_id,
                       by.y = scores_id,
                       all = merge_all)


  return(merged_data)
}



merge_scores4 <- function(quarterly_data, scores_data,
                         quarterly_id = "Participant_ID",
                         scores_id = "Participant_ID") {

  # Ensure ID columns exist
  if (!(quarterly_id %in% colnames(quarterly_data))) {
    stop(paste("Error: Column", quarterly_id, "not found in quarterly_data."))
  }
  if (!(scores_id %in% colnames(scores_data))) {
    stop(paste("Error: Column", scores_id, "not found in scores_data."))
  }

  # Check for duplicate IDs
  if (anyDuplicated(quarterly_data[[quarterly_id]]) > 0) {
    message <- paste0("Warning: Duplicate IDs found in quarterly data.",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }
  if (anyDuplicated(scores_data[[scores_id]]) > 0) {
    message <- paste0("Warning: Duplicate IDs found in scores data.",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }

  # Perform inner join
  merged_data <- merge(quarterly_data, scores_data,
                       by.x = quarterly_id,
                       by.y = scores_id,
                       all = FALSE)

  # Verify that only common IDs remain
  common_ids <- intersect(quarterly_data[[quarterly_id]], scores_data[[scores_id]])
  merged_ids <- merged_data[[quarterly_id]]

  if (!setequal(merged_ids, common_ids)) {
    stop("Error: Inner join failed. The merged dataset does not contain exactly the shared IDs.")
  }

  return(merged_data)
}

