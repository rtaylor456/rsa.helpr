#' Merge Scores Data with RSA-911 Data
#'
#' This function merges cleaned Utah scores data with cleaned Utah RSA-911 Data.
#'
#' @param quarterly_data The cleaned RSA-911 dataset.
#' @param scores_data The cleaned scores dataset.
#' @param quarterly_id A string for the variable name corresponding to
#'   participant ID in the cleaned RSA-911 dataset. Defaults to
#'   "Participant_ID".
#' @param scores_id A string for the variable name corresponding to
#'   participant ID in the cleaned scores dataset. Defaults to "Participant_ID".
#'
#' @returns A data frame with one row per participant.
#'
#' @export

merge_scores <- function(quarterly_data, scores_data,
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
    message <- paste0("Warning: Duplicate IDs found in quarterly data. ",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }
  if (anyDuplicated(scores_data[[scores_id]]) > 0) {
    message <- paste0("Warning: Duplicate IDs found in scores data. ",
                      "Try applying create_metadata next to condense.")
    warning(message)
  }

  # Perform inner join
  merged_data <- merge(quarterly_data, scores_data,
                       by.x = quarterly_id,
                       by.y = scores_id,
                       all = FALSE)

  # Verify that only common IDs remain
  common_ids <- intersect(quarterly_data[[quarterly_id]],
                          scores_data[[scores_id]])
  merged_ids <- merged_data[[quarterly_id]]

  if (!setequal(merged_ids, common_ids)) {
    message <- paste0("Error: Inner join failed. The merged dataset does not ",
                      "contain only the overlapping IDs.")
    stop(message)
  }

  return(merged_data)
}
