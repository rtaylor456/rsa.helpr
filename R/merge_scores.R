#' Merge Scores Data with RSA-911 Data
#'
#' This function merges cleaned Utah scores data with cleaned Utah RSA-911 Data.
#'
#' @param quarterly_data The cleaned RSA-911 dataset.
#' @param scores_data The cleaned scores dataset.
#' @param quarterly_ID A string for the variable name corresponding to
#'   participant ID in the cleaned RSA-911 dataset. Defaults to
#'   "Participant_ID".
#' @param scores_ID A string for the variable name corresponding to
#'   participant ID in the cleaned scores dataset. Defaults to "Participant_ID".
#'
#' @returns A data frame with one row per participant.
#'
#' @export

merge_scores <- function(quarterly_data, scores_data,
                         quarterly_ID = "Participant_ID",
                         scores_ID = "Participant_ID") {
  # eventually, add a check to make sure the only unique ID overlaps are kept

  merged_data <- merge(quarterly_data, scores_data,
                       by.x = "Participant_ID",
                       by.y = "Participant_ID",
                       all = FALSE)
  merged_data

}
