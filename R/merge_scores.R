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


  # ---- Compute Age Variables ----
  # Create a birth date column using Jan 1 of birth year
  if (!("Birth_Year" %in% colnames(merged_data))) {
    warning("Birth_Year column not found in merged data. Age variables will not be created.")
    return(merged_data)
  }

  # merged_data[, Birth_Date := as.Date(paste0(Birth_Year, "-01-01"))]
  # # Identify all date columns to compute age
  # date_cols <- grep("^(Pre|Post)_Date_", names(merged_data), value = TRUE)
  #
  # for (col in date_cols) {
  #   age_col <- paste0("Age_at_", col)
  #   merged_data[[age_col]] <- as.numeric(difftime(as.Date(merged_data[[col]]),
  #                                                 merged_data$Birth_Date,
  #                                                 units = "days")) / 365.25
  # }
  #
  # ---- Compute Age Variables Using Year Only ----
  date_cols <- grep("^(Pre|Post)_Date_", names(merged_data), value = TRUE)

  for (col in date_cols) {
    age_col <- paste0("Age_at_", col)
    year_vals <- as.numeric(format(as.Date(merged_data[[col]]), "%Y"))
    merged_data[[age_col]] <- year_vals - merged_data$Birth_Year
  }

  # Clean up temporary birth_date column
  # merged_data[, Birth_Date := NULL]


  return(merged_data)
}
