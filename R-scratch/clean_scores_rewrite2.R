#' Clean Scores Data
#'
#' This function cleans a scores dataset, based on the standard data
#'   structure.
#'
#' @param data The scores dataset.
#' @param aggregate TRUE or FALSE. Defaults to TRUE, when TRUE, rows are
#'   aggregated to include only unique participants are kept.
#'
#' @returns A cleaned data frame, restructured to a wide format, to help with
#'   merging process.
#'
#' @export
#' @import data.table

clean_scores3 <- function(data, state_filter = NULL, aggregate = TRUE) {

  # Convert to data.table format
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
  completed <- grep("(?i)^(?=.*complete)|(?=.*date)",
                    names(data), value = TRUE, perl = TRUE)

  # ADD TO THIS

  # Rename columns for consistency
  names(data)[names(data) %in% participant] <- "Participant_ID"
  names(data)[names(data) %in% state] <- "State"
  names(data)[names(data) %in% provider] <- "Provider"
  names(data)[names(data) %in% mode] <- "Mode"
  names(data)[names(data) %in% pre_post] <- "Pre_Post"
  names(data)[names(data) %in% completed] <- "Completed"

  # Apply state filter, if provided by user
  if (!is.null(state_filter)) {
    data <- data[State %in% state_filter]
  }

  # Order data by participant
  data <- data[order(Participant_ID)]

  # remove stuff in parentheses
  data[, (provider) := lapply(.SD, function(x) sub("\\s*\\([^\\)]+\\)", "", x)),
       .SDcols = provider]
  # COME BACK HERE
  # add code to abbreviate the names systematically--unless they are already
  #  abbreviated

  # Create a new variable Provider_summary that contains the most common
  #   provider per participant
  data[, Provider_summary := names(sort(table(Provider), decreasing = TRUE))[1],
       by = Participant_ID]

  # do the same thing for Mode

  # data[, Mode_summary := names(sort(table(Mode), decreasing = TRUE))[1],
  #      by = Participant_ID]
  data[, Mode_summary := {
    mode_table <- table(Mode)  # Create a frequency table for 'Mode'
    sorted_modes <- sort(mode_table, decreasing = TRUE)  # Sort in decreasing order
    most_common <- names(sorted_modes)[1]  # Most common value

    # Check if the most common value is NA
    if (most_common %in% c("NA", " ", "", NULL) ) {
      second_most_common <- names(sorted_modes)[2]  # Get the second most common value
      second_most_common  # Assign it to Mode_summary
    } else {
      most_common  # Assign the most common value to Mode_summary
    }
  }, by = Participant_ID]

  # Clean and convert Mode
  # Ordinal: no help > observer > with help, NULL


  # Remove "(MST)" and convert 'Completed' to POSIXct
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
  ## ADD to this

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
                               Provider_summary,
                               State,
                               Mode_summary,
                               Time_Passed_Days,
                               Pre_Score = Score)]

  post_selected <- post_data[, .(Participant_ID,
                                 Service,
                                 Provider_summary,
                                 State,
                                 Mode_summary,
                                 Time_Passed_Days,
                                 Post_Score = Score,
                                 Difference)]


  # Merge the pre and post data
  merged_data <- merge(pre_selected, post_selected,
                       by = c("Participant_ID", "Service", "Provider_summary",
                              "Mode_summary",
                              "Time_Passed_Days"),
                       all = TRUE)

  # Combine State and Mode columns
  merged_data[, State := fifelse(!is.na(State.x), State.x, State.y)]

  # merged_data[, Mode := fifelse(!is.na(Mode.x), Mode.x, Mode.y)]

  # Drop unnecessary columns
  # merged_data[, c("State.x", "State.y", "Mode.x", "Mode.y") := NULL]
  merged_data[, c("State.x", "State.y") := NULL]

  ## This is the new code to try to prevent an error in merge ##
  # Convert Participant_ID to factor in both data.tables to ensure the
  #.  final_data merge works
  merged_data[, Participant_ID := as.factor(Participant_ID)]
  overall_scores[, Participant_ID := as.factor(Participant_ID)]

  # # Condense data by converting all Provider values to the most common Provider
  # #   per Participant_ID
  # merged_data[, Provider := names(sort(table(Provider), decreasing = TRUE))[1],
  #             by = Participant_ID]
  #
  # # Optionally, you can do the same for Mode if needed
  # merged_data[, Mode := names(sort(table(Mode), decreasing = TRUE))[1],
  #             by = Participant_ID]


  # Merge with overall_scores to get correct Has_Multiple_Scores
  final_data <- merge(merged_data, overall_scores, by = "Participant_ID",
                      all.x = TRUE)

  # Reshape the data from long to wide format
  scores_final <- dcast(
    final_data,
    Participant_ID + Provider_summary + State + Mode_summary ~ Service,
    value.var = c("Pre_Score", "Post_Score", "Difference", "Time_Passed_Days",
                  "Has_Multiple_Scores"),
    fun.aggregate = median,
    sep = "_"
  )

  # # Try casting a simpler version to debug
  # scores_final_simple <- dcast(
  #   final_data,
  #   Participant_ID + Provider_summary + State + Mode_summary ~ Service,
  #   value.var = "Pre_Score",  # Simplify to just one variable to debug
  #   fun.aggregate = median,
  #   sep = "_"
  # )
  #
  # str(scores_final_simple)  # Examine the structure of the reshaped data



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

