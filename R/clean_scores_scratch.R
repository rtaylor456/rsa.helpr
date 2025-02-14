# # look for a state variable in the data
# state <- grep("(?i)^(?=.*state)|(?=.*\\bst\\b)(?!.*\\bst\\B)",
#               names(data), value = TRUE, perl = TRUE)
#
# if (length(state) > 0){
#   names(data)[names(data) %in% state] <- "State"
# }
#
# # If user provides state_filter and there is a state variable in data,
# if (!is.null(state_filter) & length(state) > 0){
#   # then filter by state(s) provided by user
#   data <- data[get(state) %in% state_filter]
#   # Else if user identifies states to filter by but there is no state variable,
# } else if (!is.null(state_filter) & length(state) < 1){
#   # then return a warning, but continue with cleaning process
#   warning("There is no state-identifying variable in this dataset. Cleaning process will continue on.")
# } # Else carry on (without warning message)



# pre_data <- pre_data[order(Completed), .SD[1],
#                      by = .(Participant_ID, Provider, Service)]


# selected_cols <- intersect(required_cols, names(pre_data))



# Select and rename columns
# pre_selected <- pre_data[, .(Participant_ID,
#                              Service,
#                              Provider,
#                              State,
#                              Mode,
#                              # Time_Passed_Days,
#                              Pre_Score = Score)]


# post_selected <- post_data[, .(Participant_ID,
#                                Service,
#                                Provider,
#                                State,
#                                Mode,
#                                # Time_Passed_Days,
#                                Post_Score = Score,
#                                Difference)]



# Keep Provider.x
# merged_data[, Provider := Provider.x]
# # Remove the redundant columns
# merged_data[, c("Provider.x", "Provider.y") := NULL]

# Keep the first non-missing value
# merged_data[, Provider := fifelse(
#   Provider.x == Provider.y,
#   Provider.x,
#   fifelse(!is.na(Provider.x), Provider.x, Provider.y)
# )]
#
# # Remove the redundant columns
# merged_data[, c("Provider.x", "Provider.y") := NULL]




# Ensure Most_Common_Provider is character type
# And replace NA Provider values with the most common provider
# merged_data[, Provider := fifelse(
#   is.na(Provider.x) & is.na(Provider.y),
#   as.character(Most_Common_Provider),  # Cast to character to match Provider.x/y
#   ifelse(!is.na(Provider.x), as.character(Provider.x),
#          as.character(Provider.y))  # Cast to character
# )]

# Remove the temporary 'Most_Common_Provider' column
# merged_data[, Most_Common_Provider := NULL]



# Examine our negative values--data seem to be cleaned correctly, just some
#   weird values....
# check <- final_data[final_data$Time_Passed_Days < 0, ]
# check_ids <- check$Participant_ID
# check_scores <- scores[scores$`Participant ID` %in% check_ids, ]
# check_scores <- check_scores[order(`Participant ID`)]
