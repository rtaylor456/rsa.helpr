# Most common Provider
common_provider <- merged_data[!is.na(Provider.x) | !is.na(Provider.y),
                               .(Most_Common_Provider = names(sort(table(c(Provider.x, Provider.y)), decreasing = TRUE)[1])),
                               by = Participant_ID
]

# Most common Mode
common_mode <- merged_data[!is.na(Pre_Mode) | !is.na(Post_Mode),
                           .(Most_Common_Mode = names(sort(table(c(Pre_Mode, Post_Mode)), decreasing = TRUE)[1])),
                           by = Participant_ID
]

# Most common State
common_state <- merged_data[!is.na(Pre_State) | !is.na(Post_State),
                            .(Most_Common_State = names(sort(table(c(Pre_State, Post_State)), decreasing = TRUE)[1])),
                            by = Participant_ID
]

# Merge all back into main dataset
merged_data <- merge(merged_data, common_provider, by = "Participant_ID", all.x = TRUE)
merged_data <- merge(merged_data, common_mode, by = "Participant_ID", all.x = TRUE)
merged_data <- merge(merged_data, common_state, by = "Participant_ID", all.x = TRUE)

# Drop old columns
merged_data[, c("Provider.x", "Provider.y", "Pre_Mode", "Post_Mode", "Pre_State", "Post_State") := NULL]

# Rename new columns
setnames(merged_data, c("Most_Common_Provider", "Most_Common_Mode", "Most_Common_State"),
         c("Provider", "Mode", "State"))
