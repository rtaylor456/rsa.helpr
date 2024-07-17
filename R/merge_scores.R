# see how many overlapping participants we have
overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_final$Participant.ID))

length(overlap_idx) # 329
# so, 329 observations that contain demographic/info data (data_aggregate info)
#   and pre-post scores

data_merged <- merge(data_aggregate, scores_final,
                     by.x = "Participant_ID",
                     by.y = "Participant.ID",
                     all = FALSE) # all = TRUE adds all rows
