data <- fread("data-raw/data_full_new.csv", stringsAsFactors = FALSE)
data <- fread("data-raw/full_data.csv", stringsAsFactors = FALSE)

scores <- fread("data-raw/trt_utah_4_8_2024.csv", stringsAsFactors = FALSE)

data_cleaned <- clean_utah(data, aggregate = TRUE)
nrow(data_cleaned) # 251215 # aggregated: 203427
length(unique(data_cleaned$Participant_ID)) # 38582 # aggregated: 32694

scores_cleaned <- clean_scores(scores)
nrow(scores_cleaned) # 1718
length(unique(scores_cleaned$Participant_ID)) # 1672

merged_data <- merge_scores(data_cleaned, scores_cleaned)
nrow(merged_data) # 8361 # when rsa-911 is aggregated: 1861
length(unique(merged_data$Participant_ID)) # 1371 # when rsa-911 is aggregated: 322

metadata <- create_metadata(merged_data)
nrow(metadata) # 1371 # when rsa-911 is aggregated: 322
length(unique(metadata$Participant_ID)) # 1371 # when rsa-911 is aggregated: 322


# predictors <- c(race_cols[2], severity_col)
#
# subset <- metadata[, .SD, .SDcols = predictors]
# nrow(subset)
# lapply(subset[, .SD, .SDcols = race_cols], table)
# lapply(subset[, .SD, .SDcols = race_cols], function(x) length(unique(x)))
# table(subset[[severity_col]])
#
# subset2 <- na.omit(subset)
# nrow(subset2)
# lapply(subset2[, .SD, .SDcols = race_cols], table)
# lapply(subset2[, .SD, .SDcols = race_cols], function(x) length(unique(x)))
# table(subset2[[severity_col]])


