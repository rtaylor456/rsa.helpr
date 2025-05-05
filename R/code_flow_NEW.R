data <- data.table::fread("data-raw/data_load_2025-04-22.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)


data_clean <- clean_utah(data, aggregate = FALSE)
scores_clean <- clean_scores(scores, state_filter = "Utah")


## Get the count of unique overlapping ids
overlap_ids <- as.factor(data_clean$Participant_ID)[as.factor(
  data_clean$Participant_ID) %in% scores_clean$Participant_ID]
length(unique(overlap_ids)) # 2229

overlap_data <- data_clean[as.factor(data_clean$Participant_ID) %in%
                             overlap_ids, ]
length(unique(overlap_data$Participant_ID)) # make sure we have the 2229 ids

overlap_data[, E7_Application_Date_911_NEW :=
               handle_mixed_date(E7_Application_Date_911)]


have_dates <- overlap_data[!is.na(E7_Application_Date_911), ]
dim(have_dates)
length(unique(have_dates$Participant_ID))


have_dates <- merged[!is.na(E7_Application_Date_911), ]
dim(have_dates)
length(unique(have_dates$Participant_ID)) # 326

merged <- merge_scores(data_clean, scores_clean)

length(unique(merged$Participant_ID)) ## 2206

have_dates <- merged[!is.na(E7_Application_Date_911), ]
dim(have_dates)
length(unique(have_dates$Participant_ID)) # 326

metadata <- create_metadata(merged)

length(unique(metadata$Participant_ID))

have_dates <- metadata[!is.na(E7_Application_Date_911), ]
dim(have_dates)
length(unique(have_dates$Participant_ID)) # 326
