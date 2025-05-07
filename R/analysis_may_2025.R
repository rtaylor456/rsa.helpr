#######################
## LOAD & CLEAN DATA ##
#######################
################################################################################
data <- data.table::fread("data-raw/data_load_2025-04-22.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)

data_clean <- clean_utah(data, aggregate = FALSE)
scores_clean <- clean_scores(scores, state_filter = "Utah")
merged <- merge_scores(data_clean, scores_clean)
metadata <- create_metadata(merged)

################################################################################

#########################
## NUMERICAL SUMMARIES ##
#########################
################################################################################

## UNIQUE IDS
length(unique(data$Participant_ID))
length(unique(data_clean$Participant_ID)) # 48801

# (only want cleaned scores IDs, since we need to extract UT IDs)
length(unique(scores_clean$Participant_ID)) # 2312

# Overlapping IDs from scores that are in RSA data, raw or cleaned
overlap_ids <- as.factor(data$Participant_ID)[as.factor(
  data$Participant_ID) %in% scores_clean$Participant_ID]
length(unique(overlap_ids)) # 2229

overlap_ids <- as.factor(data_clean$Participant_ID)[as.factor(
  data_clean$Participant_ID) %in% scores_clean$Participant_ID]
length(unique(overlap_ids)) # 2229

length(unique(merged$Participant_ID)) # 2229


length(unique(metadata$Participant_ID)) # 2229

metadata_corrected <- metadata[(Age_At_Application >= 14 &
                                  Age_At_Application <= 22) |
                                 is.na(Age_At_Application), ]
length(unique(metadata_corrected$Participant_ID))

## COUNTS OF PARTICIPANTS WITH APPLICATION DATES
## Grabbing just overlapping data more raw--before merged--
overlap_data <- data_clean[as.factor(data_clean$Participant_ID) %in%
                             overlap_ids, ]

have_dates <- overlap_data[!is.na(E7_Application_Date_911), ]
dim(have_dates)
length(unique(have_dates$Participant_ID)) # 525


merged_with_dates <- merged[!is.na(E7_Application_Date_911), ]
dim(merged_with_dates)
length(unique(merged_with_dates$Participant_ID)) # 525

################################################################################

##################################################
## VISUALIZATIONS ACROSS PRE, POST, DIFF SCORES ##
##################################################
################################################################################

pse_complete <- metadata_corrected[Income_Struggle == 1 |
                                     Cultural_Struggle == 1 |
                                     Housing_Struggle == 1 |
                                     Support_Struggle == 1]

summary_pse_complete <- summarize_scores_formatted(pse_complete,
                                                   robust_measures = TRUE)
summary_pse_complete

pse_not_complete <- metadata_corrected[E86_PostSecondary_Partial_Completion_911 %in%
                                         c(0, "NULL")]
summary_pse_not_complete <- summarize_scores_formatted(pse_not_complete,
                                                       robust_measures = TRUE)
summary_pse_not_complete


combined_summary <- combine_summaries(summary_pse_complete,
                                      summary_pse_not_complete,
                                      "PSE Complete", "PSE Not Complete")

combined_summary

compare_densities(pse_complete, pse_not_complete, variable = "PSE Completion",
                  label1 = "Completed", label2 = "Not Completed",
                  score_type = "Pre_Score")

compare_densities(pse_complete, pse_not_complete, variable = "PSE Completion",
                  label1 = "Completed", label2 = "Not Completed",
                  score_type = "Post_Score")

compare_densities(pse_complete, pse_not_complete, variable = "PSE Completion",
                  label1 = "Completed", label2 = "Not Completed")



## Income Struggle
table(metadata_corrected$Income_Struggle)

income_1 <- metadata_corrected[Income_Struggle == 1]

summary_income_1 <- summarize_scores_formatted(income_1,
                                                 robust_measures = TRUE)
summary_income_1

income_0 <- metadata_corrected[Income_Struggle == 0]

summary_income_0 <- summarize_scores_formatted(income_0,
                                                 robust_measures = TRUE)
summary_income_0


combined_summary <- combine_summaries(summary_income_1,
                                      summary_income_0,
                                      "Income Struggle", "No Income Struggle")

combined_summary

compare_densities(income_1, income_0, variable = "Income Struggle",
                  label1 = "Income Struggle", label2 = "No Income Struggle",
                  score_type = "Pre_Score")

compare_densities(income_1, income_0, variable = "Income Struggle",
                  label1 = "Income Struggle", label2 = "No Income Struggle",
                  score_type = "Post_Score")

compare_densities(income_1, income_0, variable = "Income Struggle",
                  label1 = "Income Struggle", label2 = "No Income Struggle")


################################################################################
## Cultural Struggle
table(metadata_corrected$Cultural_Struggle)

cultural_1 <- metadata_corrected[Cultural_Struggle == 1]

summary_cultural_1 <- summarize_scores_formatted(cultural_1,
                                                robust_measures = TRUE)
summary_cultural_1

cultural_0 <- metadata_corrected[Cultural_Struggle == 0]

summary_cultural_0 <- summarize_scores_formatted(cultural_0,
                                                robust_measures = TRUE)
summary_cultural_0


combined_summary <- combine_summaries(summary_cultural_1,
                                      summary_cultural_0,
                                      "Cultural Struggle", "No Cultural Struggle")

combined_summary

compare_densities(cultural_1, cultural_0, variable = "Cultural Struggle",
                  label1 = "Cultural Struggle", label2 = "No Cultural Struggle",
                  score_type = "Pre_Score")

compare_densities(cultural_1, cultural_0, variable = "Cultural Struggle",
                  label1 = "Cultural Struggle", label2 = "No Cultural Struggle",
                  score_type = "Post_Score")

compare_densities(cultural_1, cultural_0, variable = "Cultural Struggle",
                  label1 = "Cultural Struggle", label2 = "No Cultural Struggle")


################################################################################
## Support Struggle
table(metadata_corrected$Support_Struggle)

support_1 <- metadata_corrected[Support_Struggle == 1]

summary_support_1 <- summarize_scores_formatted(support_1,
                                                robust_measures = TRUE)
summary_support_1

support_0 <- metadata_corrected[Support_Struggle == 0]

summary_support_0 <- summarize_scores_formatted(support_0,
                                                robust_measures = TRUE)
summary_support_0


combined_summary <- combine_summaries(summary_support_1,
                                      summary_support_0,
                                      "Support Struggle", "No Support Struggle")

combined_summary

compare_densities(support_1, support_0, variable = "Support Struggle",
                  label1 = "Support Struggle", label2 = "No Support Struggle",
                  score_type = "Pre_Score")

compare_densities(support_1, support_0, variable = "Support Struggle",
                  label1 = "Support Struggle", label2 = "No Support Struggle",
                  score_type = "Post_Score")

compare_densities(support_1, support_0, variable = "Support Struggle",
                  label1 = "Support Struggle", label2 = "No Support Struggle")


################################################################################
## Housing Struggle
table(metadata_corrected$Housing_Struggle)

housing_1 <- metadata_corrected[Housing_Struggle == 1]

summary_housing_1 <- summarize_scores_formatted(housing_1,
                                                 robust_measures = TRUE)
summary_housing_1

housing_0 <- metadata_corrected[Housing_Struggle == 0]

summary_housing_0 <- summarize_scores_formatted(housing_0,
                                                robust_measures = TRUE)
summary_housing_0


combined_summary <- combine_summaries(summary_housing_1,
                                      summary_housing_0,
                                      "Housing Struggle", "No Housing Struggle")

combined_summary

compare_densities(housing_1, housing_0, variable = "Housing Struggle",
                  label1 = "Housing Struggle", label2 = "No Housing Struggle",
                  score_type = "Pre_Score")

compare_densities(housing_1, housing_0, variable = "Housing Struggle",
                  label1 = "Housing Struggle", label2 = "No Housing Struggle",
                  score_type = "Post_Score")

compare_densities(housing_1, housing_0, variable = "Housing Struggle",
                  label1 = "Housing Struggle", label2 = "No Housing Struggle")

################################################################################
## Disability Priority
table(metadata_corrected$E45_Disability_Priority_911)

priority_1 <- metadata_corrected[E45_Disability_Priority_911 == 1]

summary_priority_1 <- summarize_scores_formatted(priority_1,
                                                 robust_measures = TRUE)
summary_priority_1

priority_2 <- metadata_corrected[E45_Disability_Priority_911 == 2]

summary_priority_2 <- summarize_scores_formatted(priority_2,
                                                 robust_measures = TRUE)
summary_priority_2


combined_summary <- combine_summaries(summary_priority_1,
                                      summary_priority_2,
                                      "Priority 1", "Priority 2")

combined_summary


compare_densities(priority_1, priority_2, variable = "Disability Priority",
                  label1 = "Priority 1", label2 = "Priority 2",
                  score_type = "Pre_Score")

compare_densities(priority_1, priority_2, variable = "Disability Priority",
                  label1 = "Priority 1", label2 = "Priority 2",
                  score_type = "Post_Score")

compare_densities(priority_1, priority_2, variable = "Disability Priority",
                  label1 = "Priority 1", label2 = "Priority 2")


################################################################################
## PSE Enrollment
table(metadata_corrected$E84_PostSecondary_Enrollment_911)

pse_enrolled <- metadata_corrected[E84_PostSecondary_Enrollment_911 %in% c(1, 2, 3)]
summary_pse_enrolled <- summarize_scores_formatted(pse_enrolled,
                                                   robust_measures = TRUE)
summary_pse_enrolled

pse_not_enrolled <- metadata_corrected[E84_PostSecondary_Enrollment_911 == 0]
summary_pse_not_enrolled <- summarize_scores_formatted(pse_not_enrolled,
                                                       robust_measures = TRUE)
summary_pse_not_enrolled


combined_summary <- combine_summaries(summary_pse_enrolled,
                                      summary_pse_not_enrolled,
                                      "PSE Enroll.", "PSE Not Enroll.")
combined_summary


compare_densities(pse_enrolled, pse_not_enrolled, variable = "Enrollment",
                  label1 = "Enrolled", label2 = "Not Enrolled",
                  score_type = "Pre_Score")

compare_densities(pse_enrolled, pse_not_enrolled, variable = "Enrollment",
                  label1 = "Enrolled", label2 = "Not Enrolled",
                  score_type = "Post_Score")

compare_densities(pse_enrolled, pse_not_enrolled, variable = "Enrollment",
                  label1 = "Enrolled", label2 = "Not Enrolled")


################################################################################
## PSE Completion
table(metadata_corrected$E86_PostSecondary_Partial_Completion_911)

pse_complete <- metadata_corrected[E86_PostSecondary_Partial_Completion_911 == 1]
summary_pse_complete <- summarize_scores_formatted(pse_complete,
                                                   robust_measures = TRUE)
summary_pse_complete

pse_not_complete <- metadata_corrected[E86_PostSecondary_Partial_Completion_911 %in%
                                         c(0, "NULL")]
summary_pse_not_complete <- summarize_scores_formatted(pse_not_complete,
                                                       robust_measures = TRUE)
summary_pse_not_complete


combined_summary <- combine_summaries(summary_pse_complete,
                                      summary_pse_not_complete,
                                      "PSE Complete", "PSE Not Complete")

combined_summary

compare_densities(pse_complete, pse_not_complete, variable = "PSE Completion",
                  label1 = "Completed", label2 = "Not Completed",
                  score_type = "Pre_Score")

compare_densities(pse_complete, pse_not_complete, variable = "PSE Completion",
                  label1 = "Completed", label2 = "Not Completed",
                  score_type = "Post_Score")

compare_densities(pse_complete, pse_not_complete, variable = "PSE Completion",
                  label1 = "Completed", label2 = "Not Completed")

################################################################################
## Age
hist(metadata_corrected$Median_Age_Diff_TRT)

metadata$Min_Pre_Age

ggplot(metadata_corrected, aes(x = Min_Pre_Age)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Minimum Pre Score Age (TRT)",
    x = "Min Pre Score Age (TRT)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggplot(metadata_corrected, aes(x = Max_Post_Age)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Maximum Post Score Age (TRT)",
    x = "Max Post Score Age (TRT)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )


ggplot(metadata_corrected, aes(x = Median_Age_Diff_TRT)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
  labs(
    title = "Median Years Spent in TRT",
    x = "Years",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )


