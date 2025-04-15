############
# PREP DATA #
############
data <- data.table::fread("data-raw/data_load_2025-02-17.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)


data_clean <- rsa.helpr::clean_utah(data)
scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")
provider_data <- rsa.helpr::clean_provider(scores, state_filter = "Utah")

merged <- rsa.helpr::merge_scores(data_clean, scores_clean)

# metadata <- rsa.helpr::create_metadata(merged)
metadata <- create_metadata(merged)


metadata_corrected <- metadata[Age_At_Application >= 14 &
                                 Age_At_Application <= 22, ]


# temporary method for correcting non-integer ages (created from taking medians
#   in metadata process)
metadata_corrected$Age_At_Application <- ceiling(
  metadata_corrected$Age_At_Application)

################################################################################
############
# OVERVIEW #
############

summary_table_metadata <- summarize_scores_formatted(metadata_corrected,
                                                     robust_measures = TRUE)
summary_table_metadata


# Per participant values
table(scores_clean$Has_Multiple_Scores)

summary(scores_clean$Differences_Available)
sd(scores_clean$Differences_Available)

# Identify the relevant columns for each type of score
pre_score_cols <- grep("^Pre_Score", names(scores_clean), value = TRUE)
post_score_cols <- grep("^Post_Score", names(scores_clean), value = TRUE)
difference_cols <- grep("^Difference_", names(scores_clean), value = TRUE)

# Initialize the Services_Present column to 0 for all participants
scores_clean[, Services_Present := 0]

# Loop over the services and check if any of the Pre, Post, or Difference scores are non-missing
services <- unique(sub("^(Pre_Score_|Post_Score_|Difference_)", "",
                       c(pre_score_cols, post_score_cols, difference_cols)))

for(service in services) {
  # Get the columns related to this service
  service_cols <- c(
    grep(paste0("^Pre_Score_", service), names(scores_clean), value = TRUE),
    grep(paste0("^Post_Score_", service), names(scores_clean), value = TRUE),
    grep(paste0("^Difference_", service), names(scores_clean), value = TRUE)
  )

  # Update Services_Present: Increment by 1 if any of the columns for the service are non-missing
  scores_clean[, Services_Present := Services_Present +
                 as.integer(rowSums(!is.na(.SD)) > 0), .SDcols = service_cols,
               by = Participant_ID]
}

summary(scores_clean$Services_Present)
sd(scores_clean$Services_Present)



# Per participant values
table(metadata_corrected$Has_Multiple_Scores)

summary(metadata_corrected$Differences_Available)
sd(metadata_corrected$Differences_Available)

# Identify the relevant columns for each type of score
pre_score_cols <- grep("^Pre_Score", names(metadata_corrected), value = TRUE)
post_score_cols <- grep("^Post_Score", names(metadata_corrected), value = TRUE)
difference_cols <- grep("^Difference_", names(metadata_corrected), value = TRUE)

# Initialize the Services_Present column to 0 for all participants
metadata_corrected[, Services_Present := 0]

# Loop over the services and check if any of the Pre, Post, or Difference scores are non-missing
services <- unique(sub("^(Pre_Score_|Post_Score_|Difference_)", "",
                       c(pre_score_cols, post_score_cols, difference_cols)))

for(service in services) {
  # Get the columns related to this service
  service_cols <- c(
    grep(paste0("^Pre_Score_", service), names(metadata_corrected), value = TRUE),
    grep(paste0("^Post_Score_", service), names(metadata_corrected), value = TRUE),
    grep(paste0("^Difference_", service), names(metadata_corrected), value = TRUE)
  )

  # Update Services_Present: Increment by 1 if any of the columns for the service are non-missing
  metadata_corrected[, Services_Present := Services_Present +
                 as.integer(rowSums(!is.na(.SD)) > 0), .SDcols = service_cols,
               by = Participant_ID]
}

summary(metadata_corrected$Services_Present)
sd(metadata_corrected$Services_Present)

################################################################################
################
# PSE ENROLLMENT
################
# For some PSE enrollment
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
                  label1 = "Enrolled", label2 = "Not Enrolled")

################################################################################
################
# PSE COMPLETION
################
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
                  label1 = "Completed", label2 = "Not Completed")

################################################################################
#####################
# DISABILITY PRIORITY
#####################
## Disability Priority
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
                  label1 = "Priority 1", label2 = "Priority 2")


################################################################################
##############
# UNEMPLOYMENT
##############

long_unemployed <- metadata_corrected[E62_Plan_Long_Term_Unemployment_911 == 1]
summary_unemployed <- summarize_scores_formatted(long_unemployed,
                                                   robust_measures = TRUE)
summary_unemployed

not_long_unemployed <- metadata_corrected[E62_Plan_Long_Term_Unemployment_911 %in%
                                         c(0, "NULL")]
summary_not_long_unemployed <- summarize_scores_formatted(not_long_unemployed,
                                                       robust_measures = TRUE)
summary_not_long_unemployed


combined_summary <- combine_summaries(summary_unemployed,
                                      summary_not_long_unemployed,
                                      "Longterm Unemployed", "Not Longterm Unemployed")

combined_summary

compare_densities(long_unemployed, not_long_unemployed, variable = "Longterm Unemployment",
                  label1 = "Longterm Unemployed", label2 = "Not Longterm Unemployed")


################################################################################
#######
# TANF
#######
tanf <- metadata_corrected[E63_Plan_Exhaust_TANF_911 == 1]
summary_tanf <- summarize_scores_formatted(tanf,robust_measures = TRUE)
summary_tanf

not_tanf <- metadata_corrected[E63_Plan_Exhaust_TANF_911 %in%
                                            c(0, "NULL")]
summary_not_tanf <- summarize_scores_formatted(not_tanf,
                                                          robust_measures = TRUE)
summary_not_tanf


combined_summary <- combine_summaries(summary_tanf,
                                      summary_not_tanf,
                                      "Exhausting TANF", "Not Exhausting TANF")

combined_summary

compare_densities(long_unemployed, not_long_unemployed, variable = "Exhausting TANF",
                  label1 = "Exhausting TANF", label2 = "Not Exhausting TANF")



################################################################################
##############
# FOSTER CARE
##############


################################################################################
##########
# HOMELESS
##########

################################################################################
##########
# OFFENDER
##########

################################################################################
#############
# LOW INCOME
#############


################################################################################
##################
# ENGLISH LEARNER
##################

################################################################################
###################
# SKILLS DEFICIENT
###################
skills_deficient <- metadata_corrected[E69_Plan_Skills_Deficient_911 == 1]

summary_skills_deficient <- summarize_scores_formatted(skills_deficient,
                                                       robust_measures = TRUE)
summary_skills_deficient



not_skills_deficient <- metadata_corrected[E69_Plan_Skills_Deficient_911 == 0]

summary_not_skills_deficient <- summarize_scores_formatted(not_skills_deficient,
                                                           robust_measures = TRUE)
summary_not_skills_deficient



combined_summary <- combine_summaries(summary_skills_deficient,
                                      summary_not_skills_deficient,
                                      "Skills Deficient", "Not Skills Deficient")
combined_summary


################################################################################
####################
# CULTURAL BARRIERS
####################

################################################################################
################
# SINGLE PARENT
################

################################################################################
#####################
# DISPLACED HOMEMAKER
#####################

################################################################################
############
# FARMWORKER
############




################################################################################
################################################################################
# GROUPINGS
################################################################################
################################################################################


################################################################################
####################
# Econ. Marginalized
####################
# For E65_Plan_Homeless_911, E67_Plan_Low_Income_911, E63_Plan_Exhaust_TANF_911
econ_marginalized <- metadata_corrected[
  E65_Plan_Homeless_911 == 1 |
    E67_Plan_Low_Income_911 == 1 |
    E63_Plan_Exhaust_TANF_911 == 1]

summary_econ_marginalized <- summarize_scores_formatted(econ_marginalized,
                                                        robust_measures = TRUE)
summary_econ_marginalized


not_marginalized <- metadata_corrected[
  E65_Plan_Homeless_911 == 0 &
    E67_Plan_Low_Income_911 == 0 &
    E63_Plan_Exhaust_TANF_911 == 0]

summary_not_marginalized <- summarize_scores_formatted(not_marginalized,
                                                       robust_measures = TRUE)
summary_not_marginalized


