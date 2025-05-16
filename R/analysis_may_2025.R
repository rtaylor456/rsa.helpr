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


# age_cols <- grep("(?i)(?=.*(Age_At|Median_Age|Age_Diff\\bMin\\b|\\bMax\\b))",
#                  names(metadata), value = TRUE, perl = TRUE)


metadata_corrected <- metadata[(Age_At_Application >= 14 &
                                  Age_At_Application <= 22) |
                                 is.na(Age_At_Application), ]
# this version will lead to 2209 IDs, with 505 IDs with dates

metadata_corrected <- metadata[(Age_At_Application >= 14 &
                                  Age_At_Application <= 22) &
                                 Median_Age >= 14 &
                                 Median_Age <= 22 |
                                 is.na(Age_At_Application), ]
# this version will lead to 2203 IDs, with 499 IDs with dates

################################################################################

#########################
## NUMERICAL SUMMARIES ##
#########################
################################################################################

## UNIQUE IDS
length(unique(data$Participant_ID)) # 48802
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
length(unique(metadata_corrected$Participant_ID)) # 2209

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
################################################
## SEPARATE ROWS WITH APP DATE VS NO APP DATE ##
################################################

has_dates <- metadata_corrected[!is.na(E7_Application_Date_911), ]
dim(has_dates)

no_dates <- metadata_corrected[is.na(E7_Application_Date_911), ]
dim(no_dates)



##################################################
## VISUALIZATIONS ACROSS PRE, POST, DIFF SCORES ##
##################################################
################################################################################

pse_complete <- has_dates[E86_PostSecondary_Partial_Completion_911 == 1]

summary_pse_complete <- summarize_scores_formatted(pse_complete,
                                                   robust_measures = TRUE)
summary_pse_complete

pse_not_complete <- has_dates[E86_PostSecondary_Partial_Completion_911 %in%
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
#######################
## FACING ANY STRUGGLE
#######################

facing_struggle <- has_dates[Income_Struggle == 1 |
                               Cultural_Struggle == 1 |
                               Housing_Struggle == 1 |
                               Support_Struggle == 1]

summary_facing_struggle <- summarize_scores_formatted(facing_struggle,
                                               robust_measures = TRUE)
summary_facing_struggle

no_struggle <- has_dates[Income_Struggle != 1 &
                               Cultural_Struggle != 1 &
                               Housing_Struggle != 1 &
                               Support_Struggle != 1]

summary_no_struggle <- summarize_scores_formatted(no_struggle,
                                               robust_measures = TRUE)
summary_no_struggle


combined_summary <- combine_summaries(summary_facing_struggle,
                                      summary_no_struggle,
                                      "Facing Struggle(s)", "No Struggle(s)")

combined_summary

compare_densities(facing_struggle, no_struggle, variable = "Facing Struggle",
                  label1 = "Facing Struggle", label2 = "No Struggle",
                  score_type = "Pre_Score")

compare_densities(facing_struggle, no_struggle, variable = "Facing Struggle",
                  label1 = "Facing Struggle", label2 = "No Struggle",
                  score_type = "Post_Score")

compare_densities(facing_struggle, no_struggle, variable = "Facing Struggle",
                  label1 = "Facing Struggle", label2 = "No Struggle")



################################################################################
## Income Struggle
table(has_dates$Income_Struggle)

income_1 <- has_dates[Income_Struggle == 1]

summary_income_1 <- summarize_scores_formatted(income_1,
                                                 robust_measures = TRUE)
summary_income_1

income_0 <- has_dates[Income_Struggle == 0]

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
table(has_dates$Cultural_Struggle)

cultural_1 <- has_dates[Cultural_Struggle == 1]

summary_cultural_1 <- summarize_scores_formatted(cultural_1,
                                                robust_measures = TRUE)
summary_cultural_1

cultural_0 <- has_dates[Cultural_Struggle == 0]

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
table(has_dates$Support_Struggle)

support_1 <- has_dates[Support_Struggle == 1]

summary_support_1 <- summarize_scores_formatted(support_1,
                                                robust_measures = TRUE)
summary_support_1

support_0 <- has_dates[Support_Struggle == 0]

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
table(has_dates$Housing_Struggle)

housing_1 <- has_dates[Housing_Struggle == 1]

summary_housing_1 <- summarize_scores_formatted(housing_1,
                                                 robust_measures = TRUE)
summary_housing_1

housing_0 <- has_dates[Housing_Struggle == 0]

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
table(has_dates$E45_Disability_Priority_911)

priority_1 <- has_dates[E45_Disability_Priority_911 == 1]

summary_priority_1 <- summarize_scores_formatted(priority_1,
                                                 robust_measures = TRUE)
summary_priority_1

priority_2 <- has_dates[E45_Disability_Priority_911 == 2]

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
table(has_dates$E84_PostSecondary_Enrollment_911)

pse_enrolled <- has_dates[E84_PostSecondary_Enrollment_911 %in% c(1, 2, 3)]
summary_pse_enrolled <- summarize_scores_formatted(pse_enrolled,
                                                   robust_measures = TRUE)
summary_pse_enrolled

pse_not_enrolled <- has_dates[E84_PostSecondary_Enrollment_911 == 0]
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
table(has_dates$E86_PostSecondary_Partial_Completion_911)

pse_complete <- has_dates[E86_PostSecondary_Partial_Completion_911 == 1]
summary_pse_complete <- summarize_scores_formatted(pse_complete,
                                                   robust_measures = TRUE)
summary_pse_complete

pse_not_complete <- has_dates[E86_PostSecondary_Partial_Completion_911 %in%
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
hist(metadata_corrected$Age_At_Application)

ggplot(metadata_corrected, aes(x = Age_At_Application)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Age at Application",
    x = "Age",
    y = "Count"
  ) +
  scale_x_continuous(breaks = seq(0, max(metadata_corrected$Age_At_Application, na.rm = TRUE), by = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )


metadata_corrected$Min_Pre_Age

ggplot(metadata_corrected, aes(x = Min_Pre_Age)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Minimum Pre Score Age (TRT)",
    x = "Min Pre Score Age (TRT)",
    y = "Count"
  ) +
  scale_x_continuous(breaks = seq(0, max(metadata_corrected$Age_At_Application, na.rm = TRUE), by = 1)) +
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
  scale_x_continuous(breaks = seq(0, max(metadata_corrected$Age_At_Application, na.rm = TRUE), by = 1)) +
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
  scale_x_continuous(breaks = seq(0, max(metadata_corrected$Age_At_Application, na.rm = TRUE), by = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )


ggplot(metadata_corrected, aes(x = Median_Age)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
  labs(
    title = "Median Age in TRT",
    x = "Years",
    y = "Count"
  ) +
  scale_x_continuous(breaks = seq(0, max(metadata_corrected$Age_At_Application, na.rm = TRUE), by = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )



## Age across service
# library(ggplot2)
# library(patchwork)
# # Vector of your service variable names
# services <- c("CPSO", "CSS", "FL", "ILOM", "ISA", "JOBEX", "JS", "QWEX",
#               "WBLE", "WSS")
# # Create empty list to store plots
# plot_list <- list()
#
# # Generate each plot
# for (s in services) {
#   var_name <- paste0("Median_Age_", s)
#
#   p <- ggplot(metadata_corrected, aes_string(x = var_name)) +
#     geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
#     labs(
#       title = paste("Median Age -", s),
#       x = "Years",
#       y = "Count"
#     ) +
#     scale_x_continuous(
#       breaks = seq(0, max(metadata_corrected[[var_name]], na.rm = TRUE), by = 1)
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       plot.title = element_text(hjust = 0.5, face = "bold"),
#       axis.title = element_text(face = "bold")
#     )
#
#   plot_list[[s]] <- p
# }
#
# # Combine plots into a panel (e.g., 2 columns)
# final_plot <- wrap_plots(plot_list, ncol = 5)
# final_plot



## Age across service
library(ggplot2)
library(patchwork)

# Vector of your service variable names
services <- c("CPSO", "CSS", "FL", "ILOM", "ISA", "JOBEX", "JS", "QWEX",
              "WBLE", "WSS")

# Generate and store individual plots
plot_list <- list()

for (s in services) {
  var_name <- paste0("Median_Age_", s)

  p <- ggplot(metadata_corrected, aes_string(x = var_name)) +
    geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white", alpha = 0.8) +
    labs(
      title = paste(s),
      x = "Years",
      y = "Count"
    ) +
    scale_x_continuous(
      breaks = seq(0, max(metadata_corrected[[var_name]], na.rm = TRUE), by = 2)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )

  plot_list[[s]] <- p
}

# Combine all plots into a 2-row, 5-column layout
# Add overall title
wrap_plots(plot_list, ncol = 5) +
  plot_annotation(title = "Median Age Distribution by Service") &
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))


################################################################################
# Get summaries for Ages

library(dplyr)

age_columns <- metadata_corrected %>%
  select(starts_with("Median_Age"))

# Summarize each column
summary(age_columns)

table(metadata_corrected$Median_Age_CPSO)

table(metadata_corrected$Median_Age_CSS)


library(data.table)
library(stringr)


# Melt the data from wide to long
long_data <- melt(
  metadata_corrected,
  measure.vars = patterns(
    pre = "^Pre_Score_",
    post = "^Post_Score_",
    diff = "^Difference_"
  ),
  variable.name = "ServiceIndex"
)

# Extract service names from column names
services <- gsub("Pre_Score_", "", grep("^Pre_Score_",
                                        names(metadata_corrected),
                                        value = TRUE))
long_data[, Service := rep(services, each = nrow(metadata_corrected))]

# Add Age and summarize
summary_dt <- long_data[
  Median_Age %in% 14:22,
  .(
    Pre_Mean = mean(pre, na.rm = TRUE),
    Post_Mean = mean(post, na.rm = TRUE),
    Diff_Mean = mean(diff, na.rm = TRUE),
    N = .N
  ),
  by = .(Median_Age, Service)
]

summary_dt[order(Service, Median_Age)]




# Empty list to collect results
summary_list <- list()

# Loop over each service
for (srv in services) {
  # Dynamically generate column names
  pre_col <- paste0("Pre_Score_", srv)
  post_col <- paste0("Post_Score_", srv)
  diff_col <- paste0("Difference_", srv)
  age_col <- paste0("Median_Age_", srv)

  # Subset and rename
  temp <- metadata_corrected[, .SD, .SDcols = c(age_col, pre_col, post_col,
                                                diff_col)]
  setnames(temp, old = names(temp), new = c("Age", "Pre", "Post", "Diff"))

  # Remove rows with missing Age
  temp <- temp[!is.na(Age)]

  # Summarize by Age
  summary_dt <- temp[, .(
    Pre_Mean = round(mean(Pre, na.rm = TRUE), 2),
    Post_Mean = round(mean(Post, na.rm = TRUE), 2),
    Diff_Mean = round(mean(Diff, na.rm = TRUE), 2),
    N = .N
  ), by = Age]

  summary_dt[, Service := srv]
  summary_list[[srv]] <- summary_dt
}

# Combine all results
final_summary <- rbindlist(summary_list)

# Optional: order the result
setorder(final_summary, Service, Age)

final_summary





# Identify score columns
pre_vars <- grep("^Pre_Score_", names(metadata_corrected), value = TRUE)
post_vars <- grep("^Post_Score_", names(metadata_corrected), value = TRUE)
diff_vars <- grep("^Difference_", names(metadata_corrected), value = TRUE)

# Step 1: Calculate participant-level means for each score type
metadata_corrected[, Mean_Pre := rowMeans(.SD, na.rm = TRUE), .SDcols = pre_vars]
metadata_corrected[, Mean_Post := rowMeans(.SD, na.rm = TRUE), .SDcols = post_vars]
metadata_corrected[, Mean_Diff := rowMeans(.SD, na.rm = TRUE), .SDcols = diff_vars]

# Step 2: Group by age and average the participant-level means
summary_dt <- metadata_corrected[!is.na(Median_Age), .(
  Mean_Pre = mean(Mean_Pre, na.rm = TRUE),

  Mean_Post = mean(Mean_Post, na.rm = TRUE),

  Mean_Diff = mean(Mean_Diff, na.rm = TRUE)

), by = Median_Age][order(Median_Age)]

summary_dt




# Identify score columns
pre_vars <- grep("^Pre_Score_", names(metadata_corrected), value = TRUE)
post_vars <- grep("^Post_Score_", names(metadata_corrected), value = TRUE)
diff_vars <- grep("^Difference_", names(metadata_corrected), value = TRUE)

# Step 1: Calculate participant-level means for each score type
metadata_corrected[, Mean_Pre := rowMeans(.SD, na.rm = TRUE), .SDcols = pre_vars]
metadata_corrected[, Mean_Post := rowMeans(.SD, na.rm = TRUE), .SDcols = post_vars]
metadata_corrected[, Mean_Diff := rowMeans(.SD, na.rm = TRUE), .SDcols = diff_vars]

# Step 2: Group by age and calculate mean, sd, median
summary_dt <- metadata_corrected[!is.na(Median_Age), .(
  Mean_Pre = round(mean(Mean_Pre, na.rm = TRUE), 2),
  SD_Pre = round(sd(Mean_Pre, na.rm = TRUE), 2),
  Median_Pre = round(median(Mean_Pre, na.rm = TRUE), 2),

  Mean_Post = round(mean(Mean_Post, na.rm = TRUE), 2),
  SD_Post = round(sd(Mean_Post, na.rm = TRUE), 2),
  Median_Post = round(median(Mean_Post, na.rm = TRUE), 2),

  Mean_Diff = round(mean(Mean_Diff, na.rm = TRUE), 2),
  SD_Diff = round(sd(Mean_Diff, na.rm = TRUE), 2),
  Median_Diff = round(median(Mean_Diff, na.rm = TRUE), 2)
), by = Median_Age][order(Median_Age)]

summary_dt
