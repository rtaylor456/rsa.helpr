library(tidyverse)

# uncleaned, character, just to do basic check
table(data$`Age at Application`)

# see individual counts
table(as.numeric(data$`Age at Application`))

# numeric summary
summary(as.numeric(data$`Age at Application`))

# plot
hist(as.numeric(data$`Age at Application`),
     main = "Age at Application, raw data", xlab = "Age in Years")

# look at data to see what's going on
data |> mutate(E7_Application_Date_911 = rsa.helpr::handle_excel_date(
  E7_Application_Date_911)) |>
  select(Participant_ID, E7_Application_Date_911 ,
               `Age at Application`, E74_SWD_Age_911) |>
  filter(`Age at Application` > 22) |>
  View()


library(tidyverse)

# Filter metadata and select Participant_ID values where Age_at_Application > 22
metadata |>
  filter(Age_At_Application > 22) |>
  pull(Participant_ID)

# [1] 11100  11885  71108  109122 113478 115894 121299 121742 123446 126377
#    130506 132387 133406 134382

data |> mutate(age = as.numeric(`Age at Application`)) |>
  filter(age > 22) |>
  pull(Participant_ID) |>
  unique() |>
  length()

# 28,335 unique participants in raw data who are over 22

data_clean |> filter(Age_At_Application > 22) |>
  pull(Participant_ID) |>
  unique() |>
  length()

# 26,797 unique participants in cleaned full RSA-911 data who are over 22

merged |> filter(Age_At_Application > 22) |>
  pull(Participant_ID) |>
  unique() |>
  length()
# 14

metadata |> filter(Age_At_Application > 22) |>
  select(E7_Application_Date_911, Participant_ID, Age_At_Application,
         E74_SWD_Age_911, E77_Plan_Grade_Level_911,
         E22_SWD_911) |>
  View()

check <- metadata |> filter(Age_At_Application > 22) |>
  select(E7_Application_Date_911, Participant_ID, Age_At_Application,
         E74_SWD_Age_911, E77_Plan_Grade_Level_911,
         E22_SWD_911)
check$Participant_ID

################################################################################

# temporary method for correcting age variable to only contain proper range
data_corrected_age <- metadata[Age_At_Application >= 14 &
                                 Age_At_Application <= 22, ]


# temporary method for correcting non-integer ages (created from taking medians
#   in metadata process)
data_corrected_age$Age_At_Application <- ceiling(
  data_corrected_age$Age_At_Application)



# Compare distributions for ages
# age_14 <- data_corrected_age[Age_At_Application == 14]
# summary_age_14 <- summarize_scores_formatted(age_14,
#                                                    robust_measures = TRUE)
# summary_age_14

# To get summaries for each service across each age
age_summaries <- lapply(14:22, function(age) {
  age_subset <- data_corrected_age[Age_At_Application == age]
  summarize_scores_formatted(age_subset, robust_measures = TRUE)
})

# Optionally, name each list element by the age
names(age_summaries) <- paste0("age_", 14:22)

age_summaries


# median_score_age_counts <- lapply(14:22, function(age) {
#   age_subset <- data_corrected_age[Age_At_Application == age]
#   length(age_subset$Median_Difference_Score)
# })
# names(median_score_age_counts) <- paste0("age_", 14:22)
# median_score_age_counts


# To get summaries for median difference score across each age
median_diff_score_age <- lapply(14:22, function(age) {
  age_subset <- data_corrected_age[Age_At_Application == age]
  # summary(age_subset$Median_Difference_Score)

  summarize_column(age_subset$Median_Difference_Score)
})
names(median_diff_score_age) <- paste0("age_", 14:22)
median_diff_score_age


# To get summaries for median pre score across each age
median_pre_score_age <- lapply(14:22, function(age) {
  age_subset <- data_corrected_age[Age_At_Application == age]
  # summary(age_subset$Median_Pre_Score)

  summarize_column(age_subset$Median_Pre_Score)
})
names(median_pre_score_age) <- paste0("age_", 14:22)
median_pre_score_age


# To get summaries for median post score across each age
median_post_score_age <- lapply(14:22, function(age) {
  age_subset <- data_corrected_age[Age_At_Application == age]
  # summary(age_subset$Median_Post_Score)

  summarize_column(age_subset$Median_Post_Score)
})
names(median_post_score_age) <- paste0("age_", 14:22)
median_post_score_age

# Compare to overall summaries
summarize_column(data_corrected_age$Median_Difference_Score)
summarize_column(data_corrected_age$Median_Pre_Score)
summarize_column(data_corrected_age$Median_Post_Score)

