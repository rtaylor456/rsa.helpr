library(tidyverse)
scores_old <- read.csv("data-raw/trt_utah_4_8_2024.csv")

overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_old$Participant.ID))

providers_use <- c("CCC (9)", "LSI (8)", "SPA (28)",
                   "SUU (27)", "Turn (29)")


able(metadata_clean2$Provider)

scores_old_clean <- scores_old |>
  filter(Participant.ID %in% overlap_idx) |>
  filter(Provider %in% providers_use)


providers_use2 <- c("CCC", "LSI", "SPA", "SUU", "Turn")
metadata_clean2 <- metadata |>
  filter(!is.na(Median_Difference_Score)) |>
  filter(Provider %in% providers_use2)

# Drop unused levels
metadata_clean2$Provider <- droplevels(metadata_clean2$Provider)

overall_median <- median(metadata_clean2$Median_Difference_Score)
overall_median # 14.29

boxplot(Median_Difference_Score ~ Provider,
        data = metadata_clean2,
        main = "Median Difference Scores Across Providers",
        ylab = "Median Difference Score")
abline(h = overall_median, lty = 2, col = "blue")



subset_provider <- list()
for (provider in scores_old_clean$Provider) {
  subset_provider[[provider]] <- scores_old_clean |>
    filter(Provider == provider)
}


participants_with_multiple_scores <- list()
for (provider in names(subset_provider)) {
  participants_with_multiple_scores[[provider]] <- subset_provider[[provider]] |>
    group_by(Participant.ID, Pre.Post) |>
    summarise(count = n(), .groups = 'drop') |>
    filter(count > 1) |>
    select(Participant.ID) |>
    distinct()
}


names(subset_provider)
# [1] "LSI (8)"   "SUU (27)"  "CCC (9)"   "Turn (29)" "SPA (28)"

names(participants_with_multiple_scores)
# [1] "LSI (8)"   "SUU (27)"  "CCC (9)"   "Turn (29)" "SPA (28)"

data_with_multiple_scores <- list()
for (provider in names(subset_provider)) {
  data_with_multiple_scores[[provider]] <- subset_provider[[provider]] |>
    group_by(Participant.ID, Pre.Post) |>
    mutate(count = n()) |>
    filter(count > 1) |>
    mutate(Completed = mdy_hms(gsub(" \\(MST\\)", "", Completed))) |>
    arrange(Participant.ID, Completed)
}

names(data_with_multiple_scores)
# [1] "LSI (8)"   "SUU (27)"  "CCC (9)"   "Turn (29)" "SPA (28)"

pivoted_data <- data_with_multiple_scores |>
  lapply(distinct) |>
  lapply(function(df) {
    pivot_wider(df, names_from = Pre.Post, values_from = Score,
                names_prefix = "Score_")
  }
  )

# get the distribution of providers for each test
pivoted_data |> lapply(function(df) pull(select(df, Service))) |>
  lapply(table)

# $`LSI (8)`
#
# CPSO   CSS   EMP    FL  ILOM   ISA JOBEX    JS  QWEX  WBLE
# 62    91    56   105    67   165   136   150    27    68
#
# $`SUU (27)`
#
# CPSO   CSS   EMP    FL  ILOM   ISA JOBEX    JS  QWEX  WBLE
# 29     5     5    11     5    42    44    19     4    13
#
# $`CCC (9)`
#
# CPSO   CSS   EMP    FL  ILOM   ISA JOBEX    JS  QWEX  WBLE
# 12    23    19     8    10    39    35    38     1     5
#
# $`Turn (29)`
#
# CPSO   CSS   EMP    FL  ILOM   ISA JOBEX    JS  QWEX  WBLE
# 71    76    74    67    68    70    82    72    19    50
#
# $`SPA (28)`
#
# CPSO   CSS   EMP    FL  ILOM   ISA JOBEX    JS  QWEX  WBLE
# 16    19    22    14    10    22    31    20     7    32



library(ggplot2)
# library(RColorBrewer)


# LSI

lsi <- as.data.frame(pivoted_data[["LSI (8)"]])

lsi_pre_median <- median(lsi$Score_Pre, na.rm = TRUE)
lsi_pre_median # 55.56

boxplot(Score_Pre ~ Service,
        data = lsi,
        main = "LSI Provider Pre Scores Across Services (Test Type)",
        ylab = "Pre Score")
abline(h = lsi_pre_median, lty = 2, col = "blue")


lsi_post_median <- median(lsi$Score_Post, na.rm = TRUE)
lsi_post_median # 55.56

boxplot(Score_Post ~ Service,
        data = lsi,
        main = "LSI Provider Post Scores Across Services (Test Type)",
        ylab = "Post Score")
abline(h = lsi_pre_median, lty = 2, col = "blue")
abline(h = lsi_post_median, lty = 2, col = "red")


lsi_diff_median <- median(lsi$Difference, na.rm = TRUE)
lsi_diff_median #  18.52

boxplot(Difference ~ Service,
        data = lsi,
        main = "lsi Provider Difference Scores Across Services (Test Type)",
        ylab = "Difference Score")
abline(h = lsi_diff_median, lty = 2, col = "blue")




# SUU

suu <- as.data.frame(pivoted_data[["SUU (27)"]])

suu_pre_median <- median(suu$Score_Pre, na.rm = TRUE)
suu_pre_median # 57.68

boxplot(Score_Pre ~ Service,
        data = suu,
        main = "suu Provider Pre Scores Across Services (Test Type)",
        ylab = "Pre Score")
abline(h = suu_pre_median, lty = 2, col = "blue")


suu_post_median <- median(suu$Score_Post, na.rm = TRUE)
suu_post_median # 73.33

boxplot(Score_Post ~ Service,
        data = suu,
        main = "suu Provider Post Scores Across Services (Test Type)",
        ylab = "Post Score")
abline(h = suu_pre_median, lty = 2, col = "blue")
abline(h = suu_post_median, lty = 2, col = "red")


suu_diff_median <- median(suu$Difference, na.rm = TRUE)
suu_diff_median #  20

boxplot(Difference ~ Service,
        data = suu,
        main = "suu Provider Difference Scores Across Services (Test Type)",
        ylab = "Difference Score")
abline(h = suu_diff_median, lty = 2, col = "blue")





# CCC (9)

ccc <- as.data.frame(pivoted_data[["CCC (9)"]])

ccc_pre_median <- median(ccc$Score_Pre, na.rm = TRUE)
ccc_pre_median # 51.48

boxplot(Score_Pre ~ Service,
        data = ccc,
        main = "ccc Provider Pre Scores Across Services (Test Type)",
        ylab = "Pre Score")
abline(h = ccc_pre_median, lty = 2, col = "blue")


ccc_post_median <- median(ccc$Score_Post, na.rm = TRUE)
ccc_post_median # 73.33

boxplot(Score_Post ~ Service,
        data = ccc,
        main = "ccc Provider Post Scores Across Services (Test Type)",
        ylab = "Post Score")
abline(h = ccc_pre_median, lty = 2, col = "blue")
abline(h = ccc_post_median, lty = 2, col = "red")


ccc_diff_median <- median(ccc$Difference, na.rm = TRUE)
ccc_diff_median #  19.05

boxplot(Difference ~ Service,
        data = ccc,
        main = "ccc Provider Difference Scores Across Services (Test Type)",
        ylab = "Difference Score")
abline(h = ccc_diff_median, lty = 2, col = "blue")




# Turn (29)

turn <- as.data.frame(pivoted_data[["Turn (29)"]])

turn_pre_median <- median(turn$Score_Pre, na.rm = TRUE)
turn_pre_median # 70.83

boxplot(Score_Pre ~ Service,
        data = turn,
        main = "turn Provider Pre Scores Across Services (Test Type)",
        ylab = "Pre Score")
abline(h = turn_pre_median, lty = 2, col = "blue")


turn_post_median <- median(turn$Score_Post, na.rm = TRUE)
turn_post_median # 80.95

boxplot(Score_Post ~ Service,
        data = turn,
        main = "turn Provider Post Scores Across Services (Test Type)",
        ylab = "Post Score")
abline(h = turn_pre_median, lty = 2, col = "blue")
abline(h = turn_post_median, lty = 2, col = "red")


turn_diff_median <- median(turn$Difference, na.rm = TRUE)
turn_diff_median #  12.915

boxplot(Difference ~ Service,
        data = turn,
        main = "turn Provider Difference Scores Across Services (Test Type)",
        ylab = "Difference Score")
abline(h = turn_diff_median, lty = 2, col = "blue")



# SPA (28)

spa <- as.data.frame(pivoted_data[["SPA (28)"]])

spa_pre_median <- median(spa$Score_Pre, na.rm = TRUE)
spa_pre_median # 55.16

boxplot(Score_Pre ~ Service,
        data = spa,
        main = "spa Provider Pre Scores Across Services (Test Type)",
        ylab = "Pre Score")
abline(h = spa_pre_median, lty = 2, col = "blue")


spa_post_median <- median(spa$Score_Post, na.rm = TRUE)
spa_post_median # 54.17

boxplot(Score_Post ~ Service,
        data = spa,
        main = "spa Provider Post Scores Across Services (Test Type)",
        ylab = "Post Score")
abline(h = spa_pre_median, lty = 2, col = "blue")
abline(h = spa_post_median, lty = 2, col = "red")


spa_diff_median <- median(spa$Difference, na.rm = TRUE)
spa_diff_median #  0.15

boxplot(Difference ~ Service,
        data = spa,
        main = "spa Provider Difference Scores Across Services (Test Type)",
        ylab = "Difference Score")
abline(h = spa_diff_median, lty = 2, col = "blue")





lsi_plot <- as.data.frame(pivoted_data[["LSI (8)"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "LSI Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
lsi_plot

