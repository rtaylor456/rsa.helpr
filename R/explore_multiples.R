library(tidyverse)

scores_old <- read.csv("data-raw/trt_utah_4_8_2024.csv")
View(scores_old)

overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_old$Participant.ID))


length(overlap_idx) # 322

scores_old_clean <- scores_old |>
  filter(Participant.ID %in% overlap_idx)

length(unique(scores_old_clean$Participant.ID))
View(scores_old_clean)

unique(scores_old_clean$Service)

# "CSS"   "FL"    "ISA"   "WBLE"  "CPSO"  "ILOM"  "JS"    "JOBEX" "QWEX"  "EMP"

scores_old_clean |>
  filter(Service == "EMP") |>
  nrow()

scores_old_clean |>
  filter(Service == "EMP") |>
  select(contains("q_")) |>
  lapply(function(x) sum(is.na(x)))

# CSS - 256 rows, q1-8
# FL - 241, q1-11
# ISA - 423, q1-10
# WBLE - 190, q1-8
# CPSO - 232, q1-14
# ILOM - 192, q1-7
# JS - 348, q1-9
# JOBEX - 414, q1-15
# QWEX - 69, q1-12
# EMP - 215, q1-9

multiple_rows <- scores_old_clean |>
  group_by(Participant.ID, Pre.Post, Service) |>
  summarise(count = n(), .groups = "drop") |>
  filter(count > 1) |>
  select(Participant.ID) |>
  pull()




filtered <- scores_old_clean |>
  filter(Participant.ID %in% multiple_rows) |>
  arrange(Participant.ID)

View(filtered)

subset_list <- list()
for (service in scores_old_clean$Service) {
  subset_list[[service]] <- scores_old_clean %>% filter(Service == service)
}

names(subset_list)

participants_with_multiple_scores <- list()
for (service in names(subset_list)) {
  participants_with_multiple_scores[[service]] <- subset_list[[service]] |>
    group_by(Participant.ID, Pre.Post) |>
    summarise(count = n(), .groups = 'drop') |>
    filter(count > 1) |>
    select(Participant.ID) |>
    distinct()
}


data_with_multiple_scores <- list()
for (service in names(subset_list)) {
  data_with_multiple_scores[[service]] <- subset_list[[service]] |>
    group_by(Participant.ID, Pre.Post) |>
    mutate(count = n()) |>
    filter(count > 1) |>
    mutate(Completed = mdy_hms(gsub(" \\(MST\\)", "", Completed))) |>
    arrange(Participant.ID, Completed)
}

names(data_with_multiple_scores)
# "CSS"   "FL"    "ISA"   "WBLE"  "CPSO"  "ILOM"  "JS"    "JOBEX" "QWEX"  "EMP"

data_with_multiple_scores[["CSS"]]
View(data_with_multiple_scores[["ISA"]])

pivoted_data <- data_with_multiple_scores %>%
  lapply(function(df) {
    pivot_wider(df, names_from = Pre.Post, values_from = Score,
              names_prefix = "Score_")
    }
    )

# try <- pivot_wider(data_with_multiple_scores[["ISA"]],
#                    names_from = Pre.Post, values_from = Score,
#                    names_prefix = "Score_")

View(pivoted_data[["ISA"]])

plot(x = unlist(Completed),
     y = unlist(Score_Post),
     data = pivoted_data[["ISA"]])

ggplot(pivoted_data[["ISA"]], aes(x = unlist(Completed), y = unlist(Score_Post))) +
  geom_point() +
  labs(title = "Post scores across time",
       x = "Date when completed",
       y = "Post scores") +
  theme_minimal()



