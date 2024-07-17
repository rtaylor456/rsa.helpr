library(tidyverse)

scores_old <- read.csv("data-raw/trt_utah_4_8_2024.csv")
# View(scores_old)

overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_old$Participant.ID))


# length(overlap_idx) # 322

scores_old_clean <- scores_old |>
  filter(Participant.ID %in% overlap_idx)

# length(unique(scores_old_clean$Participant.ID))
# View(scores_old_clean)

# unique(scores_old_clean$Service)

# "CSS"   "FL"    "ISA"   "WBLE"  "CPSO"  "ILOM"  "JS"    "JOBEX" "QWEX"  "EMP"

# scores_old_clean |>
#   filter(Service == "EMP") |>
#   nrow()
#
# scores_old_clean |>
#   filter(Service == "EMP") |>
#   select(contains("q_")) |>
#   lapply(function(x) sum(is.na(x)))

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

# multiple_rows <- scores_old_clean |>
#   group_by(Participant.ID, Pre.Post, Service) |>
#   summarise(count = n(), .groups = "drop") |>
#   filter(count > 1) |>
#   select(Participant.ID) |>
#   pull()




# filtered <- scores_old_clean |>
#   filter(Participant.ID %in% multiple_rows) |>
#   arrange(Participant.ID)

# View(filtered)


subset_list <- list()
for (service in scores_old_clean$Service) {
  subset_list[[service]] <- scores_old_clean %>% filter(Service == service)
}

# names(subset_list)

participants_with_multiple_scores <- list()
for (service in names(subset_list)) {
  participants_with_multiple_scores[[service]] <- subset_list[[service]] |>
    group_by(Participant.ID, Pre.Post) |>
    summarise(count = n(), .groups = 'drop') |>
    filter(count > 1) |>
    select(Participant.ID) |>
    distinct()
}

# find the number of unique participants across all tests
# all_participants <- unlist(lapply(participants_with_multiple_scores,
#                                   function(df) df$Participant.ID))
# total_unique_participants <- length(unique(all_participants))
#
# total_unique_participants



data_with_multiple_scores <- list()
for (service in names(subset_list)) {
  data_with_multiple_scores[[service]] <- subset_list[[service]] |>
    group_by(Participant.ID, Pre.Post) |>
    mutate(count = n()) |>
    filter(count > 1) |>
    mutate(Completed = mdy_hms(gsub(" \\(MST\\)", "", Completed))) |>
    arrange(Participant.ID, Completed)
}

# names(data_with_multiple_scores)
# "CSS"   "FL"    "ISA"   "WBLE"  "CPSO"  "ILOM"  "JS"    "JOBEX" "QWEX"  "EMP"

# data_with_multiple_scores[["CSS"]]
# View(data_with_multiple_scores[["ISA"]])
# obs 27-28 are identical--same Completed time too--this is why the code was
#  creating a list for Score_Pre and Score_Post

pivoted_data <- data_with_multiple_scores |>
  lapply(distinct) |>
  lapply(function(df) {
    pivot_wider(df, names_from = Pre.Post, values_from = Score,
              names_prefix = "Score_")
    }
    )

# get the distribution of providers for each test
pivoted_data |> lapply(function(df) pull(select(df, Provider))) |>
  lapply(table)

# try <- pivot_wider(data_with_multiple_scores[["ISA"]],
#                    names_from = Pre.Post, values_from = Score,
#                    names_prefix = "Score_")

# View(pivoted_data[["ISA"]])

# plot(x = unlist(Completed),
#      y = unlist(Score_Post),
#      data = pivoted_data[["ISA"]])

# ggplot(pivoted_data[["ISA"]], aes(x = unlist(Completed), y = unlist(Score_Post))) +
#   geom_point() +
#   labs(title = "Post scores across time",
#        x = "Date when completed",
#        y = "Post scores") +
#   theme_minimal()
#

# isa <- as.data.frame(pivoted_data[["ISA"]]) |>
#   filter(Participant.ID != 118320) |>
#   mutate(Participant.ID = as.factor(Participant.ID))
#
# plot(isa$Completed, isa$Score_Post)


# Create the line plot
# ggplot(isa, aes(x = Completed)) +
#   geom_line(aes(y = Score_Pre, color = Participant.ID,
#                 linetype = "Pre")) +
#   geom_point(aes(y = Score_Pre, color = Participant.ID)) +
#   geom_line(aes(y = Score_Post, color = Participant.ID,
#                 linetype = "Post")) +
#   geom_point(aes(y = Score_Post, color = Participant.ID,
#                  shape = "Post")) +
#   labs(title = "Trend of Scores Over Time for Each Participant",
#        x = "Time/Order of Test",
#        y = "Score",
#        color = "Participant",
#        linetype = "Score Type",
#        shape = "Score Type") +
#   theme_minimal()
#
# length(unique(isa$Participant.ID))


# PLOTS for difference scores

# names(pivoted_data)
# [1] "CSS"   "FL"    "ISA"   "WBLE"  "CPSO"  "ILOM"  "JS"    "JOBEX" "QWEX"
# [10] "EMP"

library(ggplot2)
# library(RColorBrewer)

# CSS
css_plot <- as.data.frame(pivoted_data[["CSS"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "CSS Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
css_plot



# FL
fl_plot <- as.data.frame(pivoted_data[["FL"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "FL Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fl_plot


# ISA
isa_plot <- as.data.frame(pivoted_data[["ISA"]]) |>
  filter(Participant.ID != 118320) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "ISA Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
isa_plot


# WBLE
wble_plot <- as.data.frame(pivoted_data[["WBLE"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "WBLE Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "3 day", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
wble_plot

# View(as.data.frame(pivoted_data[["WBLE"]]))


# CPSO
cpso_plot <- as.data.frame(pivoted_data[["CPSO"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "CPSO Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
cpso_plot



# ILOM
ilom_plot <- as.data.frame(pivoted_data[["ILOM"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "ILOM Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal()+
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ilom_plot


# JS
js_plot <- as.data.frame(pivoted_data[["JS"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "JS Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
js_plot


# JOBEX
jobex_plot <- as.data.frame(pivoted_data[["JOBEX"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "JOBEX Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
jobex_plot


# QWEX - No pre scores, so no difference scores

# EMP
emp_plot <- as.data.frame(pivoted_data[["EMP"]]) |>
  filter(!is.na(Difference)) |>
  mutate(Participant.ID = as.factor(Participant.ID)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = Difference, color = Participant.ID)) +
  geom_point(aes(y = Difference, color = Participant.ID)) +
  labs(title = "EMP Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
emp_plot


#

