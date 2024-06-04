data <- readRDS("data-raw/data_merged.rds")

scores1 <- read.csv("data-raw/trt_utah_4_8_2024.csv")
scores2 <- read.csv("data-raw/trt_utah_6_3_2024_4pm.csv")

library(tidyverse)

scores1_ids <- scores1 |>
  select(Participant.ID) |>
  unique() |>
  pull()

scores2_ids <- scores2 |>
  select(Participant.ID) |>
  unique() |>
  pull()
length(scores2_ids)

length(intersect(scores1_ids, scores2_ids))
length(setdiff(scores2_ids, scores1_ids))
  # so it looks like there are only 135 new data points

class(data$Participant_ID)
class(scores1$Participant.ID)
class(scores2$Participant.ID)

length(intersect(unique(data$Participant_ID), unique(scores2$Participant.ID)))
# 1387
#  there are 1387 unique participants with pre-post scores in our original data
#  there are 1807 total participants with pre-post scores in the scores data

# merge the datasets
data_merged <- merge(data, scores2,
                     by.x = "Participant_ID",
                     by.y = "Participant.ID",
                     all = FALSE) # all = TRUE adds all rows

View(data_merged)

# find the number of rows where we have data from both the original dataset and
#   the new scores dataset
complete_rows <- data_merged[complete.cases(data_merged$Pre.Post,
                                            data_merged$E7_Application_Date_911), ]
nrow(complete_rows)

complete_rows |> select(Participant_ID) |> unique() |> pull() |> length()
# 1387
# we have added in the 1387 participants in our original dataset for which we
#   had pre-post scores

data_merged2 <- data |>
  group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(occurrences_per_quarter = n()) |>
  # select(Participant_ID, E1_Year_911, E2_Quarter_911, E7_Application_Date_911,
  #        occurrences_per_quarter) |>

  # convert date variables to actual date representation, as this is the
  #    raw data
  mutate_at(vars(matches("E7_Application_Date_911")),
            ~as.Date(as.numeric(.), origin = "1899-12-30")) |>
  slice(which.max(E7_Application_Date_911)) |>
  ungroup() |>
  # arrange(Participant_ID, E1_Year_911, E2_Quarter_911,
  #         desc(E7_Application_Date_911)) |>
  # slice_head(n = 1) |>
  select(Participant_ID, E1_Year_911, E2_Quarter_911, E7_Application_Date_911,
         occurrences_per_quarter)

View(data_merged2)


library(data.table)

# Assuming `data` is already a data.table
data <- as.data.table(data)

# Perform the operations in data.table
data_merged2 <- data[, .(
  occurrences_per_quarter = .N,
  E7_Application_Date_911 = as.Date(as.numeric(E7_Application_Date_911), origin = "1899-12-30")
), by = .(Participant_ID, E1_Year_911, E2_Quarter_911)][
  order(Participant_ID, E1_Year_911, E2_Quarter_911, -E7_Application_Date_911)
][
  , .SD[1], by = .(Participant_ID, E1_Year_911, E2_Quarter_911)
][
  , .(Participant_ID, E1_Year_911, E2_Quarter_911, E7_Application_Date_911, occurrences_per_quarter)
]

View(data_merged2)




