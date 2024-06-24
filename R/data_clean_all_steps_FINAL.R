library(tidyverse)

data <- readRDS("data-raw/data_merged.rds")

data_clean <- utah_clean(data)

data_aggregate <- data_clean |>
  # select(-X) |> # only need this when using stored csv file
  # remove the rows that don't contain an application date --these are pretty
  #   empty anyway
  filter(!is.na(E7_Application_Date_911)) |>
  group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(occurrences_per_quarter = n()) |>
  arrange(E7_Application_Date_911) |>
  slice(1) |>
  # for some reason, this stopped working...
  # slice(which.max(E7_Application_Date_911)) |>
  ungroup()

# read in scores dataset
scores_clean <- read.csv("data-raw/scores_clean.csv")

# see how many overlapping participants we have
overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_clean$Participant.ID))

length(overlap_idx) # 329
# so, 329 observations that contain demographic/info data (data_aggregate info)
#   and pre-post scores

data_merged <- merge(data_aggregate, scores_clean,
                     by.x = "Participant_ID",
                     by.y = "Participant.ID",
                     all = FALSE) # all = TRUE adds all rows

# check that we end up with 329 unique IDs
length(unique(data_merged$Participant_ID)) # 329

# create a function to get the most common values (modes) for my factor
#   variables
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

library(lubridate)

metadata <- data_merged |>
  mutate(Provider = as.factor(Provider)) |>
  group_by(Participant_ID) |>
  # create enrollment length variable
  mutate(Overall_Quarter = ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)) |>
  mutate(Min_Overall_Quarter = min(Overall_Quarter),
         Max_Overall_Quarter = max(Overall_Quarter)) |>
  mutate(Enroll_Length = Max_Overall_Quarter -
           Min_Overall_Quarter + 1) |>
  # create variables that count the number of years and quarters and then remove
  #   the year and quarter columns so we can condense the data
  arrange(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(
    Total_Years = n_distinct(E1_Year_911),
    Total_Quarters = n_distinct(E2_Quarter_911)
  ) |>
  select(-c(E1_Year_911, E2_Quarter_911)) |>
  # convert the rest of numeric variables to medians
  mutate(across(where(is.numeric), ~ median(., na.rm = TRUE))) |>

  # handle date variables
  mutate(across(where(lubridate::is.Date),
                ~ as.Date(ifelse(all(is.na(unique(.))), NA,
                                 max(., na.rm = TRUE)))
  )) |>
  # handle factor variables--keep only the most common values for each
  #   participant
  mutate(across(where(is.factor), ~ as.factor(get_mode(.)))) |>
  ungroup() |>

  # Summarise to condense rows, keeping one row per participant
  group_by(Participant_ID) |>
  summarise(across(everything(), first)) |>

  ungroup()

nrow(metadata) # 329
length(unique(metadata$Participant_ID)) # 329

View(metadata)


scores2 <- read.csv("data-raw/trt_data_4_8_2024.csv")

View(scores2)

