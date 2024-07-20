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
# scores_clean <- read.csv("data-raw/scores_clean.csv")

scores <- read.csv("data-raw/trt_utah_6_3_2024_4pm.csv")
scores$Completed <- mdy_hms(gsub(" \\(MST\\)", "", scores$Completed))
# Group by participant.id, service, and pre.post, and count the scores
scores2 <- scores |>
  group_by(Participant.ID, Service, Pre.Post) |>
  mutate(count = n()) |>
  ungroup() |>
  mutate(Has_Multiple_Scores = as.factor(if_else(count > 1, 1, 0))) |>
  select(-count) |>
  group_by(Participant.ID, Service) |>
  mutate(Time_Passed_Days = as.numeric(difftime(max(Completed),
                                                min(Completed),
                                                units = "days"))) |>
  mutate(Time_Passed_Days = round(Time_Passed_Days)) |>
  # mutate(Time_Passed_Days = format(Time_Passed_Days, scientific = FALSE)) |>
  ungroup() |>
  arrange(Participant.ID)

pre_data <- scores2 |>
  filter(Pre.Post == "Pre")

post_data <- scores2 |>
  filter(Pre.Post == "Post")

# Keep the earliest 'Pre' score and most recent 'Post' score
earliest_pre <- pre_data |>
  arrange(Completed) |>
  group_by(Participant.ID, Provider, Service, Completed) |>
  slice(1)
# View(earliest_pre)

latest_post <- post_data |>
  arrange(desc(Completed)) |>
  group_by(Participant.ID, Provider, Service) |>
  slice(1)
# View(latest_post)

merged_data <- full_join(
  earliest_pre %>%
    select(Participant.ID,
           Completed_Pre = Completed,
           Service, Provider,
           Has_Multiple_Scores,
           Time_Passed_Days,
           Pre_Score = Score),
  latest_post %>%
    select(Participant.ID,
           Completed_Post = Completed,
           Service, Provider,
           Has_Multiple_Scores,
           Time_Passed_Days,
           Post_Score = Score,
           Difference),
  by = c("Participant.ID", "Service", "Provider",
         "Has_Multiple_Scores", "Time_Passed_Days")
)

scores_final <- merged_data %>%
  pivot_wider(
    names_from = Service,
    values_from = c(Pre_Score, Post_Score, Difference, Time_Passed_Days),
    names_glue = "{.value}_{Service}"
  )

# see how many overlapping participants we have
overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_final$Participant.ID))

length(overlap_idx) # 329
# so, 329 observations that contain demographic/info data (data_aggregate info)
#   and pre-post scores

data_merged <- merge(data_aggregate, scores_final,
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
  # mutate(Provider = as.factor(Provider)) |>
  mutate(across(c(Provider, Has_Multiple_Scores), ~ as.factor(.))) |>
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

  # calculate
  rowwise() |>
  mutate(
    Differences_Available = sum(!is.na(c_across(starts_with("Difference_") ))),
    Median_Difference_Score = median(c_across(starts_with("Difference_")),
                                     na.rm = TRUE),
    Median_Time_Passed_Days = median(c_across(starts_with("Time_Passed_Days")),
                                     na.rm = TRUE)
  ) |>
  ungroup() |>

  # Summarise to condense rows, keeping one row per participant
  group_by(Participant_ID) |>
  summarise(across(everything(), first)) |>

  ungroup() |>
  # remove columns where all values are NA
  select(where(~ !all(is.na(.)))) |>
  # apply my handmade function to handle disability columns
  separate_disability()


nrow(metadata) # 329
length(unique(metadata$Participant_ID)) # 329

View(metadata)
names(metadata)

# scores2 <- read.csv("data-raw/trt_data_4_8_2024.csv")


