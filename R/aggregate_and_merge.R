# read in data
data <- readRDS("data-raw/data_merged.rds")
dim(data)
# preliminary cleaning
data_clean <- utah_clean(data)
View(data_clean)
dim(data_clean)

# aggregate the data--removing duplicate rows for the same id in a single
#   quarter
check <- data_clean |>
  filter(!is.na(E7_Application_Date_911))
nrow(data_clean)
length(unique(data_clean$Participant_ID))
nrow(check)
length(unique(check$Participant_ID)) # so, we are missing some application dates
#   we will have to account for this in our aggregation so we don't remove these
# 32694 -- so, this is the number of unique participants we expect to see in our
#   aggregated dataset

check2 <- data_clean |>
  filter(is.na(E7_Application_Date_911)) |>
  group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(occurences_per_quarter = n()) |>
  # select(Participant_ID, E1_Year_911, E2_Quarter_911, E7_Application_Date_911,
  #        occurences_per_quarter, E398_Plan_Date_911) |>
  arrange(Participant_ID, E1_Year_911, E2_Quarter_911)
View(check2)

write.csv(check2, file = "NA_app_dates.csv")

# these data points that don't have application dates have no real information,
#  so we should actually be fine to just leave them out

data_aggregate <- data_clean |>
  group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(occurences_per_quarter = n()) |>
  slice(which.max(E7_Application_Date_911)) |>
  ungroup()

dim(data_aggregate)
length(unique(data_aggregate$Participant_ID)) # 32694 --this is what we expected

# View(data_aggregate)
check <- data_aggregate |>
  select(Participant_ID, E1_Year_911, E2_Quarter_911, occurences_per_quarter) |>
  filter(E1_Year_911 == 2021)

View(check)

dim(data_aggregate)


# merge the pre-post score data to our dataset
scores <- read.csv("data-raw/trt_utah_6_3_2024_4pm.csv")
# old_scores <- read.csv("data-raw/trt_utah_4_8_2024.csv")
View(scores)

scores_sort <- scores |>
  group_by(Participant.ID, Service) |>
  arrange(Participant.ID, Service)
View(scores_sort)

# Pivot the DataFrame
scores_wide <- scores |>
  pivot_wider(names_from = Pre.Post, values_from = Score,
              names_prefix = "Score_") |>
  arrange(Participant.ID, Service)
View(scores_wide)



nrow(scores)
length(scores$Participant.ID)
length(unique(scores$Participant.ID))

# we should expect to have this many complete unique ids after merging...
length(intersect(unique(data_aggregate$Participant_ID),
                 unique(scores_wide$Participant.ID)))
# 329? why is it so reduced after aggregating

overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_wide$Participant.ID))

dates_data <- data_aggregate |>
  filter(Participant_ID %in% overlap_idx)|>
  select(contains("date"))
View(dates_data)

# using the pre-aggregated data
length(intersect(unique(data_clean$Participant_ID),
                 unique(scores$Participant.ID)))
# 1387 -- so this is the real overlap between ids?

data_merged <- merge(data_aggregate, scores,
                     by.x = "Participant_ID",
                     by.y = "Participant.ID",
                     all = FALSE) # all = TRUE adds all rows

data_merged <- rsa_merge(data_aggregate, scores)
dim(data_merged) # 15,294 rows--only 329 unique participants? because there are
# participants in the pre-post who are not in the utah data?


View(data_merged)

data_merged |>
  select(contains("provide"), Difference)

data_merged[1:5,345:353]
