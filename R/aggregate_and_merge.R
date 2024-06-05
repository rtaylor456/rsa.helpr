# read in data
data <- readRDS("data-raw/data_merged.rds")
dim(data)
# preliminary cleaning
data_clean <- utah_clean(data)
View(data_clean)
dim(data_clean)

# aggregate the data--removing duplicate rows for the same id in a single
#   quarter
data_aggregate <- data_clean |>
  group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(occurences_per_quarter = n()) |>
  slice(which.max(E7_Application_Date_911)) |>
  ungroup()

# View(data_aggregate)
check <- data_aggregate |>
  select(Participant_ID, E1_Year_911, E2_Quarter_911, occurences_per_quarter) |>
  filter(E2_Quarter_911 == 1)

View(check)

dim(data_aggregate)


# merge the pre-post score data to our dataset
scores <- read.csv("data-raw/trt_utah_6_3_2024_4pm.csv")
View(scores)

# we should expect to have this many complete unique ids after merging...
length(intersect(unique(data_aggregate$Participant_ID),
                 unique(scores$Participant.ID)))
# 329? why is it so reduced after aggregating

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
