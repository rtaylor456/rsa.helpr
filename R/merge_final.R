scores_clean <- read.csv("data-raw/scores_clean.csv")
View(scores_clean)

data_clean <- read.csv("data-raw/data_clean.csv")
View(data_clean)

data_aggregate <- data_clean |>
  # select(-X) |>
  # remove the rows that don't contain an application date --these are pretty
  #   empty anyway
  filter(!is.na(E7_Application_Date_911)) |>
  group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(occurences_per_quarter = n()) |>
  arrange(E7_Application_Date_911) |>
  slice(1) |>
  # for some reason, this stopped working...
  # slice(which.max(E7_Application_Date_911)) |>
  ungroup()

dim(data_aggregate)
length(unique(data_aggregate$Participant_ID)) # 32694 --this is what we expected

# save the file
# write_csv(data_aggregate, file = "data-raw/data_aggregate.csv")

data_aggregate <- read.csv("data-raw/data_aggregate.csv")

View(data_aggregate)

# see how many overlapping participants we have
overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
                         unique(scores_clean$Participant.ID))

length(overlap_idx) # 329
# so, 329 observations that contain demographic/info data (data_aggregate info)
#   and pre-post scores


# now, merge the data sets

data_merged <- merge(data_aggregate, scores_clean,
                     by.x = "Participant_ID",
                     by.y = "Participant.ID",
                     all = FALSE) # all = TRUE adds all rows


names(data_merged)
View(data_merged)
dim(data_merged)
length(unique(data_merged$Participant_ID)) # 329


# save the data set
write_csv(data_merged, file = "data-raw/data_merged.csv")

data_merged <- read.csv("data-raw/data_merged.csv")
View(data_merged)
