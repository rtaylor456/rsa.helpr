# condense the data so that it's structured with one row per participant, rather
#   than by quarters

# find a way to condense/average variables across quarters to do this

# take the medians for numeric variables
# compare values for demographic data

data_merged <- read.csv("data-raw/data_merged.csv")
View(data_merged)

library(tidyverse)

metadata_numeric <- data_aggregate |>
  group_by(Participant_ID) |>
  mutate(enroll_length = ) |>
  summarize(across(where(is.numeric),
                   ~ median(., na.rm = TRUE)))

View(metadata_numeric)




