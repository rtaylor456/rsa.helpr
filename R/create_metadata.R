# condense the data so that it's structured with one row per participant, rather
#   than by quarters

# find a way to condense/average variables across quarters to do this

# take the medians for numeric variables
# compare values for demographic data

library(tidyverse)
library(data.table)

data_merged <- read.csv("data-raw/data_merged.csv")
View(data_merged)

data_clean <- read.csv("data-raw/data_clean.csv")
data_clean |> select(where(is.numeric)) |>
  names()

library(tidyverse)

metadata_numeric <- data_merged |>
  group_by(Participant_ID) |>
  select((where(is.numeric))) |>
  names()
  # mutate(enroll_length = ) |>
  summarize(across(where(is.numeric),
                   ~ median(., na.rm = TRUE)))

View(metadata_numeric)




