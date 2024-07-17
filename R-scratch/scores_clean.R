library(tidyverse)
library(lubridate)

scores <- read.csv("data-raw/trt_utah_6_3_2024_4pm.csv")
View(scores)

unique(scores$Service)

# Pivot the DataFrame
# scores_wide <- scores |>
#   pivot_wider(names_from = Pre.Post, values_from = Score,
#               names_prefix = "Score_") |>
#   arrange(Participant.ID, Service)
# View(scores_wide)


scores$Completed <- mdy_hms(gsub(" \\(MST\\)", "", scores$Completed))
class(scores$Completed)
View(scores)

# scores$Pre.Post[scores$Pre.Post == "Pre"]

pre_data <- scores |>
  filter(Pre.Post == "Pre")
pre_data

post_data <- scores |>
  filter(Pre.Post == "Post")
post_data

# Keep the earliest 'Pre' score and most recent 'Post' score
earliest_pre <- pre_data |>
  arrange(Completed) |>
  group_by(Participant.ID, Provider, Service) |>
  slice(1)
View(earliest_pre)

latest_post <- post_data |>
  arrange(desc(Completed)) |>
  group_by(Participant.ID, Provider, Service) |>
  slice(1)
View(latest_post)

merged_data <- full_join(
  earliest_pre |>
    select(Participant.ID, Service, Provider,
           Pre_Score = Score),
  latest_post |>
    select(Participant.ID, Service, Provider,
           Post_Score = Score,
           Difference = Difference),
  by = c("Participant.ID", "Service", "Provider")
)


final_data <- merged_data |>
  pivot_wider(
    names_from = Service,
    values_from = c(Pre_Score, Post_Score, Difference),
    names_glue = "{.value}_{Service}"
  )

View(final_data)


write_csv(final_data, file = "data-raw/scores_clean.csv")

scores_clean <- read.csv("data-raw/scores_clean.csv")
View(scores_clean)


# get numeric summaries of services' pre-post scores

# this gets the info we want, but the format is weird to look at
summary_stats <- scores_clean |>
  summarise(across(starts_with("Pre_Score_"), list(
    count = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  )),
  across(starts_with("Post_Score_"), list(
    count = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  )))
View(summary_stats)

# get the general distribution of the scores
scores_clean |>
  select(contains("pre_") | contains("post_")) |>
  lapply(summary)

# get the counts of scores for each test
scores_clean |>
  select(contains("pre_") | contains("post_")) |>
  lapply(function(x) sum(!is.na(x)))



