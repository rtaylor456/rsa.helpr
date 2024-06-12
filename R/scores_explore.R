# data_aggregate <- read.csv("data-raw/data_aggregate.csv")
# View(data_aggregate)

scores_clean <- read.csv("data-raw/scores_clean.csv")
View(scores_clean)

# overlap_idx <- intersect(unique(data_aggregate$Participant_ID),
#                          unique(scores_clean$Participant.ID))

# get the general distribution of the scores
scores_clean |>
  select(contains("pre_") | contains("post_")) |>
  lapply(summary)

# get the counts of scores for each test
scores_clean |>
  select(contains("pre_") | contains("post_")) |>
  lapply(function(x) sum(!is.na(x)))


data_merged <- read.csv("data-raw/data_merged.csv")
View(data_merged)

overlap_idx <- unique(data_merged$Participant_ID)
length(overlap_idx) # 329 -- as expected

used_scores <- scores_clean |>
  filter(Participant.ID %in% overlap_idx)
View(used_scores)

write.csv(used_scores, file = "data-raw/used_scores.csv")

used_scores <- read.csv("data-raw/used_scores.csv")
View(used_scores)

# get the general distribution of the scores
used_scores |>
  select(contains("pre_") | contains("post_")) |>
  lapply(summary)

# get the counts of scores for each test
used_scores |>
  select(contains("pre_") | contains("post_")) |>
  lapply(function(x) sum(!is.na(x)))

used_scores |>
  select(contains("difference")) |>
  lapply(function(x) sum(!is.na(x)))

differences <- used_scores |>
  select(contains("difference"))

for (col in colnames(differences)){
  hist(differences[[col]], main = col, xlab = col)
}




