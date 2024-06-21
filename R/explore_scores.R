# This script will involve exploring the pre-post and difference scores for the
#    different 10 tests to look for any univariate impacts

# look at the distribution of each of the scores

differences <- metadata |>
  select(contains("difference"))

differences |>
  lapply(function(x) sum(!is.na(x)))

# get the counts of scores for each test
metadata |>
  select(contains("pre_") | contains("post_")) |>
  lapply(function(x) sum(!is.na(x)))

differences |>
  select(contains("difference")) |>
  lapply(function(x) sum(!is.na(x)))

for (col in colnames(differences)){
  hist(differences[[col]], main = col, xlab = col)
}

differences |>
  select(contains("difference")) |>
  lapply(function(x) median(x, na.rm = TRUE))


pre_scores <- metadata |>
  select(contains("pre_"))


pre_scores |>
  lapply(function(x) summary(x, na.rm = TRUE))


check <- scores |>
  filter(Service == "ISA" & Pre.Post == "Pre") |>
  filter(Participant.ID %in% any_of(overlap_idx))

