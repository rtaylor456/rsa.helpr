install.packages("stringdist")
install.packages("fuzzyjoin")


library(fuzzyjoin)

# Assume your datasets are called df1 and df2, and Participant_ID is the matching column
fuzzy_matches <- stringdist_inner_join(
  data_clean, scores_clean,
  by = "Participant_ID",
  method = "jw",          # Jaro-Winkler distance is good for typos
  max_dist = 0.1,        # adjust this threshold to allow more/less fuzzy matches
  distance_col = "distance"
)

# View the fuzzy matches
dim(fuzzy_matches)

fuzzy_matches_sorted <- fuzzy_matches[order(fuzzy_matches$distance), ]

fuzzy_only <- fuzzy_matches_sorted[fuzzy_matches_sorted$distance > 0, ]


check <- fuzzy_only[, c("Participant_ID.x", "Participant_ID.y",
                                  "distance"
                                  )]

tail(check)
View(check)


# fuzzy_matches_sorted$Application_Date <- as.Date(fuzzy_matches_sorted$Application_Date)
#
# fuzzy_matches_sorted$Completed_Date <- as.Date(fuzzy_matches_sorted$Completed_Date)


exact_ids <- inner_join(data_clean, scores_clean, by = "Participant_ID")$Participant_ID
fuzzy_ids <- fuzzy_matches_sorted$Participant_ID.x

length(intersect(exact_ids, fuzzy_ids))


nrow(fuzzy_matches_sorted)


# how many are NEW (not in exact matches)?
new_fuzzy_ids <- setdiff(fuzzy_ids, exact_ids)
length(new_fuzzy_ids)  # this is your answer!

View(fuzzy_matches_sorted)
