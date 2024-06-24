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
  lapply(function(x) summary(x, na.rm = TRUE))



## looking at pre scores
pre_scores <- metadata |>
  select(contains("pre_"))

pre_scores |>
  lapply(function(x) sum(!is.na(x))) # no pre scores for QWEX? why?

pre_scores |>
  lapply(function(x) summary(x, na.rm = TRUE))

for (col in colnames(pre_scores)){
  hist(pre_scores[[col]], main = col, xlab = col)
}



## looking at pre scores
post_scores <- metadata |>
  select(contains("post_"))

post_scores |>
  lapply(function(x) sum(!is.na(x))) # we do have post scores for QWEX

post_scores |>
  lapply(function(x) summary(x, na.rm = TRUE))

for (col in colnames(post_scores)){
  hist(post_scores[[col]], main = col, xlab = col)
}

post_scores_names <- names(post_scores)

boxplot(metadata$Pre_Score_CPSO, metadata$Pre_Score_CSS,
        metadata$Pre_Score_EMP, metadata$Pre_Score_FL,
        metadata$Pre_Score_ILOM, metadata$Pre_Score_ISA,
        metadata$Pre_Score_JOBEX, metadata$Pre_Score_JS,
        metadata$Pre_Score_QWEX, metadata$Pre_Score_WBLE,
        names = c("CPSO", "CSS", "EMP", "FL", "ILOM",
                  "ISA", "JOBEX", "JS", "QWEX", "WBLE"),
        main = "Distributions of Pre-Scores",
        ylab = "Scores",
        xlab = "Test Category")
abline(h = 60, lty = 2, col = "blue")


boxplot(metadata$Post_Score_CPSO, metadata$Post_Score_CSS,
        metadata$Post_Score_EMP, metadata$Post_Score_FL,
        metadata$Post_Score_ILOM, metadata$Post_Score_ISA,
        metadata$Post_Score_JOBEX, metadata$Post_Score_JS,
        metadata$Post_Score_QWEX, metadata$Post_Score_WBLE,
        names = c("CPSO", "CSS", "EMP", "FL", "ILOM",
                  "ISA", "JOBEX", "JS", "QWEX", "WBLE"),
        main = "Distributions of Post-Scores",
        ylab = "Scores",
        xlab = "Test Category")
abline(h = 60, lty = 2, col = "blue")
abline(h = 76, lty = 2, col = "red")



check <- scores |>
  filter(Service == "ISA" & Pre.Post == "Pre") |>
  filter(Participant.ID %in% overlap_idx)

View(check)






# Calculate percentage change
percentage_change <- function(pre, post) {
  return(((post - pre) / pre) * 100)
}

# Apply the function to each pre-post pair
change_CPSO <- percentage_change(metadata$Pre_Score_CPSO,
                                 metadata$Post_Score_CPSO)
change_CSS <- percentage_change(metadata$Pre_Score_CSS,
                                metadata$Post_Score_CSS)
change_EMP <- percentage_change(metadata$Pre_Score_EMP,
                                metadata$Post_Score_EMP)
change_FL <- percentage_change(metadata$Pre_Score_FL,
                               metadata$Post_Score_FL)
change_ILOM <- percentage_change(metadata$Pre_Score_ILOM,
                                 metadata$Post_Score_ILOM)
change_ISA <- percentage_change(metadata$Pre_Score_ISA,
                                metadata$Post_Score_ISA)
change_JOBEX <- percentage_change(metadata$Pre_Score_JOBEX,
                                  metadata$Post_Score_JOBEX)
change_JS <- percentage_change(metadata$Pre_Score_JS,
                               metadata$Post_Score_JS)
change_QWEX <- percentage_change(metadata$Pre_Score_QWEX,
                                 metadata$Post_Score_QWEX)
change_WBLE <- percentage_change(metadata$Pre_Score_WBLE,
                                 metadata$Post_Score_WBLE)

# Combine results into a data frame
percentage_changes <- data.frame(
  # Category = c("CPSO", "CSS", "EMP", "FL", "ILOM", "ISA", "JOBEX",
  #              "JS", "QWEX", "WBLE"),
  Category = c("CPSO", "CSS", "FL", "ILOM", "ISA", "JOBEX",
               "WBLE"),
  Change = c(mean(change_CPSO, na.rm = TRUE),
             mean(change_CSS, na.rm = TRUE),
             # mean(change_EMP, na.rm = TRUE),
             mean(change_FL, na.rm = TRUE),
             mean(change_ILOM, na.rm = TRUE),
             mean(change_ISA, na.rm = TRUE),
             mean(change_JOBEX, na.rm = TRUE),
             # mean(change_JS, na.rm = TRUE),
             # mean(change_QWEX, na.rm = TRUE),
             mean(change_WBLE, na.rm = TRUE))
)

# Print the percentage changes
print(percentage_changes)

# Optionally, create a bar plot of the percentage changes
barplot(percentage_changes$Change,
        names.arg = percentage_changes$Category,
        main = "Percentage Change in Scores",
        ylab = "Percentage Change",
        col = "lightblue",
        ylim = c(min(percentage_changes$Change) - 5,
                 max(percentage_changes$Change) + 5))
abline(h = 0, lty = 2, col = "red")






