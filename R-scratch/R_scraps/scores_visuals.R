
scores <- fread("data-raw/trt_utah_4_8_2024.csv", stringsAsFactors = FALSE)
scores_clean <- clean_scores(scores)

# Select columns containing "difference" but not "Median_Difference_Score"
difference_cols <- grep("(?i)difference", names(scores_clean),
                        value = TRUE, perl = TRUE)
differences <- scores_clean[, .SD, .SDcols = difference_cols]

# Find the overall median for all differences scores
differences_scores_vector <- as.vector(unlist(differences))
differences_median <- median(differences_scores_vector, na.rm = TRUE)
differences_median


boxplot(scores_clean$Difference_CPSO, scores_clean$Difference_CSS,
        scores_clean$Difference_EMP, scores_clean$Difference_FL,
        scores_clean$Difference_ILOM, scores_clean$Difference_ISA,
        scores_clean$Difference_JOBEX, scores_clean$Difference_JS,
        scores_clean$Difference_QWEX, scores_clean$Difference_WBLE,
        names = c("CPSO", "CSS", "EMP", "FL", "ILOM",
                  "ISA", "JOBEX", "JS", "QWEX", "WBLE"),
        main = "Distributions of Difference Scores",
        ylab = "Scores",
        xlab = "Test Category")
abline(h = differences_median, lty = 2, col = "blue")



