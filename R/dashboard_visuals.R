# read in data
data <- readRDS("data-raw/data_merged.rds")
scores <- read.csv("data-raw/trt_utah_4_8_2024.csv")

# clean the data
data_cleaned <- clean_utah(data, aggregate = TRUE)
scores_cleaned <- clean_scores(scores)

# merge the data
merged_data <- merge_scores(data_cleaned, scores_cleaned)

# create the metadata
metadata <- create_metadata(merged_data)

# View(metadata)

table(metadata$Grade_Level)

boxplot(metadata$Median_Difference_Score ~ metadata$Grade_Level)



