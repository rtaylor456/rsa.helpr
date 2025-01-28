# Focus on provider-related variables: This will involve organizing the data by services and provider attributes.
# Include provider-level metrics: Analyze the Pre_Score, Post_Score, and Difference_Score for different caseloads, group frequencies, online frequencies, rural frequencies, and modes.
# Retain one row per provider and service: Aggregate the data appropriately to keep one row per provider and service for each of the attributes of interest.
# Function Outline
# Separate provider-related variables: We'll focus on Pre_Score, Post_Score, Difference_Score, and provider metadata (Caseload, Group Frequency, Online Frequency, Rural Frequency, and Mode).
# Aggregate scores per provider: For each service, we'll compute summary statistics for the scores (like mean, median, etc.), grouped by the provider's metadata.
#
# Key Steps:
#   Data Selection: The function focuses on Provider, Service, Caseload, Group_Frequency, Online_Frequency, Rural_Frequency, Mode, and the score columns (Pre_Score, Post_Score, Difference_Score).
#
# Optional Aggregation: If aggregate = TRUE, the data is grouped by Provider, Service, and the various provider metadata (Caseload, Group_Frequency, Online_Frequency, Rural_Frequency, and Mode). Summary statistics like Mean, Median, and Count are computed for the scores.
#
# Additional Summary Metrics: The function calculates standard deviations (SD) for each score category, which is useful for analyzing the variability in scores for each provider and service combination.
#
# Return Provider-Level Data: The function returns the cleaned and aggregated provider-level dataset.

# Explanation of Parameters:
#   data: Your original dataset with provider-level information and participant scores.
# aggregate: A logical argument that controls whether the data should be aggregated (e.g., calculating mean or median scores per provider and service).
# Further Customization:
#   Other Aggregations: If you'd like to calculate other summary statistics (e.g., percentiles, interquartile range), you can modify the aggregation section.
# Grouping Variables: If you'd like to group by additional variables or perform other analyses (e.g., time series analysis of scores per service), you can easily extend this function.
# This function should help you focus on analyzing the provider-level data and ensure you get meaningful summaries for each provider's scores across different services and frequencies.
#
# Let me know if you'd like any further adjustments or additional features!


clean_provider <- function(data, aggregate = TRUE) {
  # Convert to data.table
  setDT(data)

  # Identify provider-related columns
  provider_columns <- c("Provider", "Caseload", "Group Frequency", "Online Frequency", "Rural Frequency", "Mode")

  # Ensure proper renaming if necessary (e.g., `Group Frequency` to `Group_Frequency`)
  setnames(data, old = c("Group Frequency", "Online Frequency", "Rural Frequency"),
           new = c("Group_Frequency", "Online_Frequency", "Rural_Frequency"))

  # Focus on the necessary columns: Provider-related metadata + scores
  provider_data <- data[, c("Provider", "Service", "Caseload", "Group_Frequency", "Online_Frequency",
                            "Rural_Frequency", "Mode", "Pre_Score", "Post_Score", "Difference_Score")]

  # Optionally aggregate data by service and provider
  if (aggregate) {
    provider_data <- provider_data[, .(Mean_Pre_Score = mean(Pre_Score, na.rm = TRUE),
                                       Mean_Post_Score = mean(Post_Score, na.rm = TRUE),
                                       Mean_Difference_Score = mean(Difference_Score, na.rm = TRUE),
                                       Median_Pre_Score = median(Pre_Score, na.rm = TRUE),
                                       Median_Post_Score = median(Post_Score, na.rm = TRUE),
                                       Median_Difference_Score = median(Difference_Score, na.rm = TRUE),
                                       Count = .N),
                                   by = .(Provider, Service, Caseload, Group_Frequency,
                                          Online_Frequency, Rural_Frequency, Mode)]
  }

  # Optionally: You can add additional summary metrics, such as standard deviation or percentiles
  provider_data[, SD_Pre_Score := sd(Pre_Score, na.rm = TRUE), by = .(Provider, Service)]
  provider_data[, SD_Post_Score := sd(Post_Score, na.rm = TRUE), by = .(Provider, Service)]
  provider_data[, SD_Difference_Score := sd(Difference_Score, na.rm = TRUE), by = .(Provider, Service)]

  # Return the cleaned provider-level data
  return(provider_data)
}


