data <- data.table::fread("data-raw/data_load_2025-02-17.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)

data_clean <- rsa.helpr::clean_utah(data)
scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")
provider_data <- rsa.helpr::clean_provider(scores, state_filter = "Utah")

merged <- rsa.helpr::merge_scores(data_clean, scores_clean)

metadata <- rsa.helpr::create_metadata(merged)

##### Calculate descriptive statistics of each TRT scale, showing group
#      means and standard deviations (pre, post, and change)

View(scores_clean)

table(scores_clean$Has_Multiple_Scores)

summary(scores_clean$Differences_Available)
sd(scores_clean$Differences_Available)

# Identify the relevant columns for each type of score
pre_score_cols <- grep("^Pre_Score", names(scores_clean), value = TRUE)
post_score_cols <- grep("^Post_Score", names(scores_clean), value = TRUE)
difference_cols <- grep("^Difference_", names(scores_clean), value = TRUE)

# Initialize the Services_Present column to 0 for all participants
scores_clean[, Services_Present := 0]

# Loop over the services and check if any of the Pre, Post, or Difference scores are non-missing
services <- unique(sub("^(Pre_Score_|Post_Score_|Difference_)", "",
                       c(pre_score_cols, post_score_cols, difference_cols)))

for(service in services) {
  # Get the columns related to this service
  service_cols <- c(
    grep(paste0("^Pre_Score_", service), names(scores_clean), value = TRUE),
    grep(paste0("^Post_Score_", service), names(scores_clean), value = TRUE),
    grep(paste0("^Difference_", service), names(scores_clean), value = TRUE)
  )

  # Update Services_Present: Increment by 1 if any of the columns for the service are non-missing
  scores_clean[, Services_Present := Services_Present +
                 as.integer(rowSums(!is.na(.SD)) > 0), .SDcols = service_cols,
               by = Participant_ID]
}

summary(scores_clean$Services_Present)
sd(scores_clean$Services_Present)


#### for metadata
table(metadata$Has_Multiple_Scores)
summary(metadata$Differences_Available)
sd(metadata$Differences_Available)

# Highlight sub-groups:
#   students who have entered PSE of some kind. (what scales, average scores
#     and change)
table(metadata$E84_PostSecondary_Enrollment_911)
# 0   3   2   1
# 104   0  62 317

table(metadata$E84_PostSecondary_Enrollment_911)[['1']] +
  table(metadata$E84_PostSecondary_Enrollment_911)[['2']] +
  table(metadata$E84_PostSecondary_Enrollment_911)[['3']]
# 379



#   students who completed PSE (what scales, average scores and change)
table(metadata2$E86_PostSecondary_Partial_Completion_911)
# 0    1 NULL
# 340   24   80





# library(data.table)
#
# # Extract service acronyms from variable names
# extract_services <- function(data) {
#   dt <- as.data.table(data)  # Ensure data.table format
#
#   # Identify columns that match the pattern
#   score_cols <- grep("^(Pre_Score|Post_Score|Difference)_[A-Z]+$", names(dt), value = TRUE)
#
#   # Extract the service acronyms by removing the prefix (Pre_Score, Post_Score, Difference) and the underscore
#   services <- unique(gsub("^(Pre_Score|Post_Score|Difference)_", "", score_cols))
#
#   return(services)
# }
#
# # Example usage
# services <- extract_services(scores_clean)
# print(services)

# summarize_scores_formatted <- function(data) {
#   # Convert to data.table if not already
#   dt <- as.data.table(data)
#
#   # Identify relevant columns
#   # services <- c("CPSO", "CSS", "FL", "OtherService1", "OtherService2")  # Update with all 10 services
#   services <- extract_services(scores_clean)
#   score_types <- c("Pre_Score", "Post_Score", "Difference")
#
#   # Create an empty list to store results
#   summary_list <- list()
#
#   # Loop through each service and calculate summary statistics
#   for (service in services) {
#     pre_col <- paste0("Pre_Score_", service)
#     post_col <- paste0("Post_Score_", service)
#     diff_col <- paste0("Difference_", service)
#
#     if (all(c(pre_col, post_col, diff_col) %in% names(dt))) {
#       summary_list[[service]] <- data.table(
#         Service = service,
#         Pre = sprintf("%.2f (%.2f)", mean(dt[[pre_col]], na.rm = TRUE), sd(dt[[pre_col]], na.rm = TRUE)),
#         Post = sprintf("%.2f (%.2f)", mean(dt[[post_col]], na.rm = TRUE), sd(dt[[post_col]], na.rm = TRUE)),
#         Diff = sprintf("%.2f (%.2f)", mean(dt[[diff_col]], na.rm = TRUE), sd(dt[[diff_col]], na.rm = TRUE))
#       )
#     }
#   }
#
#   # Combine into a final table
#   summary_table <- rbindlist(summary_list)
#
#   return(summary_table)
# }
#
# # Example usage
# summary_table <- summarize_scores_formatted(scores_clean)
# print(summary_table)
#
# library(data.table)
#
# library(data.table)
#
# summarize_scores_formatted <- function(data, robust_measures = TRUE) {
#   # Convert to data.table if not already
#   dt <- as.data.table(data)
#
#   # Extract service acronyms from column names
#   services <- unique(gsub("^(Pre_Score|Post_Score|Difference)_", "",
#                           grep("^(Pre_Score|Post_Score|Difference)_[A-Z]+$", names(dt), value = TRUE)))
#
#   # Create an empty list to store results
#   summary_list <- list()
#
#   # Loop through each service and calculate summary statistics
#   for (service in services) {
#     pre_col <- paste0("Pre_Score_", service)
#     post_col <- paste0("Post_Score_", service)
#     diff_col <- paste0("Difference_", service)
#
#     if (all(c(pre_col, post_col, diff_col) %in% names(dt))) {
#       # Compute standard and robust measures
#       pre_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[pre_col]], na.rm = TRUE), sd(dt[[pre_col]], na.rm = TRUE))
#       post_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[post_col]], na.rm = TRUE), sd(dt[[post_col]], na.rm = TRUE))
#       diff_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[diff_col]], na.rm = TRUE), sd(dt[[diff_col]], na.rm = TRUE))
#
#       if (robust_measures) {
#         pre_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[pre_col]], na.rm = TRUE),
#                                   IQR(dt[[pre_col]], na.rm = TRUE))
#         post_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[post_col]], na.rm = TRUE),
#                                    IQR(dt[[post_col]], na.rm = TRUE))
#         diff_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[diff_col]], na.rm = TRUE),
#                                    IQR(dt[[diff_col]], na.rm = TRUE))
#
#         summary_list[[service]] <- data.table(
#           Service = service,
#           Pre = paste0(pre_mean_sd, "; ", pre_median_iqr),
#           Post = paste0(post_mean_sd, "; ", post_median_iqr),
#           Diff = paste0(diff_mean_sd, "; ", diff_median_iqr)
#         )
#       } else {
#         summary_list[[service]] <- data.table(
#           Service = service,
#           Pre = pre_mean_sd,
#           Post = post_mean_sd,
#           Diff = diff_mean_sd
#         )
#       }
#     }
#   }
#
#   # Combine into a final table
#   summary_table <- rbindlist(summary_list)
#
#   return(summary_table)
# }
#
# # Example usage
# summary_table <- summarize_scores_formatted(scores_clean, robust_measures = TRUE)
# print(summary_table)
#
#
# library(data.table)
#
# summarize_scores_formatted <- function(data, robust_measures = TRUE, save_output = FALSE, file_name = "summary_scores.csv") {
#   # Convert to data.table if not already
#   dt <- as.data.table(data)
#
#   # Extract service acronyms from column names
#   services <- unique(gsub("^(Pre_Score|Post_Score|Difference)_", "",
#                           grep("^(Pre_Score|Post_Score|Difference)_[A-Z]+$", names(dt), value = TRUE)))
#
#   # Create an empty list to store results
#   summary_list <- list()
#
#   # Loop through each service and calculate summary statistics
#   for (service in services) {
#     pre_col <- paste0("Pre_Score_", service)
#     post_col <- paste0("Post_Score_", service)
#     diff_col <- paste0("Difference_", service)
#
#     if (all(c(pre_col, post_col, diff_col) %in% names(dt))) {
#       # Compute standard and robust measures
#       pre_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[pre_col]], na.rm = TRUE), sd(dt[[pre_col]], na.rm = TRUE))
#       post_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[post_col]], na.rm = TRUE), sd(dt[[post_col]], na.rm = TRUE))
#       diff_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[diff_col]], na.rm = TRUE), sd(dt[[diff_col]], na.rm = TRUE))
#
#       if (robust_measures) {
#         pre_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[pre_col]], na.rm = TRUE),
#                                   IQR(dt[[pre_col]], na.rm = TRUE))
#         post_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[post_col]], na.rm = TRUE),
#                                    IQR(dt[[post_col]], na.rm = TRUE))
#         diff_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[diff_col]], na.rm = TRUE),
#                                    IQR(dt[[diff_col]], na.rm = TRUE))
#
#         summary_list[[service]] <- data.table(
#           Service = service,
#           Measure = c("Mean (SD)", "Median (IQR)"),
#           Pre = c(pre_mean_sd, pre_median_iqr),
#           Post = c(post_mean_sd, post_median_iqr),
#           Diff = c(diff_mean_sd, diff_median_iqr)
#         )
#       } else {
#         summary_list[[service]] <- data.table(
#           Service = service,
#           Measure = "Mean (SD)",
#           Pre = pre_mean_sd,
#           Post = post_mean_sd,
#           Diff = diff_mean_sd
#         )
#       }
#     }
#   }
#
#   # Combine into a final table
#   summary_table <- rbindlist(summary_list)
#
#   # Save as CSV if requested
#   if (save_output) {
#     fwrite(summary_table, file = file_name)
#     message("Summary table saved as: ", file_name)
#   }
#
#   return(summary_table)
# }
#
# # Example usage:
# summary_table <- summarize_scores_formatted(scores_clean, robust_measures = TRUE, save_output = TRUE)
# print(summary_table)

