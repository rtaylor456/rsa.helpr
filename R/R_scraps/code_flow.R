# data <- data.table::fread("data-raw/rsa_full_1_29_25.csv",
#                           stringsAsFactors = FALSE)

data <- data.table::fread("data-raw/data_load_2025-02-17.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)


# devtools::install_github("rtaylor456/rsa.helpr")

data_clean <- rsa.helpr::clean_utah(data)
View(data_clean)

scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")
View(scores_clean)

provider_data <- rsa.helpr::clean_provider(scores, state_filter = "Utah")

merged <- rsa.helpr::merge_scores(data_clean, scores_clean)

metadata <- rsa.helpr::create_metadata(merged)
View(metadata)

# check visualization functions
?visualize_densities
?visualize_metadata
?visualize_scores

rsa.helpr::visualize_densities(metadata$E9_Gender_911,
                               metadata$Median_Difference_Score)


rsa.helpr::visualize_metadata(metadata, option = "general_demo",
                              one_window = TRUE)

rsa.helpr::visualize_metadata(metadata, option = "investigate_scores",
                              one_window = TRUE)

rsa.helpr::visualize_metadata(metadata, option = "investigate_wage",
                              one_window = TRUE)

rsa.helpr::visualize_metadata(metadata, option = "investigate_employment",
                              one_window = TRUE)


rsa.helpr::visualize_scores(scores_clean, option = "overview",
                            one_window = TRUE)

rsa.helpr::visualize_scores(scores_clean, option = "across_service",
                            one_window = TRUE)

rsa.helpr::visualize_scores(scores_clean, option = "across_provider",
                            one_window = TRUE)
