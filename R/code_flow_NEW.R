data <- data.table::fread("data-raw/data_load_2025-04-22.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)


data_clean <- clean_utah(data)
scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")

merged <- merge_scores(data_clean, scores_clean)

metadata <- create_metadata(merged)
