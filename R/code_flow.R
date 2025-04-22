# Install the rsa.helpr package
devtools::install_github("rtaylor456/rsa.helpr")

## Load RSA data
# Mac directory:
directory <- c("/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request")

# PC directory:
# directory <- "C:\\Users\\Ruth Taylor\\Box\\911 Data and Related Projects\\911 Data\\Utah Quarterly Data\\USU Data Request"

# load data from directory
quarterly <- rsa.helpr::load_data(directory, download_csv = TRUE)


## Load TRT Scores data
# Mac directory:
# directory <- c("/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/TRT Data_1.28.2025 at 12:00pm.csv")

# PC directory:
# runs into issues, because of file name just take off Box

# scores <- data.table::fread(directory, stringsAsFactors = FALSE)



## Read data in without Box:
# data <- data.table::fread("data-raw/rsa_full_1_29_25.csv",
#                           stringsAsFactors = FALSE)

# data <- data.table::fread("data-raw/data_load_2025-02-17.csv",
#                           stringsAsFactors = FALSE)

data <- data.table::fread("data-raw/data_load_2025-04-16.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)


data_clean <- rsa.helpr::clean_utah(data)
View(data_clean)
# write.csv(data_clean, "data_clean.csv")

scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")
View(scores_clean)
# write.csv(scores_clean, "scores_clean.csv")

provider_data <- rsa.helpr::clean_provider(scores, state_filter = "Utah")

merged <- rsa.helpr::merge_scores(data_clean, scores_clean)

metadata <- rsa.helpr::create_metadata(merged)
View(metadata)
write.csv(metadata, "metadata_clean.csv")

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
                            one_window = FALSE)
