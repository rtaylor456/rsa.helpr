library(readxl)
# library(dplyr)
library(stringr)
library(data.table)

# Set the Box directory containing the Excel files
directory <- "/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request"

# Get all Excel files in the directory
file_list <- list.files(directory, pattern = "*.xlsx", full.names = TRUE)
file_list

file_list2 <- file_list[grep("PY.*Q|Q.*PY", file_list)]
file_list2


# try <- read_excel(file_list2[17], col_names = TRUE)
# View(try)

# Define the file to exclude
excluded_file <- "/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request/PY22Q1.xlsx"

# Exclude the specified file
file_list2 <- setdiff(file_list2, excluded_file)

# Display the final list of files
file_list2



################################################################################
## error checking for invalid data files

for (i in 1:length(file_list2)) {
  tryCatch({
    data <- read_excel(file_list2[i], col_names = TRUE)
    message("Successfully read: ", file_list2[i])
  }, error = function(e) {
    message("Error reading: ", file_list2[i], " - ", e$message)
  })
}

# Error reading: /Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request/PY22Q1.xlsx - error reading from the connection




################################################################################

library(readxl)

# Initialize an empty list for data
data_list <- list()

# Specify the file to exclude
excluded_file <- "/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request/PY22Q1.xlsx"

# Read files with error handling and exclusion
for (i in 1:length(file_list2)) {
  # Skip the excluded file
  if (file_list2[i] == excluded_file) {
    message("Skipping excluded file: ", excluded_file)
    next
  }

  tryCatch({
    # Ensure file is a valid Excel file
    if (grepl("\\.xlsx$", file_list2[i])) {
      message("Reading: ", file_list2[i])
      data <- read_excel(file_list2[i], col_names = TRUE)
      data_list[[i]] <- data
    } else {
      message("Skipping non-Excel file: ", file_list2[i])
    }
  }, error = function(e) {
    message("Error reading file: ", file_list2[i], " - ", e$message)
  })
}

# Check if there are any valid data frames
if (length(data_list) > 0) {
  # Find common column names across all data frames
  common_columns <- Reduce(intersect, lapply(data_list, colnames))

  # Filter each data frame to keep only common columns
  data_list_filtered <- lapply(data_list, function(df) df[, common_columns,
                                                          drop = FALSE])

  # Combine the filtered data frames
  combined_data <- do.call(rbind, data_list_filtered)

  message("Data successfully combined with matching columns only.")
} else {
  message("No valid data files to combine.")
}

# View the combined data
if (exists("combined_data")) {
  View(combined_data)
}

################################################################################
# Find common columns manually
common_columns <- colnames(data_list[[1]])
for (i in 1:length(data_list)) {
  common_columns <- intersect(common_columns, colnames(data_list[[i]]))
  print(length(common_columns))
}

dim(combined_data)

write.csv(combined_data, "rsa_full_1_21_25.csv")

################################################################################
# library(readxl)
#
# # Initialize an empty list for data
# data_list <- list()
#
# # Specify the file to exclude
# excluded_file <- "/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request/PY22Q1.xlsx"
#
# # Read files with error handling and exclusion
# for (i in 1:length(file_list2)) {
#   # Skip the excluded file
#   if (file_list2[i] == excluded_file) {
#     message("Skipping excluded file: ", excluded_file)
#     next
#   }
#
#   tryCatch({
#     # Ensure file is a valid Excel file
#     if (grepl("\\.xlsx$", file_list2[i])) {
#       message("Reading: ", file_list2[i])
#       data <- read_excel(file_list2[i], col_names = TRUE)
#       data_list[[i]] <- data
#     } else {
#       message("Skipping non-Excel file: ", file_list2[i])
#     }
#   }, error = function(e) {
#     message("Error reading file: ", file_list2[i], " - ", e$message)
#   })
# }
#
# # Combine all data frames if no errors occurred
# if (length(data_list) > 0) {
#   combined_data <- do.call(rbind, data_list)
#   message("Data successfully combined.")
# } else {
#   message("No valid data files to combine.")
# }


