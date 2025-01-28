# Set the Box directory containing the Excel files
# directory <- "/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request"

# Get all PY data Excel files in the directory
# and
# Define the file to exclude -- temporary step until we get the complete data
#   file
# excluded_file <- "/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request/PY22Q1.xlsx"

file_list <- list.files(directory, pattern = ".*(PY.*Q|Q.*PY).*\\.xlsx$",
                        full.names = TRUE) |>
  setdiff(excluded_file)

# Display the final list of files
file_list


# Initialize an empty list for data
data_list <- list()

# Read files with error handling and exclusion
for (i in 1:length(file_list)) {
  tryCatch({
    # Ensure file is a valid Excel file
    if (grepl("\\.xlsx$", file_list[i])) {
      message("Reading: ", file_list[i])
      data <- read_excel(file_list[i], col_names = TRUE)
      data_list[[i]] <- data
    } else {
      message("Skipping non-Excel file: ", file_list[i])
    }
  }, error = function(e) {
    message("Error reading file: ", file_list[i], " - ", e$message)
  })
}


################################################################################

# Step 1: Identify all unique column names across datasets
all_columns <- unique(unlist(lapply(data_list, colnames)))

# Step 2: Ensure all datasets have the same columns and consistent classes
data_list <- lapply(data_list, function(data) {
  # Add missing columns with NA
  missing_cols <- setdiff(all_columns, colnames(data))
  for (col in missing_cols) {
    data[[col]] <- NA
  }
  # Ensure consistent column order
  setcolorder(data, all_columns)

  # Convert all columns to the same class across datasets
  for (col in colnames(data)) {
    data[[col]] <- as.character(data[[col]])  # Use character as the "safest" type
  }

  return(data)
})


lapply(data_list, function(x){length(names(x))})

# Step 3: Combine datasets into a single data.table
combined_data <- rbindlist(data_list, fill = TRUE)


dim(combined_data)

write.csv(combined_data, "rsa_full_1_21_25.csv")

data_NEW <- fread("data-raw/rsa_full_1_21_25.csv", stringsAsFactors = FALSE)

# lapply(data_list, function(x){table(x$E9_Gender_911}))

