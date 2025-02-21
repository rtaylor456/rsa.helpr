#' Load and Combine Data
#'
#' This function reads in and combines sensitive data from a directory.
#'
#' @param directory The directory containing the files
#' @param files Optional character vectore of file names to extract,
#'     eg. c("PY21Q2", "PY22Q3"). Otherwise, NULL, and all appropriate files
#'     from directory will be extracted.
#' @param download_csv Defaults to FALSE. If TRUE, will write a csv file
#'     containing loaded and combined data to working R directory.
#'
#' @returns A loaded and combined dataframe. Optionally, a csv data file written
#'    to the working directory.
#'
#' @export
#' @import data.table
#' @import readxl

load_data <- function(directory, files = NULL, download_csv = FALSE) {
  # Define the pattern to match the files
  pattern <- ".*(PY.*Q|Q.*PY).*\\.xlsx$"

  # Get all files in the directory matching the pattern
  all_files <- list.files(directory, pattern = pattern, full.names = TRUE)

  # If specific files are provided by the user, filter by those
  if (!is.null(files)) {
    # Match the user-specified file names (ignoring case for case-insensitivity)
    file_list <- all_files[grepl(paste(files, collapse = "|"),
                                 basename(all_files), ignore.case = TRUE)]
  } else {
    file_list <- all_files
  }

  # Initialize an empty list for data
  data_list <- list()

  # Read files with error handling and exclusion
  for (i in seq_along(file_list)) {
    tryCatch({
      # Ensure file is a valid Excel file
      if (grepl("\\.xlsx$", file_list[i])) {
        message("Reading: ", file_list[i])
        data <- suppressWarnings(readxl::read_excel(file_list[i],
                                                    col_names = TRUE))
        data_list[[i]] <- data
      } else {
        message("Skipping non-Excel file: ", file_list[i])
      }
    }, error = function(e) {
      message("Error reading file: ", file_list[i], " - ", e$message)
    })
  }

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
      # Use character as the "safest" type
      data[[col]] <- as.character(data[[col]])
    }

    data
  })

  # Step 3: Combine datasets into a single data.table
  combined_data <- rbindlist(data_list, fill = TRUE)

  # write a csv file to working directory if user chooses
  if (download_csv) {
    # Get today's date in the format "YYYY-MM-DD"
    today_date <- Sys.Date()

    # Create the file name with the date included
    file_name <- paste0("data_load_", today_date, ".csv")

    # Write the CSV file
    write.csv(combined_data, file = file_name)
  }

  return(combined_data)

}
