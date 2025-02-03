clean_and_split_column <- function(dt, col_name, sep = ";") {
  # Ensure dt is a data.table
  dt <- as.data.table(dt)

  # Split the column into a list
  split_values <- strsplit(as.character(dt[[col_name]]), split = sep,
                           perl = TRUE)

  # Determine the max number of splits
  max_splits <- max(lengths(split_values))

  # Generate new column names
  new_col_names <- paste0(col_name, "_Place", seq_len(max_splits))

  # Create new columns by filling in values or NAs
  dt[, (new_col_names) := t(sapply(split_values, function(x) {
    length(x) <- max_splits  # Ensure uniform length with NA padding
    x
  }))]

  # Remove the original column
  dt[, (col_name) := NULL]

  return(dt)
}


# vector version
clean_and_split_vector <- function(vec, var_name, sep = ";") {
  # Split the vector into a list
  split_values <- strsplit(as.character(vec), split = sep, perl = TRUE)

  # Replace "NULL" strings and blanks with NA
  split_values <- lapply(split_values, function(x) ifelse(x %in% c("NULL", ""),
                                                          NA, x))

  # Determine the max number of splits
  max_splits <- max(lengths(split_values))

  # Generate new vector names
  new_vec_names <- paste0(var_name, "_Place", seq_len(max_splits))

  # Create a matrix with values or NAs
  result_matrix <- t(sapply(split_values, function(x) {
    length(x) <- max_splits  # Ensure uniform length with NA padding
    x
  }))

  # Convert matrix to named list of vectors
  result_list <- setNames(as.list(as.data.frame(result_matrix,
                                                stringsAsFactors = FALSE)),
                          new_vec_names)

  return(result_list)
}


apply_cleaning_to_dataset <- function(data, special_cols, sep = ";") {
  for (col in special_cols) {
    if (col %in% names(data)) {
      split_results <- clean_and_split_vector(data[[col]], col, sep)
      data <- cbind(data, as.data.frame(split_results, stringsAsFactors = FALSE))
      data[[col]] <- NULL  # Remove the original column
    }
  }
  return(data)
}



### FASTER
library(dplyr)
library(purrr)

apply_handle_splits <- function(data, special_cols, sep = ";") {
  # Filter columns that exist in the data
  existing_cols <- intersect(special_cols, names(data))

  # Apply handle_splits2 function to each column in existing_cols using map
  split_results <- map(existing_cols, ~ handle_splits2(data[[.]], .x, sep))

  # Combine the results with the original data
  data <- bind_cols(data, as.data.frame(split_results, stringsAsFactors = FALSE))

  # Remove the original columns
  data <- select(data, -one_of(existing_cols))

  return(data)
}
