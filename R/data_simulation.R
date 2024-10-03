library(data.table)
library(tidyverse)

data <- fread("data-raw/data_full_new.csv", stringsAsFactors = FALSE)

data <- fread("data-raw/full_data.csv", stringsAsFactors = FALSE)

# remove annoying extra column that got added in
data[, V1 := NULL]


desc_vars <- grep("(?i)_Desc", names(data), value = TRUE)

without_e_number <- str_replace(desc_vars, "E\\d+_", "")

base_names <- sapply(without_e_number, function(x) {
  # If there is more than two underscores, remove the last word before "_Desc"
  if (str_count(x, "_") > 2) {
    str_replace(x, "_[^_]+_Desc", "")
  } else {
    # Otherwise, just remove "_Desc"
    str_replace(x, "_Desc", "")
  }
})

group_vars_by_pattern <- function(data, patterns) {
  patterns <- unique(patterns) # there are some repeats
  # App_Employer and App_Employer_Insurance groups capture the same variables,
  #   so if we don't remove one, we will end up with duplicate variables.
  # (Same for Exit_Employer)
  patterns <- patterns[!(patterns %in% c("App_Employer_Insurance",
                                         "Exit_Employer_Insurance"))]
  # Use lapply to create groups based on the provided patterns
  groups <- lapply(patterns, function(pattern) {
    data |> select(matches(pattern))
  })
  # Assign names to the groups
  names(groups) <- patterns
  return(groups)
}

# Function to permute the rows of each group
permute_group <- function(group, seed = 894) {
  set.seed(seed)
  permuted_indices <- sample(nrow(group)) # get permuted indices
  group[permuted_indices, , drop = FALSE] # apply permuted indices to the group
}


permute_dataset <- function(df, patterns, seed = 894) {
  # GROUPED COLUMNS
  vars_grouped <- group_vars_by_pattern(df, patterns)
  # permute the columns grouped
  # permuted_groups <- map(vars_grouped, permute_group)
  permuted_groups <- map(vars_grouped, ~
                           permute_group(group = ., seed = seed))
  # un-group, to get back to regular column format
  permuted_cols <- as.data.frame(do.call(cbind, permuted_groups))
  # fix names to get back to original
  permuted_colnames <- sub(".*\\.", "", names(permuted_cols))
  # colnames(permuted_cols) <- permuted_colnames
  names(permuted_cols) <- permuted_colnames

  # OTHER COLUMNS
  other_colnames <- names(data)[!(names(data) %in% permuted_colnames)]
  other_cols <- data[, ..other_colnames] # as data is a data.table, we have to
  #   use this weird format
  # permute other columns
  set.seed(seed)
  permuted_other_cols <- as.data.frame(lapply(other_cols, sample))

  # COMBINE COLUMNS
  permuted_data <- as.data.table(cbind(permuted_other_cols, permuted_cols))

  # get original column order
  original_col_order <- names(data)
  # this age variable has spaces in original name but R converts the spaces to
  #   periods--make them match.
  original_col_order <- gsub("Age at Application", "Age.at.Application",
                             original_col_order)

  # reorder columns in permuted_data to match the original order
  permuted_data <- permuted_data[, ..original_col_order]

  # REPLACE ID COLUMN
  permuted_data$Participant_ID <- seq(1:nrow(permuted_data))

  return(permuted_data)
}
