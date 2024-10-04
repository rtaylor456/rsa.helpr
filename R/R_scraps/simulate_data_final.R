library(data.table)
library(tidyverse)

data <- fread("data-raw/data_full_new.csv", stringsAsFactors = FALSE)

data <- fread("data-raw/full_data.csv", stringsAsFactors = FALSE)
data[, V1 := NULL]

# Step 1: List of description variables
desc_vars <- grep("(?i)_Desc", names(data), value = TRUE)

# Step 2: Remove the "E number_" part (e.g., E9_, E10_, etc.)
without_e_number <- str_replace(desc_vars, "E\\d+_", "")

# Step 3: Remove the "_Desc" suffix to get base variable names
# base_names <- str_replace(without_e_number, "_Desc", "")

# Step 3: For variables with multiple underscores, remove the last part before "_Desc"
# If only one underscore, keep the base name
base_names <- sapply(without_e_number, function(x) {
  # If there is more than two underscores, remove the last word before "_Desc"
  if (str_count(x, "_") > 2) {
    str_replace(x, "_[^_]+_Desc", "")
  } else {
    # Otherwise, just remove "_Desc"
    str_replace(x, "_Desc", "")
  }
})

vars_to_group <- data |>
  select(matches(base_names)) |>
  names()



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



# group_columns_by_pattern <- function(df, patterns) {
#   patterns <- unique(patterns) # there are some repeats
#   groups <- lapply(patterns, function(pattern) {
#     data |> select(matches(pattern))
#   })
#   # Assign names to the groups
#   names(groups) <- patterns
#
#   # Identify non-grouped columns
#   grouped_columns <- unlist(map(groups, colnames))
#   non_grouped_columns <- df |> select(-all_of(grouped_columns))
#
#   list(groups = groups, non_grouped = non_grouped_columns)
# }



vars_grouped <- group_vars_by_pattern(data, base_names)
vars_grouped

# to get variables out of list of groups
try <- do.call(cbind, vars_grouped)
# to remove the group names that get attached to each variable
try_names <- names(try)
cleaned_names <- sub(".*\\.", "", try_names)

# (check that the variable names of those grouped are what we expected)
setdiff(vars_to_group, cleaned_names) # character(0)
setdiff(cleaned_names, vars_to_group) # character(0)
length(cleaned_names) == length(vars_to_group) # TRUE

# to get the columns that aren't grouped
names(data)[!(names(data) %in% cleaned_names)]

# Function to permute the rows of each group
permute_group <- function(group, seed = 7783) {
  set.seed(seed)
  permuted_indices <- sample(nrow(group)) # get permuted indices
  group[permuted_indices, , drop = FALSE] # apply permuted indices to the group
}

# Apply permutation to each group
permuted_groups <- map(vars_grouped, permute_group)
# Show permuted groups
permuted_groups

# to get variables out of list of groups
try <- do.call(cbind, permuted_groups)
# to remove the group names that get attached to each variable
try_names <- names(try)
cleaned_names <- sub(".*\\.", "", try_names)

# (check that the variable names of those grouped are what we expected)
setdiff(vars_to_group, cleaned_names) # character(0)
setdiff(cleaned_names, vars_to_group) # character(0)
length(cleaned_names) == length(vars_to_group) # TRUE

# Show permuted groups
permuted_groups

permute_non_grouped <- function(non_grouped) {
  apply(non_grouped, 2, function(col) sample(col))  # Permute each column independently
}

# permuted_data <- c()
# for (i in seq(length(permuted_groups))) {
#   permuted_data <- cbind(permuted_data, permuted_groups[[i]])
# }
permuted_data <- do.call(cbind, permuted_groups)


permute_dataset <- function(df, patterns, seed = 894) {
  # get a vector of variable names we expect to group
  # vars_to_group <- data |>
  #   select(matches(base_names)) |>
  #   names()

  # Group columns
  vars_grouped <- group_vars_by_pattern(df, patterns)
  permuted_groups <- map(vars_grouped, permute_group)

  # # Permute the grouped columns
  # permuted_groups <- map(grouped_data$groups, permute_group)
  permuted_cols <- as.data.frame(do.call(cbind, permuted_groups))
  permuted_colnames <- sub(".*\\.", "", names(permuted_cols))
  colnames(permuted_cols) <- permuted_colnames

  # Permute the rest of the columns independently
  other_colnames <- names(data)[!(names(data) %in% permuted_colnames)]
  # other_cols <- data[, other_colnames]
  other_cols <- data[, ..other_colnames] # as data is a data.table, we have to
  #   use this weird format

  # permuted_other_cols <- as.data.frame(lapply(other_cols,
  #                                             function(col) sample(col)))

  permuted_other_cols <- as.data.frame(lapply(other_cols, sample))

  # # Permute non-grouped columns
  # permuted_non_grouped <- permute_non_grouped(grouped_data$non_grouped)
  #
  # # Combine the permuted groups and non-grouped columns
  # permuted_df <- bind_cols(permuted_groups,
  #                          as.data.frame(permuted_non_grouped))
  #
  # # Reorder columns to match original dataset
  # permuted_df <- permuted_df[, colnames(df)]

  permuted_data <- as.data.table(cbind(permuted_other_cols, permuted_cols))

  # Reorder columns in permutated_data to match the original order
  original_col_order <- names(data)
  original_col_order <- gsub("Age at Application", "Age.at.Application",
                             original_col_order)

  permuted_data <- permuted_data[, ..original_col_order]
  permuted_data$Participant_ID <- seq(1:nrow(permuted_data))

  return(permuted_data)
}



################################################################################

# 2. Function to group columns by pattern
group_columns_by_pattern <- function(df, patterns) {
  groups <- map(patterns, ~ df %>% select(matches(.x)))
  names(groups) <- patterns

  # Identify non-grouped columns
  grouped_columns <- unlist(map(groups, colnames))
  non_grouped_columns <- df %>% select(-all_of(grouped_columns))

  list(groups = groups, non_grouped = non_grouped_columns)
}




# 3. Permute function for grouped columns
permute_group <- function(group) {
  permuted_indices <- sample(nrow(group))  # Permute the row indices
  group[permuted_indices, , drop = FALSE]  # Apply the permutation to rows
}

# 4. Permute non-grouped columns independently
permute_non_grouped <- function(non_grouped) {
  apply(non_grouped, 2, function(col) sample(col))  # Permute each column independently
}

# 5. Apply permutations to the whole dataset
permute_dataset <- function(df, patterns) {
  # Group columns and get non-grouped columns
  grouped_data <- group_columns_by_pattern(df, patterns)

  # Permute the grouped columns
  permuted_groups <- map(grouped_data$groups, permute_group)

  # Permute non-grouped columns
  permuted_non_grouped <- permute_non_grouped(grouped_data$non_grouped)

  # Combine the permuted groups and non-grouped columns
  permuted_df <- bind_cols(permuted_groups,
                           as.data.frame(permuted_non_grouped))

  # Reorder columns to match original dataset
  permuted_df <- permuted_df[, colnames(df)]

  return(permuted_df)
}

# 6. Run the permutation
permuted_df <- permute_dataset(df, patterns)

# View the result
print(permuted_df)
