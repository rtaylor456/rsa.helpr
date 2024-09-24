data <- fread("data-raw/data_full_new.csv", stringsAsFactors = FALSE)
data <- fread("data-raw/full_data.csv", stringsAsFactors = FALSE)

try <- data[1:100, 1:50]
View(try)


set.seed(4873)

names(try)

vars <- c("Participant_ID", "E1_Year_911", "E2_Quarter_911")


# Subset the data frame by excluding the specified variables
df <- try[, setdiff(names(try), vars), with = FALSE]

df_scrambled <- setDT(lapply(df, function(x) sample(x, size = nrow(df),
                                              replace = FALSE)))

View(df_scrambled)


library(data.table)


# Identify categorical variables and their corresponding _Desc variables
# Variables without '_Desc'
categorical_vars <- grep("^[^_]+$", names(try), value = TRUE)
# Variables with '_Desc'
desc_vars <- grep("(?i)_Desc$", names(try), value = TRUE)

# Extract the base names (portion between the "E number" and "_Desc")
without_e_number <- str_replace(desc_vars, "E\\d+_", "")
base_names <- str_replace(without_e_number, "_Desc$", "")

categorical_vars <- try |> select( (contains(base_names) |
                                      contains("primary_disability") |
                                      contains("primary_disability")) &
                                     -contains("_Desc")) |>
  names()
categorical_vars <- try |> select(contains(base_names)) |>
  names()

# Filter to only keep matching categorical variables and their corresponding
#   descriptions
paired_vars <- setNames(categorical_vars, desc_vars)


# Scramble each pair together
for (var in paired_vars) {

  # Scramble the values of the variable and its description together
  # Generate a common shuffle index
  scramble_idx <- sample(nrow(try), replace = FALSE)
  try[, c(var, desc_var) := .(get(var)[scramble_idx],
                              get(desc_var)[scramble_idx])]
}

# View the scrambled data
View(try)


###############################################################################
# List of description variables
desc_vars <- grep("(?i)_Desc", names(try), value = TRUE)

# Step 1: Remove the "E number_" part (e.g., E9_, E10_, etc.)
without_e_number <- str_replace(desc_vars, "E\\d+_", "")

# Step 2: Remove the "_Desc" suffix
base_names <- str_replace(without_e_number, "_Desc", "")

# Find corresponding categorical variables by matching these base names in column names
categorical_vars <- sapply(base_names, function(base) {
  grep(paste0("E\\d+_", base, "$"), names(try), value = TRUE)
})

categorical_vars <- try |> select( (contains(base_names) |
                                      contains("primary_disability") |
                                      contains("primary_disability")) &
                                     -contains("_Desc")) |>
  names()

# Handle the exception for "E43_Primary_Disability_911"
paired_vars <- setNames(categorical_vars, desc_vars)
paired_vars[["E43_Primary_Impairment_Desc"]] <- "E43_Primary_Disability_911"
paired_vars[["E43_Primary_Cause_Desc"]] <- "E43_Primary_Disability_911"

# Check the pairs of categorical and description variables
print(paired_vars)

