library(data.table)
library(tidyverse)

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




# Identify categorical variables and their corresponding _Desc variables
# Variables without '_Desc'
# categorical_vars <- grep("^[^_]+$", names(data), value = TRUE)
# Variables with '_Desc'
desc_vars <- grep("(?i)_Desc$", names(data), value = TRUE)

# Extract the base names (portion between the "E number" and "_Desc")
without_e_number <- str_replace(desc_vars, "E\\d+_", "")
base_names <- str_replace(without_e_number, "_Desc$", "")

categorical_vars <- data |> select( (contains(base_names) |
                                      contains("primary_disability") |
                                      contains("primary_disability")) &
                                     -contains("_Desc")) |>
  names()

categorical_vars_desc <- data |> select(contains(base_names) & contains("_Desc")) |>
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
desc_vars <- grep("(?i)_Desc", names(data), value = TRUE)

# Step 1: Remove the "E number_" part (e.g., E9_, E10_, etc.)
without_e_number <- str_replace(desc_vars, "E\\d+_", "")

# Step 2: Remove the "_Desc" suffix
base_names <- str_replace(without_e_number, "_Desc", "")

# Find corresponding categorical variables by matching these base names in column names
categorical_vars <- sapply(base_names, function(base) {
  grep(paste0("E\\d+_", base, "$"), names(data), value = TRUE)
})

categorical_vars <- data |> select( (contains(base_names) |
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


cat_vars_try <- data |> select(contains(base_names) & -contains("_Desc")) |>
  names()

cat_vars_try2 <- cat_vars_try |> str_replace("E\\d+_", "") |>
  str_replace("_911", "")

setdiff(base_names, cat_vars_try2)

################################################################################
library(dplyr)
library(stringr)


# Step 1: List of description variables
desc_vars <- grep("(?i)_Desc", names(data), value = TRUE)

# Step 2: Remove the "E number_" part (e.g., E9_, E10_, etc.)
without_e_number <- str_replace(desc_vars, "E\\d+_", "")

# Step 3: Remove the "_Desc" suffix to get base variable names
base_names <- str_replace(without_e_number, "_Desc", "")


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

base_names

# Step 4: Extract variables that match the base names (but not _Desc)
# cat_vars_try <- data |>
#   select(matches(paste(base_names, collapse = "|")), -contains("_Desc")) |>
#   names()

cat_vars_try <- data |> select(contains(base_names) & -(contains("amt") |
                                                          contains("date") |
                                                          contains("hours"))) |>
  names()

# variables that will not have matches
# the variable is labeled disability, but its description variables are
#   different
# [19] "E43_Primary_Impairment_Desc"
# [20] "E43_Primary_Cause_Desc"
# [21] "E44_Secondary_Impairment_Desc"
# [22] "E44_Secondary_Cause_Desc"
# [29] "E52_Plan_Compensation_Type_Desc"

# E134_Graduate_Comp_Provided_911:

# [62] "E135_Graduate_Comp_1_Desc"
# [63] "E135_Graduate_Comp_2_Desc"
# [64] "E135_Graduate_Comp_3_Desc"

# E141_FourYear_Comp_Provided_911:

# [65] "E142_FourYear_Comp_1_Desc"
# [66] "E142_FourYear_Comp_2_Desc"
# [67] "E142_FourYear_Comp_3_Desc"

# E148_Community_Comp_Provided_911:

# [68] "E149_Community_Comp_1_Desc"
# [69] "E149_Community_Comp_2_Desc"
# [70] "E149_Community_Comp_3_Desc"


# [71] "E156_Vocational_Comp_1_Desc"
# [72] "E156_Vocational_Comp_2_Desc"
# [73] "E156_Vocational_Comp_3_Desc"

# [74] "E163_OJT_Comp_1_Desc"
# [75] "E163_OJT_Comp_2_Desc"
# [76] "E163_OJT_Comp_3_Desc"

# [77] "E169_Apprenticeship_Comp_1_Desc"
# [78] "E169_Apprenticeship_Comp_2_Desc"
# [79] "E169_Apprenticeship_Comp_3_Desc"

# [80] "E176_Basic_Comp_1_Desc"
# [81] "E176_Basic_Comp_2_Desc"
# [82] "E176_Basic_Comp_3_Desc"

# [83] "E183_JobReadiness_Comp_1_Desc"
# [84] "E183_JobReadiness_Comp_2_Desc"
# [85] "E183_JobReadiness_Comp_3_Desc"

# [86] "E190_Disability_Comp_1_Desc"
# [87] "E190_Disability_Comp_2_Desc"
# [88] "E190_Disability_Comp_3_Desc"

# [89] "E197_Miscellaneous_Comp_1_Desc"
# [90] "E197_Miscellaneous_Comp_2_Desc"
# [91] "E197_Miscellaneous_Comp_3_Desc"

# [92] "E204_RandolphSheppard_Comp_1_Desc"
# [93] "E204_RandolphSheppard_Comp_2_Desc"
# [94] "E204_RandolphSheppard_Comp_3_Desc"

# [95] "E211_Custom_Comp_1_Desc"
# [96] "E211_Custom_Comp_2_Desc"
# [97] "E211_Custom_Comp_3_Desc"

# [98] "E218_Assessment_Comp_1_Desc"
# [99] "E218_Assessment_Comp_2_Desc"
# [100] "E218_Assessment_Comp_3_Desc"

# [101] "E225_Diagnosis_Comp_1_Desc"
# [102] "E225_Diagnosis_Comp_2_Desc"
# [103] "E225_Diagnosis_Comp_3_Desc"

# [104] "E232_Counseling_Comp_1_Desc"
# [105] "E232_Counseling_Comp_2_Desc"
# [106] "E232_Counseling_Comp_3_Desc"

# [107] "E239_JobSearch_Comp_1_Desc"
# [108] "E239_JobSearch_Comp_2_Desc"
# [109] "E239_JobSearch_Comp_3_Desc"

# [110] "E246_JobPlacement_Comp_1_Desc"
# [111] "E246_JobPlacement_Comp_2_Desc"
# [112] "E246_JobPlacement_Comp_3_Desc"

# [113] "E253_ShortTerm_Comp_1_Desc"
# [114] "E253_ShortTerm_Comp_2_Desc"
# [115] "E253_ShortTerm_Comp_3_Desc"

# [116] "E260_OnJobSE_Comp_1_Desc"
# [117] "E260_OnJobSE_Comp_2_Desc"
# [118] "E260_OnJobSE_Comp_3_Desc"

# [119] "E267_Referral_Comp_1_Desc"
# [120] "E267_Referral_Comp_2_Desc"
# [121] "E267_Referral_Comp_3_Desc"

# [122] "E274_Benefits_Comp_1_Desc"
# [123] "E274_Benefits_Comp_2_Desc"
# [124] "E274_Benefits_Comp_3_Desc"

# [125] "E281_CustomEmploy_Comp_1_Desc"
# [126] "E281_CustomEmploy_Comp_2_Desc"
# [127] "E281_CustomEmploy_Comp_3_Desc"

# [128] "E293_Transportation_Comp_1_Desc"
# [129] "E293_Transportation_Comp_2_Desc"
# [130] "E293_Transportation_Comp_3_Desc"

# [131] "E300_Maintenance_Comp_1_Desc"
# [132] "E300_Maintenance_Comp_2_Desc"
# [133] "E300_Maintenance_Comp_3_Desc"

# [134] "E307_RehabTech_Comp_1_Desc"
# [135] "E307_RehabTech_Comp_2_Desc"
# [136] "E307_RehabTech_Comp_3_Desc"

# [137] "E314_Attendant_Comp_1_Desc"
# [138] "E314_Attendant_Comp_2_Desc"
# [139] "E314_Attendant_Comp_3_Desc"

# [140] "E321_TechAssist_Comp_1_Desc"
# [141] "E321_TechAssist_Comp_2_Desc"
# [142] "E321_TechAssist_Comp_3_Desc"

# [143] "E328_Reader_Comp_1_Desc"
# [144] "E328_Reader_Comp_2_Desc"
# [145] "E328_Reader_Comp_3_Desc"

# [146] "E335_Interpreter_Comp_1_Desc"
# [147] "E335_Interpreter_Comp_2_Desc"
# [148] "E335_Interpreter_Comp_3_Desc"

# [149] "E342_Other_Comp_1_Desc"
# [150] "E342_Other_Comp_2_Desc"
# [151] "E342_Other_Comp_3_Desc"

# [162] "E378_PostExit_Credential_Desc"
# [163] "E395_App_Medicaid_Desc"
# [164] "E395_App_Medicare_Desc"
# [165] "E395_App_Exchange_Insurance_Desc"
# [166] "E395_App_Public_Insurance_Desc"
# [167] "E395_App_Employer_Insurance_Desc"
# [168] "E395_App_Employer_Insurance_Pending_Desc"
# [169] "E395_App_Private_Insurance_Desc"
# [170] "E397_Exit_Medicaid_Desc"
# [171] "E397_Exit_Medicare_Desc"
# [172] "E397_Exit_Exchange_Insurance_Desc"
# [173] "E397_Exit_Public_Insurance_Desc"
# [174] "E397_Exit_Employer_Insurance_Desc"
# [175] "E397_Exit_Employer_Insurance_Pending_Desc"
# [176] "E397_Exit_Private_Insurance_Desc"
# [177] "E407_Work_Based_Comp_1_Desc"
# [178] "E407_Work_Based_Comp_2_Desc"
# [179] "E407_Work_Based_Comp_3_Desc"
# [180] "E62_Plan_Long_Term_Unemployed_Desc"


cat_vars_try <- data |> select(contains(base_names) & -(contains("amt") |
                                                          contains("date") |
                                                          contains("hours"))) |>
  names()

# Step 5: Remove E number and extra parts from the remaining variables to be
#   able to compare the variables extracted and the d

cat_vars_try2 <- cat_vars_try |> str_replace("E\\d+_", "") |>
  str_replace("_911", "")

setdiff(base_names, cat_vars_try2)

# Step 6: Handle special cases manually (if needed)
# Example: Add manual mapping for special cases
manual_mappings <- c(
  "Primary_Impairment" = "E43_Primary_Disability_911",  # Example of special mapping
  "Primary_Cause" = "E43_Primary_Disability_911",       # Map to primary disability
  "Secondary_Impairment" = "E44_Secondary_Disability",  # Manual adjustment
  "Secondary_Cause" = "E44_Secondary_Disability"
  # Add more special mappings as needed
)

# Combine automatic matching with manual mappings
final_matches <- cat_vars_try2

# Integrate the manual mappings
for (name in names(manual_mappings)) {
  final_matches <- c(final_matches, manual_mappings[name])
}

# Step 7: Check for unmatched variables
unmatched <- setdiff(base_names, final_matches)
print("Unmatched variables:")
print(unmatched)


