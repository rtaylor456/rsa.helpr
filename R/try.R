data |> select(-(contains("_date") |
                   contains("_skill_gain") |
                   contains("_desc") |
                   contains("_provide") |
                   contains("purchase") |
                   contains("amt") |
                   contains("title") |
                   contains("vendor") |
                   contains("wage") |
                   contains("hour") |
                   contains("comp") |
                   contains("start") | contains("extension") | contains("_end_")|
                   contains("code") |
                   contains("employment")
                 )) |>
  select(-matches("^(E1[0-6]_|(E42|E49|E5[4-9]|E6[2-7]|E7[0-3])_)")) |>
  names()

data |> select(-(contains("_date") |
                   contains("_skill_gain") |
                   contains("_desc") |
                   contains("_provide") |
                   contains("purchase") |
                   contains("amt") |
                   contains("title") |
                   contains("vendor") |
                   contains("wage") |
                   contains("hour") |
                   contains("comp") |
                   contains("start") | contains("extension") | contains("_end_")|
                   contains("code") |
                   contains("employment")
                 )) |>
  select(-matches("^(E1[0-6]_|(E42|E49|E5[4-9]|E6[2-7]|E7[0-3])_)")) |>
  # select(contains("employment")) |>
  lapply(unique)



# remaining variables description:

# Participant_ID -- codes, factors - nominal
# E51_Plan_Occupation_911 - codes, NA, factor, nominal
# E21_Referral_Source_911 - codes with Nulls, factor nominal - values 01-32
# E357_Exit_Occupation_911 - codes with NULLs and NA - factor nominal
#   special values: 899999 and 999999 -- randolph shephard employees

# `Age at Application`- numeric

# E1_Year_911 - numeric, discrete
#   "2020" "2021" "2022" "2023"

# E2_Quarter_911 - numeric, discrete
#    "1" "2" "3" "4"



# E22_SWD_911 - 0, 1, 2, 3 - student with disability
#  0: not student with disability
# factor - nominal?
#  potentially ordinal...

# E43_Primary_Disability_911
# E44_Secondary_Disability_911 - 2 values, separated by semicolons
# E74_SWD_Age_911 - one value, "14;21" and NA

# semicolon separation--ranges of number of values
# E394_App_Public_Support_911 - 0, 1-4
# E395_App_Medical_911 - 0, 2, 3, 5, 6
# E396_Exit_Public_Support_911 - 0, 1-4
# E397_Exit_Medical_911 - 0, 1-7

# E50_Plan_Work_Status_911 - 1-10
#    1: competitive employment, 2-5: employment, 6: termination, 7: not employed
#    factor--ordinal?
# E77_Plan_Grade_Level_911 - 1-12 for highest grade completed by individual
#   numeric discrete

# VR_Case_Type_Flag -- 1, 0 -- not sure, probably nominal factor

# E61_YouthBuild_911 - NULL, NA - 1 if received services, leave blank if not
#   factor

# E9_Gender_911 - 1,2,9, NULL - factor

# E60_Youth_911 - 0, 1, NULL
# E68_Plan_English_Learner_911 - 0, 1, NULL (should be 9)
# E69_Plan_Skills_Deficient_911 - 0, 1, 9
# E392_Q2_Q4_Employer_Match_911 - 0, 1, NULL (should just be 0 or 1)
# E400_Secondary_Equivalent_Enrollment_911 - 0, 1, NULL (should just be 0 or 1)

# E45_Disability_Priority_911 - 0, 1, 2, NULL (null means something different
#                                               than 0)
# E78_Secondary_Enrollment_911 - 0, 1,2, NULL (Nulls --> 0)

# E355_Exit_Reason_911 - 2-19, NULL -- 02, 03, 04, 06, 07, 08, 13-22


# E84_PostSecondary_Enrollment_911 - 0, 1, 2, 3, NULL (null -> 0)
#   factor, possibly ordinal 0, 3, 2, 1 --> better


# E354_Exit_Type_911 - 0, 3, 4, 6, 7, NULL -- 1, 2, 3, 4, 5, 6, 7, 0
#   potentially ordinal factor

# E356_Exit_Work_Status_911 - 1, 2, 4, 5, NULL -- 1, 2, 3, 4, 5, 7
#   1: competitive employment

# E378_PostExit_Credential_911 - 1-7, NULL -- 1-8 factor, potentially ordinal

# variables with "data" in title, probably safe to ignore










# OLD work
data_merged <- readRDS("data-raw/data_merged.rds")

py20 <- read.csv("data-raw/original/PY20exit.csv")

py21 <- read.csv("data-raw/original/PY21exit.csv")

data_cleaned <- readRDS("data-raw/data_cleaned.rds")
View(data_cleaned)

names(data)
names(py20)
names(py21)
View(data_merged[c(29, 30, 33:36)])

data_cleaned_snippet <- data_cleaned |>
  select(E38_Eligibility_Date_911, E39_Eligibility_Extension_911,
         E42_Has_Disability_911, contains("E43"))

View(data_cleaned_snippet)

intersect(names(data), names(py20))

library(tidyverse)
data <- data_merged |>
  select(-contains("desc"))

# py20 vs utah
# amt spent: "title", "amt" - ut
#


