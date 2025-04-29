data <- data.table::fread("data-raw/data_load_2025-04-22.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)
scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")


# number of unique IDs in TRT from Utah: 2,312
length(unique(scores$`Participant ID`[scores$State %in% c("Utah", "UT",
                                                          "utah", "UTAH", "ut")]))


overlap_ids <- as.factor(data$Participant_ID)[as.factor(data$Participant_ID) %in%
                                                scores_clean$Participant_ID]

length(unique(overlap_ids)) # 2229 unique IDs in RSA-911 data that have TRT data

## RSA-911 quarterly data with TRT data
overlap_data <- data[as.factor(data$Participant_ID) %in% overlap_ids, ]
length(unique(overlap_data$Participant_ID)) # make sure we have the 2229 ids
# 2229

# clean_overlap <- clean_utah(overlap_data)

overlap_data[, E7_Application_Date_911_NEW :=
               handle_mixed_date(E7_Application_Date_911)]


overlap_data$Age <- ifelse(
  is.na(overlap_data$`Age at Application`),
  overlap_data$Age.at.Application,
  overlap_data$`Age at Application`
)

overlap_data[, Age := as.numeric(Age)]

overlap_data_corrected <- overlap_data[(Age >= 14 &
                                      Age <= 22) |
                                     is.na(Age), ]

dim(overlap_data_corrected)


# Overlapping data that have dates -- 525, which is already more than we had
#   originally.
have_dates <- overlap_data[!is.na(E7_Application_Date_911_NEW), ]
dim(have_dates)
length(unique(have_dates$Participant_ID)) # 525


# have_dates$Age <- ifelse(
#   is.na(have_dates$`Age at Application`),
#   have_dates$Age.at.Application,
#   have_dates$`Age at Application`
# )
#
# have_dates[, Age := as.numeric(Age)]


have_dates_corrected <- have_dates[(Age >= 14 &
                                      Age <= 22) |
                                 is.na(Age), ]

dim(have_dates_corrected)
length(unique(have_dates_corrected$Participant_ID)) # 506

#######
# Get the counts of missing dates per combination of year and quarter
is_missing <- function(x) {
  is.na(x) |
    is.null(x) |
    trimws(x) == "" |
    tolower(trimws(x)) %in% c("na", "null")
}

setDT(overlap_data)  # if it's not already a data.table

overlap_data[, missing_flag := is_missing(E7_Application_Date_911)]


###
table(have_dates$E1_Year_911, have_dates$E2_Quarter_911)

setDT(have_dates)  # if it isn't already a data.table
have_dates[, .N, by = .(E1_Year_911, E2_Quarter_911)]

# instead, get ratios of full dates
overlap_data[, .(
  ratio_with_dates = sum(!missing_flag) / .N
), by = .(E1_Year_911, E2_Quarter_911)]

# ratios for missing dates
overlap_data[, .(
  ratio_without_dates = sum(missing_flag) / .N
), by = .(E1_Year_911, E2_Quarter_911)]

#########

# Do the same for full dataset
is_missing <- function(x) {
  is.na(x) |
    is.null(x) |
    trimws(x) == "" |
    tolower(trimws(x)) %in% c("na", "null")
}

setDT(data)  # if it's not already a data.table

data[, missing_flag := is_missing(E7_Application_Date_911)]

have_dates_full <- data[missing_flag == FALSE]
dim(have_dates_full)

have_dates_full[, .N, by = .(E1_Year_911, E2_Quarter_911)]

# instead, get ratios of full dates
data[, .(
  ratio_with_dates = sum(!missing_flag) / .N
), by = .(E1_Year_911, E2_Quarter_911)]

# ratios for missing dates
data[, .(
  ratio_without_dates = sum(missing_flag) / .N
), by = .(E1_Year_911, E2_Quarter_911)]

#######


# agecols_to_check <- c("Age at Application", "Age.at.Application")

# Make sure the columns exist
cols_present <- intersect(names(have_dates), agecols_to_check)

setDT(have_dates)
# Apply filter: any of the age columns (that exist) are in range or NA
have_dates_corrected <- have_dates[
  apply(.SD, 1, function(row) {
    any(is.na(row) | (as.numeric(row) >= 14 & as.numeric(row) <= 22))
  }),
  .SDcols = cols_present
]




check <- overlap_data[is.na(E7_Application_Date_911_NEW), ]
dim(check) # 14828   522 <-- remember, we haven't condensed/aggregated yet


## Of these quarterly rows, find out how many are missing analysis variables
# Extract names for WIOA columns
wioa_cols <- grep("^E(6[2-9]|7[0-3])_", names(overlap_data), value = TRUE)
# Remove any that contain "desc" (case-insensitive)
wioa_cols <- wioa_cols[!grepl("desc", wioa_cols,
                                      ignore.case = TRUE)]

race_cols <- grep(paste0("(?i)(_indian|_asian|_black|_hawaiian|_islander|",
                         "_white|hispanic)(?!.*(?i)_desc)"),
                  names(overlap_data),
                  value = TRUE, perl = TRUE)

cols_to_check <- c("E7_Application_Date_911_NEW", "E9_Gender_911",
                   "E45_Disability_Priority_911",
                   "Age", race_cols,
                   wioa_cols)


lapply(check[, ..cols_to_check], table)

# Count number of missing values per row
check[, missing_count := apply(.SD, 1, function(row) {
  sum(sapply(row, function(x) {
    is.na(x) || is.null(x) || x %in% c("NULL", "NA", "NaN", "")
  }))
}), .SDcols = cols_to_check]

length(cols_to_check)

sort(table(check$missing_count), decreasing = TRUE)
# all are missing all 16

check
