data_unique_ids <- unique(as.factor(data$Participant_ID))
length(data_unique_ids) # 46,529

# number of unique IDs in TRT from Utah: 2,312
length(unique(scores$`Participant ID`[scores$State %in% c("Utah", "UT",
                                                          "utah", "UTAH", "ut")]))


overlap_ids <- as.factor(data$Participant_ID)[as.factor(data$Participant_ID) %in%
                                                scores_clean$Participant_ID]

length(unique(overlap_ids)) # 2067 unique IDs in RSA-911 data that have TRT data

## Missing Application Date is what is significantly reducing the overlap ids,
#   since clean_utah aggregate = TRUE aggregates based on date and removes rows
#   with missing dates.

overlap_data <- data[as.factor(data$Participant_ID) %in% overlap_ids, ]
length(unique(overlap_data$Participant_ID)) # make sure we have the 2067 ids
# 2067

check <- overlap_data[is.na(E7_Application_Date_911),]

# Extract the years and quarters for those rows with missing dates--see if there
#   are any specific datasets with these missing values
get_datasets <- check[, c("E1_Year_911", "E2_Quarter_911")]
lapply(get_datasets, unique)
# unfortunately, there are not....so we have to dig a little deeper

# Subset rows where E7_Application_Date_911 is NA and count by year/quarter
counts <- overlap_data[is.na(E7_Application_Date_911),
                       .N,
                       by = .(E1_Year_911, E2_Quarter_911)]

# View the result
counts

dim(check)

# find out how many columns contain NULLs, blanks or NAs for each participant in
#   check
check[, missing_count := apply(.SD, 1, function(x) sum(is.na(x) | x == "" |
                                                         is.null(x)))]

# check$missing_count is a vector of the number of columns per participant with
#   missing values
sort(table(check$missing_count), decreasing = TRUE)


# Extract names for WIOA columns
cols_to_check <- grep("^E(6[2-9]|7[0-3])_", names(data), value = TRUE)
# Remove any that contain "desc" (case-insensitive)
cols_to_check <- cols_to_check[!grepl("desc", cols_to_check,
                                      ignore.case = TRUE)]

# cols_to_check <- c("E7_Application_Date_911", "Age at Application",
#                    cols_to_check)

lapply(check[, ..cols_to_check], table)

# Count number of missing values per row for only WIOA columns
check[, missing_count_WIOA := apply(.SD, 1, function(row) {
  sum(sapply(row, function(x) {
    is.na(x) || is.null(x) || x %in% c("NULL", "NA", "NaN", "")
  }))
}), .SDcols = cols_to_check]

length(cols_to_check) # 12 Variables

sort(table(check$missing_count_WIOA), decreasing = TRUE)
# Most are in fact missing all 12 WIOA variables

################################################################################
# NEXT: check the distribution of missings for just 2067 UNIQUE participants.
# need to add the is.na() portion to account for all of the participants with
#   missing application date values
metadata_corrected <- metadata[(Age_At_Application >= 14 &
                                  Age_At_Application <= 22) |
                                 is.na(Age_At_Application), ]

check <- metadata_corrected[is.na(E7_Application_Date_911),]
# find out how many columns contain NULLs, blanks or NAs for each participant in
#   check
check[, missing_count := apply(.SD, 1, function(x) sum(is.na(x) | x == "" |
                                                         is.null(x)))]

# check$missing_count is a vector of the number of columns per participant with
#   missing values
sort(table(check$missing_count), decreasing = TRUE)


# Extract names for WIOA columns
cols_to_check <- grep("^E(6[2-9]|7[0-3])_", names(data), value = TRUE)
# Remove any that contain "desc" (case-insensitive)
cols_to_check <- cols_to_check[!grepl("desc", cols_to_check,
                                      ignore.case = TRUE)]

# cols_to_check <- c("E7_Application_Date_911", "Age at Application",
#                    cols_to_check)

lapply(check[, ..cols_to_check], table)

# Count number of missing values per row for only WIOA columns
check[, missing_count_WIOA := apply(.SD, 1, function(row) {
  sum(sapply(row, function(x) {
    is.na(x) || is.null(x) || x %in% c("NULL", "NA", "NaN", "")
  }))
}), .SDcols = cols_to_check]

length(cols_to_check) # 12 Variables

sort(table(check$missing_count_WIOA), decreasing = TRUE)
# Most are in fact missing all 12 WIOA variables
