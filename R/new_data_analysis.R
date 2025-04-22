library(data.table)

# devtools::install_github("rtaylor456/rsa.helpr")


# grab full, uncleaned datasets

directory <- "C:\\Users\\Ruth Taylor\\Box\\911 Data and Related Projects\\911 Data\\Utah Quarterly Data\\USU Data Request"
quarterly <- load_data(directory, download_csv = TRUE)


data <- data.table::fread("data-raw/data_load_2025-04-22.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)
scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")


# Total unique IDs for RSA-911 data
data_unique_ids <- unique(as.factor(data$Participant_ID))
length(data_unique_ids) # 48,802

# Total unique TRT IDs from Utah
length(unique(scores$`Participant ID`[scores$State %in% c("Utah", "UT",
                                                          "utah", "UTAH",
                                                          "ut")]))
# 2312

# (When scores_clean is done using "Utah" state filter)
# Number of unique RSA-911 IDs with matches in Utah TRT data
overlap_ids <- as.factor(data$Participant_ID)[as.factor(data$Participant_ID) %in%
                                                scores_clean$Participant_ID]
length(unique(overlap_ids)) # 2229

overlap_data <- data[as.factor(data$Participant_ID) %in% overlap_ids, ]
# dim(overlap_data) # 18905   521
length(unique(overlap_data$Participant_ID)) # make sure we have the 2229 ids
# 2229

setDT(overlap_data)

# overlap_data[, E1_Year_911 := as.numeric(as.character(E1_Year_911))]
# overlap_data[, E2_Quarter_911 := as.numeric(as.character(E2_Quarter_911))]
#
# # Order and get the most recent record per participant_id
# most_recent <- overlap_data[order(Participant_ID, -E1_Year_911, -E2_Quarter_911),
#                             .SD[1],
#                   by = Participant_ID]

# or...
overlap_data[, year_quarter := as.numeric(E1_Year_911) +
               as.numeric(E2_Quarter_911) / 10]

# Ensure the date column is in date format
overlap_data[, E7_Application_Date_911_NEW :=
               handle_mixed_date(E7_Application_Date_911)]

# most_recent <- overlap_data[order(Participant_ID, -year_quarter), .SD[1],
#                             by = Participant_ID]

# Order by Participant_ID, descending year_quarter, then descending application date
most_recent <- overlap_data[order(Participant_ID, -year_quarter,
                                  -E7_Application_Date_911_NEW),
                            .SD[1],
                            by = Participant_ID]

# make sure we have 2229 rows (and added columns for year_quarter and new date)
dim(most_recent) # 2229  523


check <- most_recent[is.na(E7_Application_Date_911_NEW),]
dim(check)

# # Subset rows where E7_Application_Date_911_NEW is NA and count by year/quarter
# counts <- check[,
#                        .N,
#                        by = .(E1_Year_911, E2_Quarter_911)][order(-N)]
#
# # View the result
# counts
#
# # E1_Year_911   E2_Quarter_911    N
# # 1:        2024              2 1635
# # 2:        2024              1   44
# # 3:        2023              4   15
# # 4:        2023              2   13
# # 5:        2023              1    7
# # 6:        2023              3    7
# # 7:        2022              4    6
# # 8:        2022              3    4
# # 9:        2021              4    2
# # 10:        2021              2    1
# # 11:        2021              3    1
## Not useful for really getting to the bottom of the issue since we've already
#   only kept most recent years and quarters (often 2024, q2)

check_2024_q2 <- read.csv("data-raw/PY24Q2.csv", header = TRUE)
check_2024_q2$E7_Application_Date_911

cols_to_check <- c("E7_Application_Date_911")

setDT(check_2024_q2)
check_2024_q2[, (cols_to_check) := lapply(.SD, function(x) {
  x[x %in% c("NULL", "NA", "NaN", "")] <- NA
  x
}), .SDcols = cols_to_check]

check_2024_q2[, missing_count := rowSums(is.na(.SD)), .SDcols = cols_to_check]

sum(check_2024_q2$missing_count)
