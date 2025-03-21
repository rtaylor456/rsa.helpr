set.seed(3478) # For reproducibility
random_numbers <- sample(100000:999999, 3, replace = TRUE)
random_numbers

Participant_ID <- c(288941, 288941, 288941, 288941,
                    234057,
                    477753, 477753, 477753) # 100245, 100526, 101567
# remove
E1_Year_911 <- c(2022, 2022, 2022, 2023,
                 2020,
                 2022, 2022, 2023)
# remove
E2_Quarter_911 <- c(2, 3, 4, 1,
                    1,
                    3, 4, 1)

# mode
E14_White_911 <- as.factor(c(1, 1, 9, 1,
                   0,
                   1, 1, 1))
# mode
E45_Disability_Priority_911 <- as.factor(c(NA, 1, 1, 1,
                                 1,
                                 NA, 2, 2))
# 1: sig. disability, 2: most sig., no sig.


# mode
# E103_WorkLearning_Provided_911 <- c(0, 0, 0, 1,
#                                     0,
#                                     1, 0, 0)
# # provided by VR agency or staff
# E104_WorkLearning_Purchased_911 <- c(0, 1, 1, 0,
#                                      0,
#                                      0, 1, 0)
# # provided through purchased by VR agency
# E105_WorkLearning_Vendor_911 <- c(0, 2, 2, 0,
#                                   0,
#                                   0, 4, 0)
# # type of provider

# most recent
E38_Eligibility_Date_911 <- as.Date(c(NA, "2021-12-10", "2022-08-20", "2023-03-13",
                              "2019-09-27",
                              NA, "2022-01-19", "2023-04-26"))
# date of eligibility determination
# most recent
# E88_Bachelor_Date_911 # date

# median
# E106_WorkLearning_TitleI_911 #(median)
# median
E359_Exit_Hourly_Wage_911 <- c(0, 8, 8.5, 10,
                               11,
                               0, 0, 0)
# median
# E360_Exit_Weekly_Hours_Worked_911 <- c(0, 10, 20, 10,
#                                        10,
#                                        0, 0, 0)


# vars <- c(Participant_ID, E1_Year_911, E2_Quarter_911,
#           E14_White_911, E45_Disability_Priority_911,
#           E103_WorkLearning_Provided_911, E104_WorkLearning_Purchased_911,
#           E105_WorkLearning_Vendor_911,
#           E38_Eligibility_Date_911,
#           E359_Exit_Hourly_Wage_911,
#           E360_Exit_Weekly_Hours_Worked_911)

example <- data.frame(Participant_ID, E1_Year_911, E2_Quarter_911,
                      E14_White_911,
                      E45_Disability_Priority_911,
                      E38_Eligibility_Date_911,
                      E359_Exit_Hourly_Wage_911)

example <- as.data.table(example)
View(example)

# create metadata using same methods
# numeric
double_cols <- "E359_Exit_Hourly_Wage_911"
example[, (double_cols) := lapply(.SD, function(x) median(x, na.rm = TRUE)),
     .SDcols = double_cols, by = Participant_ID]
# dates
date_cols <- "E38_Eligibility_Date_911"
example[, (date_cols) := lapply(.SD,
                             function(x) as.Date(ifelse(all(is.na(unique(x))),
                                                        NA,
                                                        max(x, na.rm = TRUE)))),
     .SDcols = date_cols, by = Participant_ID]

# factors
factor_cols <- c("E14_White_911", "E45_Disability_Priority_911")

get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
example[, (factor_cols) := lapply(.SD, get_mode), .SDcols = factor_cols,
     by = Participant_ID]

example[, c("E1_Year_911", "E2_Quarter_911") := NULL]

condensed <- example[, lapply(.SD, first), by = Participant_ID]
View(condensed)






##############################


vars <- c("Participant_ID", "E1_Year_911", "E2_Quarter_911",
          "E14_White_911", "E45_Disability_Priority_911",
          "E103_WorkLearning_Provided_911",
          "E38_Eligibility_Date_911",
          "E360_Exit_Weekly_Hours_Worked_911")

# Identify Participant_IDs with multiple rows
duplicates <- merged_data$Participant_ID[duplicated(
  merged_data$Participant_ID) | duplicated(merged_data$Participant_ID,
                                           fromLast = TRUE)]

# Extract rows with those Participant_IDs
duplicate_rows <- merged_data[merged_data$Participant_ID %in% duplicates,

                              ..vars]

duplicate_rows
View(duplicate_rows)
