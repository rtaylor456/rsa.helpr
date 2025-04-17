################################################################################
################################################################################
# We have Age at Application in RSA-911 and Application Date in RSA-911 data.
# --> first, calculate Birth Year from these values for RSA-911
################################################################################

### Apply clean_utah chunk to convert Application Date to date in R
date_cols <- grep(paste0("(?i)_date|(?i)_skill_gain|(?i)_start|(?i)_end_|",
                         "(?i)_extension(?!.*(?i)_desc)"),
                  names(data), value = TRUE,
                  perl = TRUE)

data[, (date_cols) := lapply(.SD, handle_excel_date), .SDcols = date_cols]

################################################################################

### Apply clean_utah chunk to convert Age to numeric
age_cols <- grep("(?i)^(?=.*age)(?=.*app)(?!.*(desc|amt))", names(data),
                 value = TRUE, perl = TRUE)
# Rename Age column
names(data)[names(data) %in% age_cols] <- "Age_At_Application"

data[, (age_cols) :=
       lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
     .SDcols = "Age_At_Application"]



### Calculate Birth Year
data[, Birth_Year := year(E7_Application_Date_911) - `Age at Application`]


################################################################################
################################################################################

# We have Completed (date) in TRT data.
# --> next, calculate Age at Completion using Birth Year and Completed date
################################################################################
library(lubridate)

# Strip off the timezone in parentheses
scores[, Completed_clean := gsub(" \\(.*\\)", "", Completed)]

# Parse to POSIXct (includes date + time)
scores[, Completed_datetime := mdy_hms(Completed_clean)]

# If you only want the date part
scores[, Completed_date := as.Date(Completed_datetime)]

table(scores$Completed)
class(scores$Completed)
class(scores$Completed_date)

# # Step 1: Remove the timezone part (anything in parentheses)
# check$Completed_clean <- gsub(" \\(.*\\)", "", check$Completed)
#
# # Step 2: Convert to POSIXct datetime
# check$Completed_datetime <- as.POSIXct(check$Completed_clean, format = "%m/%d/%Y %H:%M:%S")
#
# # Step 3 (optional): If you just want the date
# check$Completed_date <- as.Date(check$Completed_datetime)

#### In order to calculate this, I'll have to merge the datasets first--this
#   might involve changing my cleaning and merge functions....



