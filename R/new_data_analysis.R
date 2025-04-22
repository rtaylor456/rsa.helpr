library(data.table)

# grab full, uncleaned dataset

#


# Subset rows where E7_Application_Date_911 is NA and count by year/quarter
counts <- overlap_data[is.na(E7_Application_Date_911),
                       .N,
                       by = .(E1_Year_911, E2_Quarter_911)]

# View the result
counts

