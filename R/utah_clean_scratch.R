
data <- readRDS("data-raw/data_merged.rds")

library(data.table)

setDT(data)
grouped_data <- data[, .(occurrences_per_quarter = .N),
                     by = .(Participant_ID, E1_Year_911, E2_Quarter_911)]

names(grouped_data)

View(grouped_data)
