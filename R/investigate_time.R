library(tidyverse)
library(lubridate)

scores <- read.csv("data-raw/trt_utah_6_3_2024_4pm.csv")
scores$Completed <- mdy_hms(gsub(" \\(MST\\)", "", scores$Completed))

View(scores)

# Group by participant.id, service, and pre.post, and count the scores
scores2 <- scores |>
  group_by(Participant.ID, Service, Pre.Post) |>
  mutate(count = n()) |>
  ungroup() |>
  mutate(Has_Multiple_Scores = as.factor(if_else(count > 1, 1, 0))) |>
  select(-count) |>
  group_by(Participant.ID, Service) |>
  mutate(Time_Passed_Days = as.numeric(difftime(max(Completed),
                                                min(Completed),
                                                units = "days"))) |>
  mutate(Time_Passed_Days = round(Time_Passed_Days)) |>
  # mutate(Time_Passed_Days = format(Time_Passed_Days, scientific = FALSE)) |>
  ungroup() |>
  arrange(Participant.ID)

View(scores2)


pre_data <- scores2 |>
  filter(Pre.Post == "Pre")
# pre_data

post_data <- scores2 |>
  filter(Pre.Post == "Post")
# post_data


# Keep the earliest 'Pre' score and most recent 'Post' score
earliest_pre <- pre_data |>
  arrange(Completed) |>
  group_by(Participant.ID, Provider, Service, Completed) |>
  slice(1)
# View(earliest_pre)

latest_post <- post_data |>
  arrange(desc(Completed)) |>
  group_by(Participant.ID, Provider, Service) |>
  slice(1)
# View(latest_post)

# merged_data <- full_join(
#   earliest_pre |>
#     select(Participant.ID, Completed, Service, Provider, Has_Multiple_Scores,
#            Time_Passed_Days,
#            Pre_Score = Score),
#   latest_post |>
#     select(Participant.ID, Completed, Service, Provider, Has_Multiple_Scores,
#            Time_Passed_Days,
#            Post_Score = Score,
#            Difference = Difference),
#   by = c("Participant.ID", "Service", "Provider")
#   )



# Merge the data
merged_data <- full_join(
  earliest_pre %>%
    select(Participant.ID,
           Completed_Pre = Completed,
           Service, Provider,
           Has_Multiple_Scores,
           Time_Passed_Days,
           Pre_Score = Score),
  latest_post %>%
    select(Participant.ID,
           Completed_Post = Completed,
           Service, Provider,
           Has_Multiple_Scores,
           Time_Passed_Days,
           Post_Score = Score,
           Difference),
  by = c("Participant.ID", "Service", "Provider",
         "Has_Multiple_Scores", "Time_Passed_Days")
)


View(merged_data)

# relationship = "many-to-many"

# scores2 <- scores |>
#   group_by(Participant.ID, Provider, Service) |>
#   mutate()


# final_data <- merged_data |>
#   pivot_wider(
#     names_from = Service,
#     values_from = c(Pre_Score, Post_Score, Difference),
#     names_glue = "{.value}_{Service}"
#   )

final_data <- merged_data |>
  pivot_wider(
    names_from = Service,
    values_from = c(Completed_Pre, Completed_Post, Pre_Score, Post_Score,
                    Difference,
                    Time_Passed_Days),
    names_glue = "{.value}_{Service}"
  )

View(final_data)


# write_csv(final_data, file = "data-raw/scores_clean_NEW.csv")
#
# scores_clean <- read.csv("data-raw/scores_clean_NEW.csv")
# View(scores_clean)



# use data_clean_all_steps_FINAL.R file to get metadata

metadata_clean <- metadata |>
  # mutate(Median_Time_Passed_Days = ifelse(Median_Time_Passed_Days < 1, NA,
  #                                         Median_Time_Passed_Days)) |>
  mutate(across(contains("Time_Passed"), ~ ifelse(. == 0, NA, .)))



time_model <- lm(Median_Difference_Score ~ Median_Time_Passed_Days +
                   Has_Multiple_Scores,
                 data = metadata_clean)
summary(time_model)

plot(metadata_clean$Median_Difference_Score ~ metadata_clean$Median_Time_Passed_Days,
     main = "Median Difference Scores Across Time in Program",
     xlab = "Median Days in Program",
     ylab = "Median Difference Score")
plot(metadata_clean$Median_Difference_Score ~ metadata_clean$Has_Multiple_Scores)



all_differences <- metadata_clean |>
  select(contains("Difference") & -contains(c("Median", "Available"))) |>
  names()
all_differences

all_times <- metadata_clean |>
  select(contains("Time_Passed_") & -contains("Median")) |>
  names()
all_times

# CPSO
# time_model_CPSO <- lm(Difference_CPSO ~ Time_Passed_Days_CPSO +
#                    Has_Multiple_Scores,
#                  data = metadata_clean)
# summary(time_model_CPSO)

# ISA
time_model_ISA <- lm(Difference_ISA ~ Time_Passed_Days_ISA +
                        Has_Multiple_Scores,
                      data = metadata_clean)
summary(time_model_ISA)

# JOBEX
time_model_JOBEX <- lm(Difference_JOBEX ~ Time_Passed_Days_JOBEX +
                       Has_Multiple_Scores,
                     data = metadata_clean)
summary(time_model_JOBEX)

# JS
time_model_JS <- lm(Difference_JS ~ Time_Passed_Days_JS +
                         Has_Multiple_Scores,
                       data = metadata_clean)
summary(time_model_JS)

# CSS
time_model_CSS <- lm(Difference_CSS ~ Time_Passed_Days_CSS +
                      Has_Multiple_Scores,
                    data = metadata_clean)
summary(time_model_CSS)

# EMP
time_model_EMP <- lm(Difference_EMP ~ Time_Passed_Days_EMP +
                       Has_Multiple_Scores,
                     data = metadata_clean)
summary(time_model_EMP)

# FL
time_model_FL <- lm(Difference_FL ~ Time_Passed_Days_FL +
                       Has_Multiple_Scores,
                     data = metadata_clean)
summary(time_model_FL)

# ILOM
# time_model_ILOM <- lm(Difference_ILOM ~ Time_Passed_Days_ILOM +
#                       Has_Multiple_Scores,
#                     data = metadata_clean)
# summary(time_model_ILOM)
#
# plot(metadata_clean$Difference_ILOM ~ metadata_clean$Time_Passed_Days_ILOM)
# plot(metadata_clean$Difference_ILOM ~ metadata_clean$Has_Multiple_Scores)


# WBLE
time_model_WBLE <- lm(Difference_WBLE ~ Time_Passed_Days_WBLE +
                        Has_Multiple_Scores,
                      data = metadata_clean)
summary(time_model_WBLE)

plot(metadata_clean$Difference_WBLE ~ metadata_clean$Time_Passed_Days_WBLE)
plot(metadata_clean$Difference_WBLE ~ metadata_clean$Has_Multiple_Scores)

# QWEX
# time_model_QWEX <- lm(Difference_QWEX ~ Time_Passed_Days_QWEX +
#                         Has_Multiple_Scores,
#                       data = metadata_clean)
# summary(time_model_QWEX)

hist(metadata_clean$Median_Difference_Score)

hist(metadata_clean$Median_Time_Passed_Days,
     main = "Median times passed per participant across all tests",
     xlab = "median times",
     breaks = 40)


