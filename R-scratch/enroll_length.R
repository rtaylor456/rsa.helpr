data_merged <- read.csv("data-raw/data_merged.csv")

id_counts_over_time <- data_merged |>
  # mutate(across(c(E1_Year_911, E2_Quarter_911), as.numeric)) |>
  # group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(Overall_Quarter = ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)) |>
  select(Participant_ID, E1_Year_911, E2_Quarter_911, Overall_Quarter)

View(id_counts_over_time)

unique(id_counts_over_time$Overall_Quarter) # make sure we have roughly
# 13 quarters represented

id_enroll_length <- id_counts_over_time |>
  group_by(Participant_ID) |>
  summarise(min_Overall_Quarter = min(Overall_Quarter),
            max_Overall_Quarter = max(Overall_Quarter)) |>
  mutate(enroll_length = max_Overall_Quarter -
           min_Overall_Quarter + 1) |>
  pull(enroll_length)

# hist(id_enroll_length, freq = FALSE, breaks = seq(0.5, 13.5,
#                                                   by = 1))
barplot(table(id_enroll_length),
        main = "Distribution of Program Enrollment Length, per ID",
        xlab = "Enrollment length by quarters (max is 13)")


