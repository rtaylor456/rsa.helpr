library(tidyverse)

scores_old <- read.csv("data-raw/trt_utah_4_8_2024.csv")

# look at the median differences across each test



subsets <- list()
for (service in scores_old$Service) {
  subsets[[service]] <- scores_old %>% filter(Service == service)
}

# pivot the pre.post column
subset_pivot <- subsets |>
  lapply(distinct) |>
  lapply(function(df) {
    pivot_wider(df, names_from = Pre.Post, values_from = Score,
                names_prefix = "Score_")
  }
  )

# lapply(subset_pivot, nrow)

css <- as.data.frame(pivoted_data[["CSS"]])
View(css)


css_plot <- as.data.frame(pivoted_data[["CSS"]]) |>
  filter(!is.na(Difference)) |>
  mutate(median_diff = median(Difference)) |>
  ggplot(aes(x = Completed)) +
  geom_line(aes(y = median_diff)) +
  geom_point(aes(y = median_diff, color = Participant.ID)) +
  labs(title = "CSS Difference Scores Over Time for Each Participant",
       x = "Completion Date of Test",
       y = "Difference in Scores",
       color = "Participant") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
css_plot





