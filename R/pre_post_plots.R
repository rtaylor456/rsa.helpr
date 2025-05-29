##########################################



scale_fill_manual(
  values = c("Pre" = "#A8DADC", "Post" = "#1D3557"),
  breaks = c("Pre", "Post")  # sets the order of the legend
)


library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape and filter out QWEX
df_long <- scores_clean %>%
  pivot_longer(
    cols = starts_with("Pre_Score_") | starts_with("Post_Score_"),
    names_to = c("Time", "Service"),
    names_pattern = "(Pre|Post)_Score_(.*)",
    values_to = "Score"
  ) %>%
  filter(Service != "QWEX")

# Plot with fixed y-axis and reordered legend
ggplot(df_long, aes(x = Score, fill = Time)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Service, scales = "fixed") +
  scale_fill_manual(
    values = c("Pre" = "#A8DADC", "Post" = "#1D3557"),
    breaks = c("Pre", "Post")  # set legend order
  ) +
  labs(title = "Pre vs Post Score Densities by Service",
       x = "Score",
       y = "Density",
       fill = "Time") +
  theme_minimal()




########################


library(dplyr)
library(tidyr)
library(ggplot2)

# Add a unique participant ID (if not already present)
df <- has_dates %>%
  mutate(Participant_ID = row_number())

# Reshape to long format and filter out QWEX
df_long <- df %>%
  pivot_longer(
    cols = starts_with("Pre_Score_") | starts_with("Post_Score_"),
    names_to = c("Time", "Service"),
    names_pattern = "(Pre|Post)_Score_(.*)",
    values_to = "Score"
  ) %>%
  filter(Service != "QWEX")

# Line plot: one line per participant per service
ggplot(df_long, aes(x = Time, y = Score, group = Participant_ID)) +
  geom_line(alpha = 0.3, color = "gray40") +
  geom_point(aes(color = Time), size = 1.5) +
  facet_wrap(~ Service, scales = "fixed") +
  scale_color_manual(values = c("Pre" = "skyblue", "Post" = "orange")) +
  labs(
    title = "Pre to Post Score Changes by Participant",
    x = "Time",
    y = "Score"
  ) +
  theme_minimal()



library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)

# 1. Reshape your difference columns to long format
df_diff_long <- scores_clean %>%
  pivot_longer(
    cols = starts_with("Difference_"),
    names_to = "Service",
    names_prefix = "Difference_",
    values_to = "Difference"
  ) %>%
  filter(Service != "QWEX")


# 2. Plot
ggplot(df_diff_long, aes(x = Service, y = Difference)) +
  geom_quasirandom(alpha = 0.5, size = 1.5, color = "#1D3557") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(
    breaks = seq(-100, 100, by = 20)  # adjust range and spacing as needed
  ) +
  labs(
    title = "Difference (Post - Pre) Scores by Service",
    # subtitle = "Positive = improvement; Negative = decline",
    y = "Post - Pre Score",
    x = "Service"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

# 1. Reshape and exclude QWEX
df_diff_long <- scores_clean %>%
  pivot_longer(
    cols = starts_with("Difference_"),
    names_to = "Service",
    names_prefix = "Difference_",
    values_to = "Difference"
  ) %>%
  filter(Service != "QWEX")

# Order services alphabetically
df_diff_long$Service <- factor(
  df_diff_long$Service,
  levels = rev(sort(unique(df_diff_long$Service)))
)

# 2. Plot
ggplot(df_diff_long, aes(x = Difference, y = Service)) +
  geom_density_ridges(
    fill = "#1D3557",   # uniform fill color
    alpha = 0.7,
    color = "gray30",
    scale = 0.9,             # reduces vertical overlap
    rel_min_height = 0.01    # trims low tails
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(
    breaks = seq(-100, 100, by = 20)  # adjust range and spacing as needed
  ) +
  labs(
    title = "Difference (Post - Pre) Scores by Service",
    # subtitle = "Positive = improvement; Negative = decline",
    x = "Post - Pre Score",
    y = "Service"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
