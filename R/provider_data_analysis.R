scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)

provider_data <- clean_provider(scores, state_filter = "Utah")

names(provider_data)


table(scores_clean$Mode)


library(dplyr)
library(ggplot2)
library(forcats)

provider_counts <- scores_clean %>%
  count(Provider) %>%
  filter(n >= 20)

ggplot(provider_counts, aes(x = fct_reorder(Provider, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Providers with at Least 20 Observations",
       x = "Provider",
       y = "Count") +
  theme_minimal()


ggplot(provider_data, aes(x = Provider, fill = Mode)) +
  geom_bar(position = "dodge") +
  labs(title = "Mode Distribution by Provider",
       x = "Provider",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(provider_data, aes(x = Mode, y = Difference)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Score Distribution by Mode",
       x = "Mode",
       y = "Score") +
  theme_minimal()



# Install if not already
# install.packages("ggridges")

library(ggplot2)
library(ggridges)
library(dplyr)

provider_data %>%
  filter(!is.na(Mode)) %>%
  ggplot(aes(x = Difference, y = Mode)) +
  geom_density_ridges(fill = "steelblue", alpha = 0.8, scale = 1, rel_min_height = 0.01) +
  labs(title = "Difference Scores by Mode",
       x = "Difference (Post - Pre) Scores",
       y = "Mode") +
  theme_minimal() +
  theme(legend.position = "none")


provider_counts <- provider_data %>%
  count(Provider) %>%
  filter(n >= 20)

# Step 2: Filter the data to only include those providers
filtered_data <- provider_data %>%
  filter(Provider %in% provider_counts$Provider,
         !is.na(Provider),
         !is.na(Difference))  # ensure no missing Difference scores

# Step 3: Plot density ridges
ggplot(filtered_data, aes(x = Difference, y = fct_reorder(Provider, Difference, .fun = median))) +
  geom_density_ridges(fill = "steelblue", alpha = 0.8, scale = 1, rel_min_height = 0.01) +
  labs(title = "Difference Scores by Provider",
       x = "Difference (Post - Pre) Scores",
       y = "Provider") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(provider_data, aes(x = Group_freq, y = Score, color = Provider)) +
  geom_point() +
  facet_wrap(~ Mode) +
  labs(title = "Score vs Group Frequency Faceted by Mode",
       x = "Group Frequency",
       y = "Score") +
  theme_minimal()


library(ggplot2)

# Example for Mode
ggplot(provider_data, aes(x = Mode, y = Difference)) +
  geom_boxplot() +
  # facet_wrap(~Provider) +
  theme_minimal() +
  labs(title = "Difference Scores by Mode and Provider",
       y = "Score", x = "Mode of Service")





library(dplyr)
library(ggplot2)

# Filter to providers with at least 20 participants
provider_counts <- scores_clean %>%
  count(Provider) %>%
  filter(n >= 20)

# Filter and prepare data
filtered_data <- scores_clean %>%
  filter(Provider %in% provider_counts$Provider,
         !is.na(Provider),
         !is.na(Mode))

# Plot: Stacked bar plot (normalized to proportions)
ggplot(filtered_data, aes(x = fct_reorder(Provider, Mode, .fun = function(x) length(x)), fill = Mode)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Mode Distribution by Provider",
       x = "Provider",
       y = "Proportion",
       fill = "Mode") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





library(dplyr)
library(ggplot2)
library(forcats)

# Filter to providers with at least 20 participants
provider_counts <- scores_clean %>%
  count(Provider) %>%
  filter(n >= 20)

# Prepare data: filter NA Mode, drop unused factor levels
filtered_data <- scores_clean %>%
  filter(Provider %in% provider_counts$Provider,
         !is.na(Provider),
         !is.na(Mode)) %>%
  mutate(Mode = droplevels(factor(Mode)))

# Dynamically assign shades of blue based on number of Mode levels
blue_shades <- RColorBrewer::brewer.pal(n = length(levels(filtered_data$Mode)), name = "Blues")

# Plot
ggplot(filtered_data, aes(x = fct_infreq(Provider), fill = Mode)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = blue_shades) +
  labs(title = "Mode Distribution by Provider",
       x = "Provider",
       y = "Proportion",
       fill = "Mode") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
