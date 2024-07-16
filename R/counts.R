counts <- metadata |>
  summarise(across(starts_with("Difference_"), ~sum(!is.na(.))))
View(counts)


# Convert to long format
counts_long <- counts %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Count")

# Print long tibble
print(counts_long)


counts_pre <- metadata |>
  summarise(across(starts_with("Pre_Score_"), ~sum(!is.na(.))))
View(counts_pre)


# Convert to long format
counts_pre_long <- counts_pre %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Count")

# Print long tibble
print(counts_pre_long)



counts_post <- metadata |>
  summarise(across(starts_with("Post_Score_"), ~sum(!is.na(.))))
View(counts_post)


# Convert to long format
counts_post_long <- counts_post %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Count")

# Print long tibble
print(counts_post_long)


