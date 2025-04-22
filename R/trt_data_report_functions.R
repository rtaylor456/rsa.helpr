library(data.table)

summarize_scores_formatted <- function(data, robust_measures = TRUE,
                                       save_output = FALSE,
                                       file_name = "summary_scores.csv") {
  # Convert to data.table if not already
  dt <- as.data.table(data)

  # Extract service acronyms from column names
  services <- unique(gsub("^(Pre_Score|Post_Score|Difference)_", "",
                          grep("^(Pre_Score|Post_Score|Difference)_[A-Z]+$",
                               names(dt), value = TRUE)))

  # Create an empty list to store results
  summary_list <- list()

  # Loop through each service and calculate summary statistics
  for (service in services) {
    pre_col <- paste0("Pre_Score_", service)
    post_col <- paste0("Post_Score_", service)
    diff_col <- paste0("Difference_", service)

    if (all(c(pre_col, post_col, diff_col) %in% names(dt))) {
      # Compute standard and robust measures
      pre_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[pre_col]], na.rm = TRUE),
                             sd(dt[[pre_col]], na.rm = TRUE))
      post_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[post_col]], na.rm = TRUE),
                              sd(dt[[post_col]], na.rm = TRUE))
      diff_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[diff_col]], na.rm = TRUE),
                              sd(dt[[diff_col]], na.rm = TRUE))

      # Count valid (non-NA) entries
      pre_count <- sum(!is.na(dt[[pre_col]]))
      post_count <- sum(!is.na(dt[[post_col]]))
      diff_count <- sum(!is.na(dt[[diff_col]]))

      counts_str <- paste0("Pre: ", pre_count, "; Post: ", post_count,
                           "; Diff: ", diff_count)

      if (robust_measures) {
        pre_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[pre_col]],
                                                        na.rm = TRUE),
                                  IQR(dt[[pre_col]], na.rm = TRUE))
        post_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[post_col]],
                                                         na.rm = TRUE),
                                   IQR(dt[[post_col]], na.rm = TRUE))
        diff_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[diff_col]],
                                                         na.rm = TRUE),
                                   IQR(dt[[diff_col]], na.rm = TRUE))

        summary_list[[service]] <- rbindlist(list(
          data.table(
            Service = service,
            Measure = "Mean (SD)",
            Pre = pre_mean_sd,
            Post = post_mean_sd,
            Diff = diff_mean_sd
          ),
          data.table(
            Service = service,
            Measure = "Median (IQR)",
            Pre = pre_median_iqr,
            Post = post_median_iqr,
            Diff = diff_median_iqr
          ),
          data.table(
            Service = service,
            Measure = "Counts",
            Pre = pre_count,
            Post = post_count,
            Diff = diff_count
          )
        ))
      } else {
        summary_list[[service]] <- rbindlist(list(
          data.table(
            Service = service,
            Measure = "Mean (SD)",
            Pre = pre_mean_sd,
            Post = post_mean_sd,
            Diff = diff_mean_sd
          ),
          data.table(
            Service = service,
            Measure = "Counts",
            Pre = pre_count,
            Post = post_count,
            Diff = diff_count
          )
        ))
      }
    }
  }

  # Combine into a final table
  summary_table <- rbindlist(summary_list)

  # Save as CSV if requested
  if (save_output) {
    fwrite(summary_table, file = file_name)
    message("Summary table saved as: ", file_name)
  }

  return(summary_table)
}

# Usage
# For all TRT scores data
summary_table <- summarize_scores_formatted(scores_clean,
                                            robust_measures = TRUE)
summary_table

# For metadata
summary_table_metadata <- summarize_scores_formatted(metadata,
                                                     robust_measures = TRUE)
summary_table_metadata

# For some PSE enrollment
pse_enrolled <- metadata[E84_PostSecondary_Enrollment_911 %in% c(1, 2, 3)]
summary_pse_enrolled <- summarize_scores_formatted(pse_enrolled,
                                                   robust_measures = TRUE)
summary_pse_enrolled

pse_not_enrolled <- metadata[E84_PostSecondary_Enrollment_911 == 0]
summary_pse_not_enrolled <- summarize_scores_formatted(pse_not_enrolled,
                                                   robust_measures = TRUE)
summary_pse_not_enrolled





# For PSE completion
pse_complete <- metadata[E86_PostSecondary_Partial_Completion_911 == 1]
summary_pse_complete <- summarize_scores_formatted(pse_complete,
                                                   robust_measures = TRUE)
summary_pse_complete

pse_not_complete <- metadata[E86_PostSecondary_Partial_Completion_911 %in%
                               c(0, "NULL")]
summary_pse_not_complete <- summarize_scores_formatted(pse_not_complete,
                                                       robust_measures = TRUE)
summary_pse_not_complete

## Econ. Marginalized
# For E65_Plan_Homeless_911, E67_Plan_Low_Income_911, E63_Plan_Exhaust_TANF_911
econ_marginalized <- metadata2[
  E65_Plan_Homeless_911 == 1 |
    E67_Plan_Low_Income_911 == 1 |
    E63_Plan_Exhaust_TANF_911 == 1]

summary_econ_marginalized <- summarize_scores_formatted(econ_marginalized,
                                                   robust_measures = TRUE)
summary_econ_marginalized


not_marginalized <- metadata2[
  E65_Plan_Homeless_911 == 0 &
    E67_Plan_Low_Income_911 == 0 &
    E63_Plan_Exhaust_TANF_911 == 0]

summary_not_marginalized <- summarize_scores_formatted(not_marginalized,
                                                        robust_measures = TRUE)
summary_not_marginalized


## Skills Deficient
skills_deficient <- metadata[E69_Plan_Skills_Deficient_911 == 1]

summary_skills_deficient <- summarize_scores_formatted(skills_deficient,
                                                        robust_measures = TRUE)
summary_skills_deficient



not_skills_deficient <- metadata[E69_Plan_Skills_Deficient_911 == 0]

summary_not_skills_deficient <- summarize_scores_formatted(not_skills_deficient,
                                                       robust_measures = TRUE)
summary_not_skills_deficient

combine_skill_deficient_summaries <- function(skills_df, not_skills_df) {
  # Ensure data.table
  library(data.table)
  skills_df <- as.data.table(skills_df)
  not_skills_df <- as.data.table(not_skills_df)

  # Rename columns
  setnames(skills_df, old = c("Pre", "Post", "Diff"),
           new = c("Pre_skills_deficient", "Post_skills_deficient",
                   "Diff_skills_deficient"))

  setnames(not_skills_df, old = c("Pre", "Post", "Diff"),
           new = c("Pre_not_skills_deficient", "Post_not_skills_deficient",
                   "Diff_not_skills_deficient"))

  # Merge datasets on Service and Measure
  merged <- merge(
    skills_df,
    not_skills_df,
    by = c("Service", "Measure"),
    all = TRUE
  )

  # Reorder columns
  setcolorder(merged, c(
    "Service", "Measure",
    "Pre_skills_deficient", "Pre_not_skills_deficient",
    "Post_skills_deficient", "Post_not_skills_deficient",
    "Diff_skills_deficient", "Diff_not_skills_deficient"
  ))

  return(merged)
}

combined_summary <- combine_skill_deficient_summaries(summary_skills_deficient,
                                                      summary_not_skills_deficient)



## Cultural Barriers
cultural_barriers <- metadata[
  E70_Plan_Cultural_Barriers_911 == 1 |
    E68_Plan_English_Learner_911 == 1 ]

summary_cultural_barriers <- summarize_scores_formatted(cultural_barriers,
                                                       robust_measures = TRUE)
summary_cultural_barriers

no_cultural_barriers <- metadata[
  E70_Plan_Cultural_Barriers_911 == 0 &
    E68_Plan_English_Learner_911 == 0]

summary_no_cultural_barriers <- summarize_scores_formatted(no_cultural_barriers,
                                                        robust_measures = TRUE)
summary_no_cultural_barriers


combine_summaries <- function(var_df, not_var_df, var_name, var_name2) {
  # Ensure data.table
  library(data.table)
  var_df <- as.data.table(var_df)
  not_var_df <- as.data.table(not_var_df)

  # Rename columns
  setnames(var_df, old = c("Pre", "Post", "Diff"),
           new = c(paste0("Pre_", var_name), paste0("Post_", var_name),
                   paste0("Diff_", var_name)))

  setnames(not_var_df, old = c("Pre", "Post", "Diff"),
           new = c(paste0("Pre_", var_name2), paste0("Post_", var_name2),
                   paste0("Diff_", var_name2)))

  # Merge datasets on Service and Measure
  merged <- merge(
    var_df,
    not_var_df,
    by = c("Service", "Measure"),
    all = TRUE
  )

  # Reorder columns
  setcolorder(merged, c(
    "Service", "Measure",
    paste0("Pre_", var_name), paste0("Pre_", var_name2),
    paste0("Post_", var_name), paste0("Post_", var_name2),
    paste0("Diff_", var_name), paste0("Diff_", var_name2)
  ))

  return(merged)
}

combined_summary <- combine_summaries(summary_cultural_barriers,
                                      summary_no_cultural_barriers,
                                      "Cult_Bar", "No_Cult_Bar")

combined_summary


## Disability Priority
priority_1 <- metadata[E45_Disability_Priority_911 == 1]

summary_priority_1 <- summarize_scores_formatted(priority_1,
                                                        robust_measures = TRUE)
summary_priority_1

priority_2 <- metadata[E45_Disability_Priority_911 == 2]

summary_priority_2 <- summarize_scores_formatted(priority_2,
                                                 robust_measures = TRUE)
summary_priority_2


combined_summary <- combine_summaries(summary_priority_1,
                                      summary_priority_2,
                                      "Priority 1", "Priority 2")

combined_summary


################################################################################

### VISUALIZATIONS

library(ggplot2)
library(tidyr)
library(dplyr)

# Assuming summary_pse_enrolled and summary_pse_not_enrolled are already created
# Combine both subsets of data into one data frame
pse_enrolled$Enrollment_Status <- "Enrolled"
pse_not_enrolled$Enrollment_Status <- "Not Enrolled"

# Combine both data sets
combined_data <- bind_rows(pse_enrolled, pse_not_enrolled)

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  select(Enrollment_Status, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
         Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
         Difference_WBLE, Difference_WSS) %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Service", values_to = "Score")

# Create the density plot with dark blue and light blue
ggplot(combined_data_long, aes(x = Score, fill = Enrollment_Status)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c("Enrolled" = "#1D3557", "Not Enrolled" = "#A8DADC")) +  # Dark blue and light blue
  theme_minimal() +
  labs(title = "Density Plot for Difference Scores by Enrollment Status",
       x = "Score", y = "Density", fill = "Enrollment Status") +
  theme(legend.position = "top")




# Assuming summary_econ_marginalized and summary_not_marginalized are already created

# Add Enrollment Status labels
econ_marginalized$Economic_Status <- "Marginalized"
not_marginalized$Economic_Status <- "Not Marginalized"

# Combine both data sets
combined_data <- bind_rows(econ_marginalized, not_marginalized)

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  select(Economic_Status, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
         Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
         Difference_WBLE, Difference_WSS) %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Service", values_to = "Score")

# Create the density plot with dark blue and light blue
ggplot(combined_data_long, aes(x = Score, fill = Economic_Status)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c("Marginalized" = "#1D3557", "Not Marginalized" = "#A8DADC")) +  # Dark blue and light blue
  theme_minimal() +
  labs(title = "Density Plot for Difference Scores by Economic Status",
       x = "Score", y = "Density", fill = "Economic Status") +
  theme(legend.position = "top")


# Assuming summary_pse_complete and summary_pse_not_complete are already created

# Add Completion Status labels
pse_complete$Completion_Status <- "Completed"
pse_not_complete$Completion_Status <- "Not Completed"

# Combine both data sets
combined_data <- bind_rows(pse_complete, pse_not_complete)

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  select(Completion_Status, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
         Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
         Difference_WBLE, Difference_WSS) %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Service", values_to = "Score")

# Create the density plot with dark blue and light blue
ggplot(combined_data_long, aes(x = Score, fill = Completion_Status)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c("Completed" = "#1D3557", "Not Completed" = "#A8DADC")) +  # Dark blue and light blue
  theme_minimal() +
  labs(title = "Density Plot for Difference Scores by PSE Completion Status",
       x = "Score", y = "Density", fill = "Completion Status") +
  theme(legend.position = "top")

############


# Add Completion Status labels
skills_deficient$Skills_Status <- "Skills Deficient"
not_skills_deficient$Skills_Status <- "Not Skills Deficient"

# Combine both data sets
combined_data <- bind_rows(skills_deficient, not_skills_deficient)

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  select(Skills_Status, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
         Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
         Difference_WBLE, Difference_WSS) %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Service", values_to = "Score")

# Create the density plot with dark blue and light blue
ggplot(combined_data_long, aes(x = Score, fill = Skills_Status)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c("Skills Deficient" = "#1D3557",
                               "Not Skills Deficient" = "#A8DADC")) +  # Dark blue and light blue
  theme_minimal() +
  labs(title = "Density Plot for Difference Scores by Skills Deficient Status",
       x = "Score", y = "Density", fill = "Skills Status") +
  theme(legend.position = "top")



########

# Add Completion Status labels
cultural_barriers$Status <- "Cultural Barriers"
no_cultural_barriers$Status <- "No Cultural Barriers"

# Combine both data sets
combined_data <- bind_rows(cultural_barriers, no_cultural_barriers)

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  select(Status, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
         Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
         Difference_WBLE, Difference_WSS) %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Service", values_to = "Score")

# Create the density plot with dark blue and light blue
ggplot(combined_data_long, aes(x = Score, fill = Status)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c("Cultural Barriers" = "#1D3557",
                               "No Cultural Barriers" = "#A8DADC")) +  # Dark blue and light blue
  theme_minimal() +
  labs(title = "Density Plot for Difference Scores by Cultural Barriers Status",
       x = "Score", y = "Density", fill = "Status") +
  theme(legend.position = "top")



########

# Add Completion Status labels
priority_1$Priority <- "Priority 1"
priority_2$Priority <- "Priority 2"

# Combine both data sets
combined_data <- bind_rows(priority_1, priority_2)

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  select(Priority, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
         Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
         Difference_WBLE, Difference_WSS) %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Service", values_to = "Score")

# Create the density plot with dark blue and light blue
ggplot(combined_data_long, aes(x = Score, fill = Priority)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c("Priority 1" = "#1D3557",
                               "Priority 2" = "#A8DADC")) +  # Dark blue and light blue
  theme_minimal() +
  labs(title = "Density Plot for Difference Scores by Disability Priority",
       x = "Score", y = "Density", fill = "Priority") +
  theme(legend.position = "top")

###########

# Assuming combined_data already includes the Age_Group column
# and contains all the data from both priority_1 and priority_2

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  select(Age_Group, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
         Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
         Difference_WBLE, Difference_WSS) %>%
  pivot_longer(cols = starts_with("Difference_"), names_to = "Service", values_to = "Score")

# Create the density plot with color for Age_Group
ggplot(combined_data_long, aes(x = Score, fill = Age_Group)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c(
    # "11-13" = "#1d3557",
    "14-16" = "#457b9d",
    "17-19" = "#a8dadc",
    "20-22" = "#f1faee"
  )) +
  theme_minimal() +
  labs(title = "Density Plot for Difference Scores by Age Group",
       x = "Score", y = "Density", fill = "Age Group") +
  theme(legend.position = "top")


### Pre

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  filter(Age_Group %in% c("14-16", "17-19", "20-22")) %>%
  select(Age_Group, Pre_Score_CPSO, Pre_Score_CSS, Pre_Score_FL, Pre_Score_ILOM,
         Pre_Score_ISA, Pre_Score_JOBEX, Pre_Score_JS,
         Pre_Score_WBLE, Pre_Score_WSS) %>%
  pivot_longer(cols = starts_with("Pre_"), names_to = "Service", values_to = "Score")

# Create the density plot with color for Age_Group
ggplot(combined_data_long, aes(x = Score, fill = Age_Group)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c(
    # "11-13" = "#1d3557",
    "14-16" = "#457b9d",
    "17-19" = "#a8dadc",
    "20-22" = "#f1faee"
  )) +
  theme_minimal() +
  labs(title = "Density Plot for Pre Scores by Age Group",
       x = "Score", y = "Density", fill = "Age Group") +
  theme(legend.position = "top")



### Post

# Reshape the data to long format for easier plotting
combined_data_long <- combined_data %>%
  filter(Age_Group %in% c("14-16", "17-19", "20-22")) %>%
  select(Age_Group, Post_Score_CPSO, Post_Score_CSS, Post_Score_FL, Post_Score_ILOM,
         Post_Score_ISA, Post_Score_JOBEX, Post_Score_JS,
         Post_Score_WBLE, Post_Score_WSS) %>%
  pivot_longer(cols = starts_with("Post_"), names_to = "Service", values_to = "Score")

# Create the density plot with color for Age_Group
ggplot(combined_data_long, aes(x = Score, fill = Age_Group)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
  scale_fill_manual(values = c(
    # "11-13" = "#1d3557",
    "14-16" = "#457b9d",
    "17-19" = "#a8dadc",
    "20-22" = "#f1faee"
  )) +
  theme_minimal() +
  labs(title = "Density Plot for Post Scores by Age Group",
       x = "Score", y = "Density", fill = "Age Group") +
  theme(legend.position = "top")

