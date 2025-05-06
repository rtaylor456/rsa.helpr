library(ggplot2)
library(tidyr)
library(dplyr)

compare_densities <- function(subset1, subset2, variable, label1, label2,
                              score_type = "Difference",
                              exclude_qwex = TRUE) {

  # Restrict allowed score_type values
  allowed_types <- c("Difference", "Pre_Score", "Post_Score")

  if (!(score_type %in% allowed_types)) {
    stop(paste("Invalid score_type. Must be one of:", paste(allowed_types,
                                                            collapse = ", ")))
  }

  if (score_type == "Difference") score_var_name <- "Difference"
  if (score_type == "Pre_Score") score_var_name <- "Pre"
  if (score_type == "Post_Score") score_var_name <- "Post"

  # Assuming summary_pse_enrolled and summary_pse_not_enrolled are already created
  # Combine both subsets of data into one data frame
  subset1$Status <- label1
  subset2$Status <- label2

  # Combine both data sets
  combined_data <- bind_rows(subset1, subset2)

  # score_var <- names(combined_data)[grepl(paste0("^", score_type, "_"),
  #                                         names(combined_data))]

  score_vars <- names(combined_data)[startsWith(names(combined_data),
                                               paste0(score_type, "_"))]

  if (exclude_qwex) {
    score_vars <- score_vars[!grepl("_QWEX", score_vars)]
  }

  # Reshape the data to long format for easier plotting
  combined_data_long <- combined_data %>%
    select(Status, score_vars) %>%
    pivot_longer(cols = starts_with(paste0(score_type,"_")),
                 names_to = "Service",
                 values_to = "Score")

  # Create the density plot with dark blue and light blue
  ggplot(combined_data_long, aes(x = Score, fill = Status)) +
    geom_density(alpha = 0.6) +
    facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
    scale_fill_manual(values = c("#1D3557","#A8DADC")) +  # Dark blue and light blue
    theme_minimal() +
    labs(title = paste0("Density Plot for ", score_var_name, " Scores by ",
                        variable),
         x = "Score", y = "Density", fill = "Status") +
    theme(legend.position = "top")

}

compare_densities(pse_enrolled, pse_not_enrolled, variable = "Enrollment",
                  label1 = "Enrolled", label2 = "Not Enrolled")
