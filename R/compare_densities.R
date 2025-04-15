library(ggplot2)
library(tidyr)
library(dplyr)

compare_densities <- function(subset1, subset2, variable, label1, label2) {

  # Assuming summary_pse_enrolled and summary_pse_not_enrolled are already created
  # Combine both subsets of data into one data frame
  subset1$Status <- label1
  subset2$Status <- label2

  # Combine both data sets
  combined_data <- bind_rows(subset1, subset2)

  # Reshape the data to long format for easier plotting
  combined_data_long <- combined_data %>%
    select(Status, Difference_CPSO, Difference_CSS, Difference_FL, Difference_ILOM,
           Difference_ISA, Difference_JOBEX, Difference_JS, Difference_QWEX,
           Difference_WBLE, Difference_WSS) %>%
    pivot_longer(cols = starts_with("Difference_"), names_to = "Service",
                 values_to = "Score")

  # Create the density plot with dark blue and light blue
  ggplot(combined_data_long, aes(x = Score, fill = Status)) +
    geom_density(alpha = 0.6) +
    facet_wrap(~ Service, scales = "free", ncol = 5) +  # 10 panels, 5 columns
    scale_fill_manual(values = c("#1D3557","#A8DADC")) +  # Dark blue and light blue
    theme_minimal() +
    labs(title = paste0("Density Plot for Difference Scores by ", variable,
                        " Status"),
         x = "Score", y = "Density", fill = "Status") +
    theme(legend.position = "top")

}

compare_densities(pse_enrolled, pse_not_enrolled, variable = "Enrollment",
                  label1 = "Enrolled", label2 = "Not Enrolled")
