library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

metadata$Income_Struggle

metadata$Pre_Date_CPSO


plot_service_densities <- function(data, binary_var) {
  # Find all pre-score variables starting with 'Pre_Date_'
  service_vars <- names(data)[str_starts(names(data), "Pre_Date_")]

  if (length(service_vars) != 10) {
    warning(paste("Found", length(service_vars), "pre variables — expected 10."))
  }

  # Pivot to long format, stripping 'Pre_Date_' prefix for clean service names
  plot_data <- data %>%
    select(all_of(c(binary_var, service_vars))) %>%
    pivot_longer(
      cols = all_of(service_vars),
      names_to = "service",
      values_to = "score"
    ) %>%
    mutate(service = str_remove(service, "^Pre_Date_"))

  # Plot densities
  p <- ggplot(plot_data, aes(x = score, fill = factor(.data[[binary_var]]))) +
    geom_density(alpha = 0.6, na.rm = TRUE) +
    facet_wrap(~ service, scales = "free") +
    labs(
      title = "Comparative Density Plots by Service (Pre Scores)",
      fill = binary_var,
      x = "Pre Score",
      y = "Density"
    ) +
    theme_minimal() +
    theme(legend.position = "top")

  return(p)
}


# Plot comparing by Income_Struggle
p <- plot_service_densities(metadata, binary_var = "Income_Struggle")
p



