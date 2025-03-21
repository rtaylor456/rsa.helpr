### BEST PLOT WITH BEST AXES LABELS
library(ggplot2)
library(gridExtra)
library(grid)

# List of difference variables
difference_vars <- c("Difference_CPSO", "Difference_CSS", "Difference_FL",
                     "Difference_ILOM", "Difference_ISA", "Difference_JOBEX",
                     "Difference_JS", "Difference_QWEX", "Difference_WBLE",
                     "Difference_WSS")

# Generate the plots
plots <- lapply(difference_vars, function(var) {
  # Extract the acronym by removing "Difference_"
  acronym <- sub("Difference_", "", var)

  ggplot(metadata, aes_string(x = "Time_Passed_Days_CPSO", y = var)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear trend line
    labs(title = acronym) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),  # Remove individual x-axis labels
          axis.title.y = element_blank())  # Remove individual y-axis labels
})

# Arrange plots in a grid
plot_grid <- arrangeGrob(grobs = plots, nrow = 2, ncol = 5)

# Create common X and Y labels
x_label <- textGrob("Time Passed (Days)", gp = gpar(fontsize = 16))
y_label <- textGrob("Difference Score", gp = gpar(fontsize = 16), rot = 90)

# Combine everything in a single layout with x-axis label at the bottom
final_plot <- arrangeGrob(
  y_label, plot_grid, nullGrob(),  # Left: Y label, Middle: Plots, Right: Empty space
  ncol = 3, widths = c(1, 10, 1),  # Give space for y label
  bottom = x_label                  # X label at the bottom
)

# Render the final layout
grid.newpage()
grid.draw(final_plot)
