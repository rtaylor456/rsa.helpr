## To match Allison's values, but for metadata:

library(tidyverse)


## RACE
race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                  names(metadata),
                  value = TRUE, perl = TRUE)

# proportions <- metadata |> select(any_of(race_cols)) |>
#   lapply(function(x) round(table(x) / nrow(metadata), 2))

race_proportions <- metadata |>
  select(any_of(race_cols)) |>
  lapply(function(x) round(sum(x == 1) / nrow(metadata), 2))

race_proportions <- race_proportions[order(unlist(race_proportions),
                                           decreasing = TRUE)]

race_proportions
# $E14_White_911
# [1] 0.92
#
# $E15_Hispanic_Latino_911
# [1] 0.14
#
# $E12_Black_African_911
# [1] 0.04
#
# $E10_Indian_Alaskan_911
# [1] 0.03
#
# $E11_Asian_911
# [1] 0.02
#
# $E13_Hawaiian_Pacific_Islander_911
# [1] 0.02

## SKILLS DEFICIENT
skills_def_col <- grep("(?i)(skills).*?(def)(?!.*(?i)_desc)",
                     names(metadata), value = TRUE, perl = TRUE)

skills_def_count <- sum(metadata[[skills_def_col]] == 1, na.rm = TRUE)
# 141

## ECONOMICALLY MARGINALIZED
low_inc_col <- grep("(?i)(low).*?(inc)(?!.*(?i)_desc)",
                       names(metadata), value = TRUE, perl = TRUE)

low_inc_count <- sum(metadata[[low_inc_col]] == 1, na.rm = TRUE)

homeless_col <- grep("(?i)(homeless)(?!.*(?i)_desc)",
                    names(metadata), value = TRUE, perl = TRUE)

homeless_count <- sum(metadata[[homeless_col]] == 1, na.rm = TRUE)

tanf_col <- grep("(?i)(plan).*?(tanf)(?!.*(?i)_desc)",
                 names(metadata), value = TRUE, perl = TRUE)

# to capture any order:
tanf_col <- grep("(?i)(tanf.*?plan|plan.*?tanf)(?!.*(?i)_desc)",
                 names(metadata), value = TRUE, perl = TRUE)

tanf_count <- sum(metadata[[tanf_col]] == 1, na.rm = TRUE)

econ_marginalized <- low_inc_count + homeless_count + tanf_count
# 210 -- different than Allison's 197...


## FOSTER YOUTH
foster_col <- grep("(?i)(foster)(?!.*(?i)_desc)",
                     names(metadata), value = TRUE, perl = TRUE)

foster_count <- sum(metadata[[foster_col]] == 1, na.rm = TRUE)
# 35

## ANY CULTURAL BARRIERS
cult_bar_col <- grep("(?i)(cult).*?(barrier)(?!.*(?i)_desc)",
                     names(metadata), value = TRUE, perl = TRUE)

cultural_count <- sum(metadata[[cult_bar_col]] == 1, na.rm = TRUE)
# 30

eng_learn_col <- grep("(?i)(english).*?(learn)(?!.*(?i)_desc)",
                      names(metadata), value = TRUE, perl = TRUE)

eng_learn_count <- sum(metadata[[eng_learn_col]] == 1, na.rm = TRUE)

any_cled <- cultural_count + eng_learn_count
# 49 -- different than Allison's 47

## DISABILITY PRIORITY
dis_priority_col <- grep("(?i)(disability).*?(priority)(?!.*(?i)_desc)",
                      names(metadata), value = TRUE, perl = TRUE)

round(table(metadata[[dis_priority_col]]) / nrow(metadata), 3)
# 7.9% Priority 1, 36.9% Priority 2 -- Different from Allison's values


################################################################################
##

rsa.helpr::visualize_densities(cat_var = metadata$E84_PostSecondary_Enrollment_911,
                               num_var = metadata$Median_Time_Passed_Days,
                               cat_var_name = "Post_Sec_Enroll",
                               num_var_name = "Median Days in Program",
                               main = "Time in Program across Post_Sec_Enroll")

rsa.helpr::visualize_densities(cat_var = metadata$E84_PostSecondary_Enrollment_911,
                               num_var = metadata$E359_Exit_Hourly_Wage_911,
                               cat_var_name = "Post_Sec_Enroll",
                               num_var_name = "Wage ($ per hour)",
                               main = "Exit Wages across Post_Sec_Enroll")

rsa.helpr::visualize_densities(cat_var = metadata$E84_PostSecondary_Enrollment_911,
                               num_var = metadata$Median_Difference_Score,
                               cat_var_name = "Post_Sec_Enroll",
                               num_var_name = "Median Difference Score",
                               main = "Difference Scores across Post_Sec_Enroll")


plot(metadata$Median_Time_Passed_Days, metadata$Median_Difference_Score)

plot(metadata$Time_Passed_Days_CPSO, metadata$Difference_CPSO)


# library(ggplot2)
# library(gridExtra)
#
# # List of difference variables
# difference_vars <- c("Difference_CPSO", "Difference_CSS", "Difference_FL",
#                      "Difference_ILOM", "Difference_ISA", "Difference_JOBEX",
#                      "Difference_JS", "Difference_QWEX", "Difference_WBLE",
#                      "Difference_WSS")
#
# # Generate the plots
# plots <- lapply(difference_vars, function(var) {
#   # Extract the acronym by removing "Difference_"
#   acronym <- sub("Difference_", "", var)
#
#   ggplot(metadata, aes_string(x = "Time_Passed_Days_CPSO", y = var)) +
#     geom_point(alpha = 0.6, color = "blue") +
#     geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear trend line
#     labs(title = acronym, x = "Time Passed (Days)", y = acronym) +
#     theme_minimal()
# })
#
# # Arrange the plots in a grid (2 rows x 5 columns)
# grid.arrange(grobs = plots, nrow = 2, ncol = 5)


#
# library(ggplot2)
# library(gridExtra)
# library(grid)
#
# # List of difference variables
# difference_vars <- c("Difference_CPSO", "Difference_CSS", "Difference_FL",
#                      "Difference_ILOM", "Difference_ISA", "Difference_JOBEX",
#                      "Difference_JS", "Difference_QWEX", "Difference_WBLE",
#                      "Difference_WSS")
#
# # Generate the plots
# plots <- lapply(difference_vars, function(var) {
#   # Extract the acronym by removing "Difference_"
#   acronym <- sub("Difference_", "", var)
#
#   ggplot(metadata, aes_string(x = "Time_Passed_Days_CPSO", y = var)) +
#     geom_point(alpha = 0.6, color = "blue") +
#     geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear trend line
#     labs(title = acronym) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank(),  # Remove individual x-axis labels
#           axis.title.y = element_blank())  # Remove individual y-axis labels
# })
#
# # Arrange plots in a grid
# plot_grid <- arrangeGrob(grobs = plots, nrow = 2, ncol = 5)
#
# # Create common X and Y labels
# x_label <- textGrob("Time Passed (Days)", gp = gpar(fontsize = 16))
# y_label <- textGrob("Difference Score", gp = gpar(fontsize = 16), rot = 90)
#
# # Arrange everything properly with extra space for labels
# grid.arrange(y_label, plot_grid, ncol = 2, widths = c(1, 10))  # Left-aligned y-axis
# grid.arrange(plot_grid, x_label, nrow = 2, heights = c(10, 1))  # Bottom-aligned x-axis

################################################################################

# library(ggplot2)
# library(gridExtra)
# library(grid)
#
# # List of difference variables
# difference_vars <- c("Difference_CPSO", "Difference_CSS", "Difference_FL",
#                      "Difference_ILOM", "Difference_ISA", "Difference_JOBEX",
#                      "Difference_JS", "Difference_QWEX", "Difference_WBLE",
#                      "Difference_WSS")
#
# # Generate the plots
# plots <- lapply(difference_vars, function(var) {
#   # Extract the acronym by removing "Difference_"
#   acronym <- sub("Difference_", "", var)
#
#   ggplot(metadata, aes_string(x = "Time_Passed_Days_CPSO", y = var)) +
#     geom_point(alpha = 0.6, color = "blue") +
#     geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add linear trend line
#     labs(title = acronym) +
#     theme_minimal() +
#     theme(axis.title.x = element_blank(),  # Remove individual x-axis labels
#           axis.title.y = element_blank())  # Remove individual y-axis labels
# })
#
# # Arrange plots in a grid
# plot_grid <- arrangeGrob(grobs = plots, nrow = 2, ncol = 5)
#
# # Create common X and Y labels
# x_label <- textGrob("Time Passed (Days)", gp = gpar(fontsize = 16))
# y_label <- textGrob("Difference Score", gp = gpar(fontsize = 16), rot = 90)
#
# # Combine everything in a single layout to avoid overwriting
# final_plot <- arrangeGrob(
#   y_label, plot_grid, nullGrob(),  # Y label, plots, empty space (to balance grid)
#   ncol = 3, widths = c(1, 10, 1),  # Give space for y label
#   top = x_label                     # X label at the top
# )
#
# # Render the final layout
# grid.newpage()
# grid.draw(final_plot)


################################################################################

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



## CONTINGENCY TABLE
# Create the contingency table
contingency_table <- table(metadata$E84_PostSecondary_Enrollment_911,
                           metadata$E109_Enrollment_Provided_911)
colnames(contingency_table) <- c("Couns. NOT provided", "Couns. provided")
rownames(contingency_table) <- c("None", "Not for credit", "Technical train.",
                                 "Post-second. ed.")
contingency_table


## WITH TOTALS
# Add row and column totals
contingency_table_with_totals <- addmargins(contingency_table)

# Rename the total rows and columns for clarity
rownames(contingency_table_with_totals)[nrow(contingency_table_with_totals)] <- "Total"
colnames(contingency_table_with_totals)[ncol(contingency_table_with_totals)] <- "Total"

contingency_table_with_totals

# 0.03726708 --> just 4% of metadata participants are receiving post-second.
#   enrollment counseling service.

## PROPORTIONS
# Convert the contingency table to proportions
contingency_proportions <- round(prop.table(contingency_table,
                                      margin = 2), 3) # Proportions by column (counseling: 0 vs 1)

# Print the proportions
contingency_proportions


# Convert the contingency table to proportions
contingency_proportions2 <- round(prop.table(contingency_table,
                                            margin = 1), 3) # Proportions by row (counseling: 0 vs 1)

# Print the proportions
contingency_proportions2



