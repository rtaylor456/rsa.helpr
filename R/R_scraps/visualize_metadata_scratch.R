# General demo
# plot #1:
# hist(data$Median_Time_Passed_Days,
#      col = "steelblue",
#      main = "Distribution of Time in Programs",
#      xlab = "Median Time in Program (per individual)")

# plot #2:
# hist(data$Enroll_Length,
#      col = "steelblue",
#      main = "Distribution of Enrollment Lengths",
#      xlab = "Enrollment Length (Quarters)")

# plot #4:
# data_subset <- data[, .SD, .SDcols = c("Final_Employment",
#                                        race_cols)]

# # Create a long-format data.table
# long_data <- melt(data_subset,
#                   id.vars = "Final_Employment",
#                   measure.vars = race_cols,
#                   variable.name = "Race",
#                   value.name = "Has_Race")
# # Filter rows where Has_Race is 1
# filtered_data <- long_data[Has_Race == 1]

# plot #6
# barplot(table(data$Primary_Impairment_Group),
#         main = "Distribution of Primary Impairments",
#         xlab = "Primary Impairment",
#         col = "steelblue")

# barplot(table(data[[prim_impair_grp_col]]),
#         main = "Distribution of Primary Impairments",
#         xlab = "Primary Impairment",
#         col = "steelblue")

# Create the bar plot
# par(mar = c(3, 4, 4, 2) + 0.1)
# barplot(table(data[[prim_impair_grp_col]]),
#         main = "Distribution of Primary Impairments",
#         xlab = "Primary Impairment",
#         col = "steelblue",
#         names = NA,
#         las = 1,  # Ensure the default label orientation is horizontal
#         cex.names = 0.8,)  # Adjust the size of the x-axis labels
#
# Rotate x-axis labels by 45 degrees
# text(x = 1:length(table(data[[prim_impair_grp_col]])),  # Position of labels
#      y = par("usr")[3] - 0.5,  # Place labels slightly below the x-axis
#      labels = names(table(data[[prim_impair_grp_col]])),  # Get the category names
#      srt = 45,  # Rotate by 45 degrees
#      xpd = TRUE,  # Allow text outside the plot area
#      adj = 1,  # Adjust alignment
#      cex = 0.8)  # Adjust the size of the labels


## plot #7
# barplot(table(data$Secondary_Impairment_Group),
#         main = "Distribution of Secondary Impairments",
#         xlab = "Secondary Impairment",
#         col = "steelblue")

# barplot(table(data[[second_impair_grp_col]]),
#         main = "Distribution of Secondary Impairments",
#         xlab = "Secondary Impairment",
#         col = "steelblue")




### Investigate scores
# plot 1
# hist(data$Median_Difference_Score,
#      col = "steelblue",
#      main = "Distribution of Median Difference Scores",
#      xlab = "Median Difference Scores")

# plot 2
# plot(data$Median_Time_Passed_Days,
#      data$Median_Difference_Score,
#      main = "Difference Scores Across Time in Program",
#      ylab = "Median Difference Scores",
#      xlab = "Median Days Spent in Programs (per individual)",
#      col = "steelblue",
#      pch = 3)

# plot 3
# boxplot(Median_Difference_Score ~ data[["Enroll_Length_Grp"]],
#         data = data,
#         main = "Difference Scores Across Quarters Enrolled",
#         xlab = "Enrollment Length (total quarters)",
#         ylab = "Median Difference Scores",
#         col = "steelblue")

# boxplot(data[[median_diff_col]] ~ data[[enroll_len_col]],
#         # data = data,
#         main = "Difference Scores Across Quarters Enrolled",
#         xlab = "Enrollment Length (total quarters)",
#         ylab = "Median Difference Scores",
#         col = "steelblue")


# plot 4
# sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
#                 value = TRUE, perl = TRUE)

# use our function to visualize densities
# visualize_densities(cat_var = data[[sex_col]],
#                     num_var = data$Median_Difference_Score,
#                     cat_var_name = "Gender",
#                     num_var_name = "Median Difference Scores",
#                     level_labels = c("Males", "Females",
#                                      "Other", "Did not identify"),
#                     main = "Difference Scores by Gender",
#                     colors = c("steelblue4", "darkblue", "gray"))


# plot 5
# race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
#                   names(data),
#                   value = TRUE, perl = TRUE)

# data_subset <- data[, .SD, .SDcols = c("Median_Difference_Score",
#                                        race_cols)]

# Create a long-format data.table
# long_data <- melt(data_subset,
#                   id.vars = "Median_Difference_Score",
#                   measure.vars = race_cols,
#                   variable.name = "Race",
#                   value.name = "Has_Race")

# Adjust the outer margins, so that the bottom doesn't get cut off
# par(oma = c(0, 0, 0, 0) + 0.6)
# boxplot(Median_Difference_Score ~ Race, data = filtered_data,
#         # names = gsub("^E[0-9]+_|_911$", "", race_cols),
#         col = "steelblue",
#         xaxt = "n",
#         yaxt = "n",
#         xlab = "",
#         ylab = "Median Difference Score",
#         main = "Difference Scores Across Race"
# )


# plot 6

# Identify the severity column dynamically
# severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
#                      names(data), value = TRUE, perl = TRUE)

# use our function to visualize densities
# visualize_densities(cat_var = data[[severity_col]],
#                     num_var = data$Median_Difference_Score,
#                     cat_var_name = "Disability Severity",
#                     num_var_name = "Median Difference Scores",
#                     main = "Difference Scores by Disability Severity",
#                     colors = c("steelblue4", "darkblue", "gray"))

# plot 7
# boxplot(Median_Difference_Score ~
#           Primary_Impairment_Group,
#         data = data,
#         main = "Difference Scores by Primary Disability Type",
#         xlab = "Primary Disability",
#         ylab = "Median Difference Scores",
#         col = "steelblue")

# plot 8
# boxplot(Median_Difference_Score ~
#           Secondary_Impairment_Group,
#         data = data,
#         main = "Difference Scores by Secondary Disability Type",
#         xlab = "Secondary Disability",
#         ylab = "Median Difference Scores",
#         col = "steelblue")




#### investigate wage

# plot 1
wages <- data[, .SD, .SDcols = wage_col]
wages_vector <- as.vector(unlist(wages))

# hist(wages_vector,
#      col = "steelblue",
#      main = "Distribution of Exit Wages",
#      xlab = "Exit Wage ($ per hour)")

# plot 2
# these variables have been created in the data cleaning process,
#   so we can use the exact names -- I will still make them robust
# plot(data$Median_Time_Passed_Days,
#      wages_vector,
#      main = "Exit Wages Across Days in Program",
#      ylab = "Exit Wages ($ per hour)",
#      xlab = "Median Days Spent in Programs (per individual)",
#      col = "steelblue",
#      pch = 3)


# plot 3
# boxplot(wages_vector ~ data[["Enroll_Length_Grp"]],
#         main = "Exit Wages Across Quarters Enrolled",
#         xlab = "Enrollment Length (total quarters)",
#         ylab = "Exit Wages ($ per Hour)",
#         col = "steelblue")

# plot 6

visualize_densities(cat_var = data[[severity_col]],
                    num_var = wages_vector,
                    cat_var_name = "Disability Severity",
                    num_var_name = "Exit Wages ($ per Hour)",
                    level_labels = c("Non significant", "Significant",
                                     "Most significant"),
                    main = "Exit Wages by Disability Severity",
                    colors = c("steelblue4", "darkblue", "gray"))

# plot 7
boxplot(wages_vector ~ Primary_Impairment_Group,
        data = data,
        main = "Exit Wages by Primary Impairment Type",
        xlab = "Primary Impairment",
        ylab = "Exit Wages ($ per Hour)",
        col = "steelblue")



### investigate employment

# plot 1
barplot(table(data$Final_Employment),
        main = "Distribution of Exit Employment",
        names = c("Non-competitive", "Competitive"),
        xlab = "Exit Employment Status",
        col = c("lightsteelblue", "steelblue"))


# plot 2
plot(data$Median_Time_Passed_Days,
     as.character(data$Final_Employment),
     main = "Exit Employment Across Time in Program",
     ylab = "Exit Employment",
     xlab = "Median Days Spent in Programs (per individual)",
     col = "steelblue",
     pch = 8)

# plot 3
# Create a contingency table of Final_Employment by Gender
employment_enroll_table <- table(data$Final_Employment,
                                 data[["Enroll_Length_Grp"]])

rownames(employment_enroll_table) <- c("Non-competitive Employment",
                                       "Competitive Employment")

colnames(employment_enroll_table) <- c("<5", "5-10", "11+")


# Create a bar plot with bars broken up by gender
barplot(employment_enroll_table, beside = TRUE,
        col = c("lightsteelblue", "steelblue"),
        legend.text = c("Non-competitive", "Competitive"),
        args.legend = list(x = "topright", bty = "n",
                           title = "Employment Type"),
        xlab = "Enrollment Length (total quarters)", ylab = "Count",
        main = "Exit Employment Across Quarters Enrolled")


# plot 4

# Create a contingency table of Final_Employment by Gender
employment_gender_table <- table(data$Final_Employment,
                                 data[[sex_col]])

rownames(employment_gender_table) <- c("Non-competitive Employment",
                                       "Competitive Employment")

colnames(employment_gender_table) <- c("Male",
                                       "Female",
                                       "Other",
                                       "Did not identify")

# Create a bar plot with bars broken up by gender
barplot(employment_gender_table, beside = TRUE,
        col = c("lightsteelblue", "steelblue"),
        legend.text = c("Non-competitive", "Competitive"),
        args.legend = list(x = "topright", bty = "n",
                           title = "Employment Type"),
        xlab = "Gender", ylab = "Count",
        main = "Exit Employment by Gender")


# plot 5
## PLOT 5
data_subset <- data[, .SD, .SDcols = c("Final_Employment",
                                       race_cols)]

# Create a long-format data.table
long_data <- melt(data_subset,
                  id.vars = "Final_Employment",
                  measure.vars = race_cols,
                  variable.name = "Race",
                  value.name = "Has_Race")
# Filter rows where Has_Race is 1
filtered_data <- long_data[Has_Race == 1]


# Create a contingency table of Final_Employment by Gender
employment_race_table <- table(filtered_data$Final_Employment,
                               filtered_data$Race)


# Create the bar plot based on the contingency table
# par(oma = c(0, 0, 0, 0) + 0.6)
# barplot(employment_race_table, beside = TRUE,
#         col = c("lightsteelblue", "steelblue"),
#         legend.text = c("Non-competitive", "Competitive"),
#         args.legend = list(x = "topleft", bty = "n",
#                            title = "Employment Type"),
#         ylab = "Count",
#         xaxt = "n",
#         yaxt = "n",
#         xlab = "",
#         main = "Final Employment by Race", las = 2)
