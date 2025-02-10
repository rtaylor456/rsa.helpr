#' Visualize RSA-911 Metadata
#'
#' This function produces tailored visualizations for RSA and TRT scores
#'   metadata in one step.
#'
#' @param data A cleaned metadata dataset. Apply clean_utah function first.
#' @param option The selected visual analysis option. The options are
#'   "general_demo":
#'   "investigate_scores":
#'   "investigate_wage":
#'   "investigate_employment":
#' @param one_window Whether or not the user wants the visuals to be displayed
#'   in one plotting window, or spread across multiple plot windows. Defaults to
#'   FALSE.
#'
#' @returns The appropriate plots for the chosen visual analysis.
#'
#' @export
#'

visualize_metadata <- function(data, option = c("general_demo",
                                                "investigate_scores",
                                                "investigate_wage",
                                                "investigate_employment"),
                               one_window = FALSE) {

  option <- match.arg(option)

  if (option == "general_demo") {

    if (one_window == TRUE) {par(mfrow = c(3, 3))}

    ## PLOT 1
    hist(data$Median_Time_Passed_Days,
         col = "steelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

    ## PLOT 2
    hist(data$Enroll_Length,
         col = "steelblue",
         main = "Distribution of Enrollment Lengths",
         xlab = "Enrollment Length (Quarters)")

    ## PLOT 3
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    barplot(table(data[[sex_col]]),
            main = "Distribution of Genders",
            xlab = "Gender",
            names = c("Males", "Females", "Other", "Did not identify"),
            col = c("lightsteelblue", "steelblue", "darkblue", "gray"))

    ## PLOT 4
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

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
    race_table <- table(filtered_data$Race)

    # Create the bar plot based on the contingency table
    # par(oma = c(0, 0, 0, 0) + 0.6)
    barplot_heights <- barplot(race_table, beside = TRUE,
                               ylab = "Count",
                               xaxt = "n",   # Disable default x-axis labels
                               yaxt = "n",   # Disable default y-axis labels
                               xlab = "",
                               main = "Distribution of Race", las = 2,
                               col = "steelblue")

    # Add the y-axis
    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    # Add diagonal labels
    text(x = barplot_heights, # Center labels based on barplot positions
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),
         xpd = NA,
         srt = 45,  # Rotate the labels by 45 degrees
         cex = .8,
         adj = c(1, 1))  # Adjust text alignment to center under bars

    ## PLOT 5
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    barplot(table(data[[severity_col]]),
            main = "Distribution of Disability Severity",
            xlab = "Severity",
            names = c("Non-significant", "Significant", "Most significant"),
            col = c("lightsteelblue", "steelblue", "darkblue"))

    ## PLOT 6
    barplot(table(data$Primary_Impairment_Group),
            main = "Distribution of Primary Impairments",
            xlab = "Primary Impairment",
            col = "steelblue")

    ## PLOT 7
    barplot(table(data$Secondary_Impairment_Group),
            main = "Distribution of Secondary Impairments",
            xlab = "Secondary Impairment",
            col = "steelblue")


    if (one_window == TRUE) { par(mfrow = c(1, 1)) }

  } else if (option == "investigate_scores") {

    if (one_window == TRUE) {par(mfrow = c(3, 3))}

    ## PLOT 1
    hist(data$Median_Difference_Score,
         col = "steelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")

    ## PLOT 2
    plot(data$Median_Time_Passed_Days,
         data$Median_Difference_Score,
         main = "Difference Scores Across Time in Program",
         ylab = "Median Difference Scores",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)

    ## PLOT 3
    boxplot(Median_Difference_Score ~ data[["Enroll_Length_Grp"]], data = data,
            main = "Difference Scores Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Median Difference Scores",
            col = "steelblue")

    ## PLOT 4
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # use our function to visualize densities
    visualize_densities(cat_var = data[[sex_col]],
                        num_var = data$Median_Difference_Score,
                        cat_var_name = "Gender",
                        num_var_name = "Median Difference Scores",
                        level_labels = c("Males", "Females",
                                         "Other", "Did not identify"),
                        main = "Difference Scores by Gender",
                        colors = c("steelblue4", "darkblue", "gray"))


    ## PLOT 5

    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    data_subset <- data[, .SD, .SDcols = c("Median_Difference_Score",
                                           race_cols)]

    # Create a long-format data.table
    long_data <- melt(data_subset,
                      id.vars = "Median_Difference_Score",
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")
    # Filter rows where Has_Race is 1
    filtered_data <- long_data[Has_Race == 1]

    # Adjust the outer margins, so that the bottom doesn't get cut off
    # par(oma = c(0, 0, 0, 0) + 0.6)
    boxplot(Median_Difference_Score ~ Race, data = filtered_data,
            # names = gsub("^E[0-9]+_|_911$", "", race_cols),
            col = "steelblue",
            xaxt = "n",
            yaxt = "n",
            xlab = "",
            ylab = "Median Difference Score",
            main = "Difference Scores Across Race"
    )
    # axis(side = 1, labels = FALSE) # this adds in x-axis tick marks
    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    text(x = 1:length(race_cols),
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),
         xpd = NA,
         ## Rotate the labels by 45 degrees.
         srt = 45,
         cex = .8,
         adj = 1)

    ## PLOT 6 -- RUNNING INTO ERRORS...come back to
    # Identify the severity column dynamically
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    # use our function to visualize densities
    visualize_densities(cat_var = data[[severity_col]],
                        num_var = data$Median_Difference_Score,
                        cat_var_name = "Disability Severity",
                        num_var_name = "Median Difference Scores",
                        main = "Difference Scores by Disability Severity",
                        colors = c("steelblue4", "darkblue", "gray"))


    ## PLOT 7
    boxplot(Median_Difference_Score ~
              Primary_Impairment_Group,
            data = data,
            main = "Difference Scores by Primary Disability Type",
            xlab = "Primary Disability",
            ylab = "Median Difference Scores",
            col = "steelblue")

    if (one_window == TRUE) {par(mfrow = c(1, 1))}

  } else if (option == "investigate_wage") {

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    if (one_window == TRUE) {par(mfrow = c(3, 3))}
    ## PLOT 1
    hist(wages_vector,
         col = "steelblue",
         main = "Distribution of Exit Wages",
         xlab = "Exit Wage ($ per hour)")

    ## PLOT 2
    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         wages_vector,
         main = "Exit Wages Across Days in Program",
         ylab = "Exit Wages ($ per hour)",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)

    ## PLOT 3
    boxplot(wages_vector ~ data[["Enroll_Length_Grp"]],
            main = "Exit Wages Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

    # PLOT 4
    # Identify the column for gender/sex
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    visualize_densities(cat_var = data[[sex_col]],
                        num_var = wages_vector,
                        cat_var_name = "Gender",
                        num_var_name = "Exit Wages ($ per Hour)",
                        level_labels = c("Males", "Females",
                                         "Other", "Did not identify"),
                        main = "Exit Wages by Gender",
                        colors = c("lightsteelblue", "steelblue", "darkblue",
                                   "gray"))

    ## PLOT 5
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    data_subset <- data[, .SD, .SDcols = c(wage_col,
                                           race_cols)]

    # Create a long-format data.table
    long_data <- melt(data_subset,
                      id.vars = wage_col,
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")
    # Filter rows where Has_Race is 1
    filtered_data <- long_data[Has_Race == 1]


    # Ensure wage_col has only one column name for the boxplot
    if (length(wage_col) != 1) {
      stop("wage_col should contain exactly one column name.")
    }

    # Extract the wage column name
    wage_col_name <- wage_col[1]

    # Create a formula for the boxplot
    boxplot_formula <- as.formula(paste(wage_col_name, "~ Race"))

    # Plot using the dynamic formula
    # par(oma = c(0, 0, 0, 0) + 0.6)
    boxplot(boxplot_formula, data = filtered_data,
            col = "steelblue",
            xaxt = "n",
            yaxt = "n",
            xlab = "",
            ylab = "Exit Wages ($ per Hour)",
            main = "Exit Wages Across Race"
    )
    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    text(x = 1:length(race_cols),
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),
         xpd = NA,
         ## Rotate the labels by 45 degrees.
         srt = 45,
         cex = .8,
         adj = 1)

    ## PLOT 6
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    visualize_densities(cat_var = data[[severity_col]],
                        num_var = wages_vector,
                        cat_var_name = "Disability Severity",
                        num_var_name = "Exit Wages ($ per Hour)",
                        level_labels = c("Non significant", "Significant",
                                         "Most significant"),
                        main = "Exit Wages by Disability Severity",
                        colors = c("steelblue4", "darkblue", "gray"))


    ## PLOT 7
    boxplot(wages_vector ~ Primary_Impairment_Group,
            data = data,
            main = "Exit Wages by Primary Impairment Type",
            xlab = "Primary Impairment",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

    if (one_window == TRUE) { par(mfrow = c(1, 1)) }

  } else if (option == "investigate_employment") {

    exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

    if (one_window == TRUE) {par(mfrow = c(3, 3))}

    ## PLOT 1
    barplot(table(data$Final_Employment),
            main = "Distribution of Exit Employment",
            names = c("Non-competitive", "Competitive"),
            xlab = "Exit Employment Status",
            col = c("lightsteelblue", "steelblue"))


    ## PLOT 2
    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         as.character(data$Final_Employment),
         main = "Exit Employment Across Time in Program",
         ylab = "Exit Employment",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 8)

    ## PLOT 3
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


    ## PLOT 4
    # the name for the gender/sex column could be varied, so we need to
    #   account for this possibility
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)


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


    ## PLOT 5
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

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

    bar_midpoints <- barplot(employment_race_table, beside = TRUE,
                             col = c("lightsteelblue", "steelblue"),
                             legend.text = c("Non-competitive", "Competitive"),
                             args.legend = list(x = "topleft", bty = "n",
                                                title = "Employment Type"),
                             ylab = "Count",
                             xaxt = "n",   # Disable default x-axis labels
                             yaxt = "n",   # Disable default y-axis labels
                             xlab = "",
                             main = "Final Employment by Race", las = 2)


    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    # Add custom x-axis labels at the midpoints of the bars
    text(x = colMeans(bar_midpoints),  # Calculate the midpoints for grouped bars
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),  # Clean the race names
         xpd = NA,  # Allow plotting outside plot region
         ## Rotate the labels by 45 degrees.
         srt = 45,
         cex = .8,
         adj = 1)


    ## PLOT 6
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    # Create a contingency table of Final_Employment by Gender
    employment_severity_table <- table(data$Final_Employment,
                                       data[[severity_col]])

    rownames(employment_severity_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")

    colnames(employment_severity_table) <- c("Non-significant",
                                             "Significant",
                                             "Most significant")


    # Create a bar plot with bars broken up by gender
    barplot(employment_severity_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topleft", bty = "n",
                               title = "Employment Type"),
            xlab = "Disability Severity", ylab = "Count",
            main = "Exit Employment by Disability Severity")


    ## PLOT 7
    employment_prim_dis_table <- table(data$Final_Employment,
                                       data$Primary_Impairment_Group)

    rownames(employment_prim_dis_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")


    # Create a bar plot with bars broken up by gender
    barplot(employment_prim_dis_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topleft", bty = "n",
                               title = "Employment Type"),
            xlab = "Primary Impairment", ylab = "Count",
            main = "Exit Employment by Primary Impairment",
    )

    if (one_window == TRUE) {par(mfrow = c(1, 1))}

  }

}
