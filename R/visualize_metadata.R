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

    # Check if the required columns are found

    # Find the column for Median_Time_Passed_Days
    median_time_col <- grep("(?i)(med|median).*?(time|days)(?!.*(?i)_desc)",
                            names(data),
                            value = TRUE, perl = TRUE)

    # Find the column for enrollment length
    enroll_len_col <- grep("(?i)(enroll|enrollment).*?(length|len)(?!.*(?i)_grp)",
                            names(data), value = TRUE, perl = TRUE)

    # Find the column for sex/gender
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # Find the columns for race
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    # Find the column for final employment status
    final_employ_col <- grep("(?i)(final).*?(employ)(?!.*(?i)_desc)",
                         names(data), value = TRUE, perl = TRUE)

    # Find the column for disability severity
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    # Find the column for primary impairment group
    prim_impair_grp_col <- grep("(?i)(prim|primary).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                            names(data), value = TRUE, perl = TRUE)

    # Find the column for secondary impairment group
    second_impair_grp_col <- grep("(?i)(sec|second).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                                names(data), value = TRUE, perl = TRUE)


    # Check if the required columns are found
    if (length(median_time_col) == 0 || length(enroll_len_col) == 0 ||
        length(sex_col) == 0 || length(race_cols) == 0 ||
        length(final_employ_col) == 0 || length(severity_col) == 0 ||
        length(prim_impair_grp_col) == 0 ||
        length(second_impair_grp_col) == 0) {
      stop("Missing required columns for 'general demo' visualization.")
    }

    # Proceed with visualization using the identified columns
    message("Columns identified: ", paste(median_time_col, collapse = ", "),
            "; ",
            paste(enroll_len_col, collapse = ", "), "; ",
            paste(sex_col, collapse = ", "), "; ",
            paste(race_cols, collapse = ", "), "; ",
            paste(final_employ_col, collapse = ", "), "; ",
            paste(severity_col, collapse = ", "), "; ",
            paste(prim_impair_grp_col, collapse = ", "), "; ",
            paste(second_impair_grp_col, collapse = ", "))


    # Set adjusted plotting window if necessary
    if (one_window == TRUE) {par(mfrow = c(3, 3))}

    ## PLOT 1: Time (days) in program
    hist(data[[median_time_col]],
         col = "steelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

    ## PLOT 2: Enrollment length (quarters)
    hist(data[[enroll_len_col]],
         col = "steelblue",
         main = "Distribution of Enrollment Lengths",
         xlab = "Enrollment Length (Quarters)")

    ## PLOT 3: Gender
    barplot(table(data[[sex_col]]),
            main = "Distribution of Genders",
            xlab = "Gender",
            names = c("Males", "Females", "Other", "Did not identify"),
            col = c("lightsteelblue", "steelblue", "darkblue", "gray"))

    ## PLOT 4: Race
    data_subset <- data[, .SD, .SDcols = c(final_employ_col, race_cols)]

    # Create a long-format data.table
    long_data <- melt(data_subset,
                      id.vars = final_employ_col,
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

    ## PLOT 5: Severity
    barplot(table(data[[severity_col]]),
            main = "Distribution of Disability Severity",
            xlab = "Severity",
            names = c("Non-significant", "Significant", "Most significant"),
            col = c("lightsteelblue", "steelblue", "darkblue"))


    ## PLOT 6: Primary Impairment Group
    # Create the bar plot and capture the bar midpoints
    bar_midpoints <- barplot(table(data[[prim_impair_grp_col]]),
                             main = "Distribution of Primary Impairments",
                             xlab = "Primary Impairment",
                             col = "steelblue",
                             names = NA,
                             las = 1,  # Ensure the default label orientation is horizontal
                             cex.names = 0.8)  # Adjust the size of the x-axis labels

    # Rotate and center x-axis labels by 45 degrees
    text(x = bar_midpoints,  # Use the midpoints of the bars for correct alignment
         y = par("usr")[3] - 0.5,  # Place labels slightly below the x-axis
         labels = names(table(data[[prim_impair_grp_col]])),  # Category names
         srt = 45,  # Rotate by 45 degrees
         xpd = TRUE,  # Allow text outside the plot area
         adj = 1,  # Adjust alignment (set to 1 to align to the right)
         cex = 0.8)  # Adjust the size of the labels

    ## PLOT 7: Secondary Impairment Group
    # Create the bar plot and capture the bar midpoints
    bar_midpoints <- barplot(table(data[[second_impair_grp_col]]),
                             main = "Distribution of Secondary Impairments",
                             xlab = "Secondary Impairment",
                             col = "steelblue",
                             names = NA,
                             las = 1,  # Ensure the default label orientation is horizontal
                             cex.names = 0.8)  # Adjust the size of the x-axis labels

    # Rotate and center x-axis labels by 45 degrees
    text(x = bar_midpoints,  # Use the midpoints of the bars for correct alignment
         y = par("usr")[3] - 0.5,  # Place labels slightly below the x-axis
         labels = names(table(data[[second_impair_grp_col]])),  # Category names
         srt = 45,  # Rotate by 45 degrees
         xpd = TRUE,  # Allow text outside the plot area
         adj = 1,  # Adjust alignment (set to 1 to align to the right)
         cex = 0.8)  # Adjust the size of the labels

    # Reset the plot window
    par(mfrow = c(1, 1))

  } else if (option == "investigate_scores") {

    # Check if the required columns are found

    # Find the column for Median_Time_Passed_Days
    median_time_col <- grep("(?i)(med|median).*?(time|days)(?!.*(?i)_desc)",
                            names(data),
                            value = TRUE, perl = TRUE)

    # Find the column for enrollment length
    enroll_len_col <- grep("(?i)(enroll|enrollment).*?(length|len)(?!.*(?i)_grp)",
                            names(data), value = TRUE, perl = TRUE)

    # Find the column for enrollment length group
    enroll_len_grp_col <- grep("(?i)(enroll|enrollment).*?(length|len)*?(_grp)",
                           names(data), value = TRUE, perl = TRUE)

    # Find the column for sex/gender
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # Find the columns for race
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    # Find the column for final employment status
    final_employ_col <- grep("(?i)(final).*?(employ)(?!.*(?i)_desc)",
                         names(data), value = TRUE, perl = TRUE)

    # Find the column for disability severity
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    # Find the column for primary impairment group
    prim_impair_grp_col <- grep("(?i)(prim|primary).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                            names(data), value = TRUE, perl = TRUE)

    # Find the column for secondary impairment group
    second_impair_grp_col <- grep("(?i)(sec|second).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                                names(data), value = TRUE, perl = TRUE)


    # Check if the required columns are found
    if (length(median_time_col) == 0 || length(enroll_len_col) == 0 ||
        length(sex_col) == 0 || length(race_cols) == 0 ||
        length(final_employ_col) == 0 || length(severity_col) == 0 ||
        length(prim_impair_grp_col) == 0 ||
        length(second_impair_grp_col) == 0) {
      stop("Missing required columns for 'investigate scores' visualization.")
    }

    # Proceed with visualization using the identified columns
    message("Columns identified: ", paste(median_time_col, collapse = ", "),
            "; ",
            paste(enroll_len_col, collapse = ", "), "; ",
            paste(sex_col, collapse = ", "), "; ",
            paste(race_cols, collapse = ", "), "; ",
            paste(final_employ_col, collapse = ", "), "; ",
            paste(severity_col, collapse = ", "), "; ",
            paste(prim_impair_grp_col, collapse = ", "), "; ",
            paste(second_impair_grp_col, collapse = ", "))


    # Set adjusted plotting window if necessary
    if (one_window == TRUE) {par(mfrow = c(3, 3))}

    ## PLOT 1
    hist(data[[median_diff_col]],
         col = "steelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")


    ## PLOT 2
    plot(data[[median_time_col]],
         data[[median_diff_col]],
         main = "Difference Scores Across Time in Program",
         ylab = "Median Difference Scores",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)

    ## PLOT 3
    boxplot(data[[median_diff_col]] ~ data[[enroll_len_grp_col]],
            # data = data,
            main = "Difference Scores Across Quarters Enrolled",
            xlab = "Enrollment Length Group (total quarters)",
            ylab = "Median Difference Scores",
            col = "steelblue")

    ## PLOT 4
    visualize_densities(cat_var = data[[sex_col]],
                        num_var = data[[median_diff_col]],
                        cat_var_name = "Gender",
                        num_var_name = "Median Difference Scores",
                        level_labels = c("Males", "Females",
                                         "Other", "Did not identify"),
                        main = "Difference Scores by Gender",
                        colors = c("steelblue4", "darkblue", "gray"))


    ## PLOT 5
    data_subset <- data[, .SD, .SDcols = c(median_diff_col,
                                           race_cols)]

    long_data <- melt(data_subset,
                      id.vars = median_diff_col,
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")
    # Filter rows where Has_Race is 1
    filtered_data <- long_data[Has_Race == 1]

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

    ## PLOT 6
    visualize_densities(cat_var = data[[severity_col]],
                        num_var = data[[median_diff_col]],
                        cat_var_name = "Disability Severity",
                        num_var_name = "Median Difference Scores",
                        main = "Difference Scores by Disability Severity",
                        colors = c("steelblue4", "darkblue", "gray"))


    ## PLOT 7: Primary impairment group
    boxplot(data[[median_diff_col]] ~
              data[[prim_impair_grp_col]],
            # data = data,
            main = "Difference Scores by Primary Disability Type",
            xlab = "Primary Disability",
            ylab = "Median Difference Scores",
            col = "steelblue")

    ## PLOT 8: Secondary impairment group

    boxplot(data[[median_diff_col]] ~
              data[[second_impair_grp_col]],
            # data = data,
            main = "Difference Scores by Secondary Disability Type",
            xlab = "Secondary Disability",
            ylab = "Median Difference Scores",
            col = "steelblue")

    # Reset the plotting window
    par(mfrow = c(1, 1))

  } else if (option == "investigate_wage") {


    # Check if the required columns are found

    # Find the column for Median_Time_Passed_Days
    median_time_col <- grep("(?i)(med|median).*?(time|days)(?!.*(?i)_desc)",
                            names(data),
                            value = TRUE, perl = TRUE)

    # Find the column for enrollment length
    enroll_len_col <- grep("(?i)(enroll|enrollment).*?(length|len)(?!.*(?i)_grp)",
                           names(data), value = TRUE, perl = TRUE)

    # Find the column for enrollment length group
    enroll_len_grp_col <- grep("(?i)(enroll|enrollment).*?(length|len)*?(_grp)",
                               names(data), value = TRUE, perl = TRUE)

    # Find the column for sex/gender
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # Find the columns for race
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    # Find the column for final employment status
    final_employ_col <- grep("(?i)(final).*?(employ)(?!.*(?i)_desc)",
                             names(data), value = TRUE, perl = TRUE)

    # Find the column for disability severity
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    # Find the column for primary impairment group
    prim_impair_grp_col <- grep("(?i)(prim|primary).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                                names(data), value = TRUE, perl = TRUE)

    # Find the column for secondary impairment group
    second_impair_grp_col <- grep("(?i)(sec|second).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                                  names(data), value = TRUE, perl = TRUE)

    # Find the column for exit wage
    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)

    # Check if the required columns are found
    if (length(median_time_col) == 0 || length(enroll_len_col) == 0 ||
        length(sex_col) == 0 || length(race_cols) == 0 ||
        length(final_employ_col) == 0 || length(severity_col) == 0 ||
        length(prim_impair_grp_col) == 0 ||
        length(second_impair_grp_col) == 0 || length(wage_col) == 0) {
      stop("Missing required columns for 'investigate wage' visualization.")
    }

    # Proceed with visualization using the identified columns
    message("Columns identified: ", paste(median_time_col, collapse = ", "),
            "; ",
            paste(enroll_len_col, collapse = ", "), "; ",
            paste(sex_col, collapse = ", "), "; ",
            paste(race_cols, collapse = ", "), "; ",
            paste(final_employ_col, collapse = ", "), "; ",
            paste(severity_col, collapse = ", "), "; ",
            paste(prim_impair_grp_col, collapse = ", "), "; ",
            paste(second_impair_grp_col, collapse = ", "), ";",
            paste(wage_col, collapse = ", "))


    # Set adjusted plotting window if necessary
    if (one_window == TRUE) {par(mfrow = c(3, 3))}


    ## PLOT 1

    hist(data[[wage_col]],
         col = "steelblue",
         main = "Distribution of Exit Wages",
         xlab = "Exit Wage ($ per hour)")

    ## PLOT 2
    plot(data[[median_time_col]],
         wages_vector,
         main = "Exit Wages Across Days in Program",
         ylab = "Exit Wages ($ per hour)",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)

    ## PLOT 3
    boxplot(data[[wage_col]] ~ data[[enroll_len_grp_col]],
            main = "Exit Wages Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

    # PLOT 4
    visualize_densities(cat_var = data[[sex_col]],
                        num_var = data[[wage_col]],
                        cat_var_name = "Gender",
                        num_var_name = "Exit Wages ($ per Hour)",
                        level_labels = c("Males", "Females",
                                         "Other", "Did not identify"),
                        main = "Exit Wages by Gender",
                        colors = c("lightsteelblue", "steelblue", "darkblue",
                                   "gray"))

    ## PLOT 5
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
    # wage_col_name <- wage_col[1]


    # Create a formula for the boxplot
    # boxplot_formula <- as.formula(paste(wage_col_name, "~ Race"))
    boxplot_formula <- as.formula(paste(wage_col, "~ Race"))


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
    visualize_densities(cat_var = data[[severity_col]],
                        num_var = data[[wage_col]],
                        cat_var_name = "Disability Severity",
                        num_var_name = "Exit Wages ($ per Hour)",
                        level_labels = c("Non significant", "Significant",
                                         "Most significant"),
                        main = "Exit Wages by Disability Severity",
                        colors = c("steelblue4", "darkblue", "gray"))


    ## PLOT 7
    boxplot(data[[wage_col]] ~ data[[prim_impair_grp_col]],
            main = "Exit Wages by Primary Impairment Type",
            xlab = "Primary Impairment",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

    ## PLOT 8
    boxplot(data[[wage_col]] ~ data[[second_impair_grp_col]],
            main = "Exit Wages by Secondary Impairment Type",
            xlab = "Secondary Impairment",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

    # Reset plotting window
    par(mfrow = c(1, 1))

  } else if (option == "investigate_employment") {

    # Check if the required columns are found

    # Find the column for Median_Time_Passed_Days
    median_time_col <- grep("(?i)(med|median).*?(time|days)(?!.*(?i)_desc)",
                            names(data),
                            value = TRUE, perl = TRUE)

    # Find the column for enrollment length
    enroll_len_col <- grep("(?i)(enroll|enrollment).*?(length|len)(?!.*(?i)_grp)",
                           names(data), value = TRUE, perl = TRUE)

    # Find the column for enrollment length group
    enroll_len_grp_col <- grep("(?i)(enroll|enrollment).*?(length|len)*?(_grp)",
                               names(data), value = TRUE, perl = TRUE)

    # Find the column for sex/gender
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # Find the columns for race
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    # Find the column for final employment status
    final_employ_col <- grep("(?i)(final).*?(employ)(?!.*(?i)_desc)",
                             names(data), value = TRUE, perl = TRUE)

    # Find the column for disability severity
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    # Find the column for primary impairment group
    prim_impair_grp_col <- grep("(?i)(prim|primary).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                                names(data), value = TRUE, perl = TRUE)

    # Find the column for secondary impairment group
    second_impair_grp_col <- grep("(?i)(sec|second).*?(impair|disability).*?(grp|group)(?!.*(?i)_desc)",
                                  names(data), value = TRUE, perl = TRUE)

    # Find the column for exit wage
    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)

    # Find the column for work status
    exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)


    # Check if the required columns are found
    if (length(median_time_col) == 0 || length(enroll_len_col) == 0 ||
        length(sex_col) == 0 || length(race_cols) == 0 ||
        length(final_employ_col) == 0 || length(severity_col) == 0 ||
        length(prim_impair_grp_col) == 0 ||
        length(second_impair_grp_col) == 0 || length(wage_col) == 0 ||
        length(exit_work_col) == 0) {
      stop("Missing required columns for 'investigate employment' visualization.")
    }

    # Proceed with visualization using the identified columns
    message("Columns identified: ", paste(median_time_col, collapse = ", "),
            "; ",
            paste(enroll_len_col, collapse = ", "), "; ",
            paste(sex_col, collapse = ", "), "; ",
            paste(race_cols, collapse = ", "), "; ",
            paste(final_employ_col, collapse = ", "), "; ",
            paste(severity_col, collapse = ", "), "; ",
            paste(prim_impair_grp_col, collapse = ", "), "; ",
            paste(second_impair_grp_col, collapse = ", "), ";",
            paste(wage_col, collapse = ", "))


    # Set adjusted plotting window if necessary
    if (one_window == TRUE) {par(mfrow = c(3, 3))}

    ## PLOT 1
    barplot(table(data[[final_employ_col]]),
            main = "Distribution of Exit Employment",
            names = c("Non-competitive", "Competitive"),
            xlab = "Exit Employment Status",
            col = c("lightsteelblue", "steelblue"))


    ## PLOT 2
    plot(data[[median_time_col]],
         as.character(data[[final_employ_col]]),
         main = "Exit Employment Across Time in Program",
         ylab = "Exit Employment",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 8)

    ## PLOT 3
    # Create a contingency table of Final_Employment by Gender
    employment_enroll_table <- table(data[[final_employ_col]],
                                     data[[enroll_len_grp_col]])

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
    # Create a contingency table of Final_Employment by Gender
    employment_gender_table <- table(data[[final_employ_col]],
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
    data_subset <- data[, .SD, .SDcols = c(final_employ_col,
                                           race_cols)]

    # Create a long-format data.table
    long_data <- melt(data_subset,
                      id.vars = final_employ_col,
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")
    # Filter rows where Has_Race is 1
    filtered_data <- long_data[Has_Race == 1]


    # Create a contingency table of Final_Employment by Gender
    employment_race_table <- table(filtered_data[[final_employ_col]],
                                   filtered_data$Race)


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
    # Create a contingency table of Final_Employment by Gender
    employment_severity_table <- table(data[[final_employ_col]],
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
    employment_prim_dis_table <- table(data[[final_employ_col]],
                                       data[[prim_impair_grp_col]])

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

    # Reset the plotting window
    par(mfrow = c(1, 1))

  }

}
