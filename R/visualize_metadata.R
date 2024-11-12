#' Visualize Metadata
#'
#' This function
#'
#' @param data
#' @param option
#' @param one_window
#' @param
#' @param
#' @param
#' @param
#' @param
#'
#' @returns
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
            names = c("Females", "Males", "Did not identify"),
            col = c("lightsteelblue", "steelblue", "darkblue"))

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
                        level_labels = c("Did not identify", "Females",
                                         "Males"),
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
    # visualize_densities(cat_var = data[[severity_col]],
    #                     num_var = data$Median_Difference_Score,
    #                     cat_var_name = "Disability Severity",
    #                     num_var_name = "Median Difference Scores",
    #                     main = "Difference Scores by Disability Severity",
    #                     colors = c("steelblue4", "darkblue", "gray"))
    #

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

  } else if (option == "investigate_employment") {

  }

}
