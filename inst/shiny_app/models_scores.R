################
# SCORES MODEL #
################

# # Reactive value to track if the ANOVA test has been run
# anova_run <- reactiveVal(FALSE)
#
# model_scores <- reactive({
#   req(selected_data())
#   req(input$anova)
#   data <- selected_data()
#   anova_test <- input$anova
#
#   if (anova_test == "ANOVA across Services") {
#     difference_cols <- grep("(?i)difference", names(data),
#                             value = TRUE, perl = TRUE)
#
#     participant_col <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
#                             names(data), value = TRUE, perl = TRUE)
#
#     # Exclude columns that contain "med" or "avail"
#     exclude_patterns <- "(?i)med|avail"
#     filtered_columns <- difference_cols[!grepl(exclude_patterns,
#                                                difference_cols, perl = TRUE)]
#
#     differences <- data[, .SD, .SDcols = c(participant_col, filtered_columns)]
#
#     # Convert data to long format to input into ANOVA test function
#     long_data <- melt(differences, id.vars = participant_col,
#                       measure.vars = filtered_columns,
#                       variable.name = "Services",
#                       value.name = "Difference_Scores")
#
#     # Convert Services column to a factor
#     long_data[, Services := factor(Services)]
#
#
#     # Perform ANOVA
#     # result <- aov(mean_value ~ service, data = means_df)
#     result <- aov(Difference_Scores ~ Services, data = na.omit(long_data))
#     # pairwise comparisons
#     tukey_result <- TukeyHSD(result)
#
#
#   } else if (anova_test == "ANOVA across Providers") {
#
#     result <- aov(Median_Difference_Score ~ Provider,
#                   data = na.omit(data[, c("Median_Difference_Score",
#                                           "Provider")]))
#
#     # pairwise comparisons
#     tukey_result <- TukeyHSD(result)
#
#
#   } else {
#     return(NULL)  # Return NULL if no valid test is selected
#   }
#
#   # Set anova_run to TRUE
#   anova_run(TRUE)
#
#   list(anova = result, tukey = tukey_result)
#
# })
#


compute_model_scores <- function(selected_data, anova_test) {
  if (is.null(selected_data) || is.null(anova_test)) {
    return(NULL)
  }

  data <- selected_data()

  if (anova_test == "ANOVA across Services") {
    difference_cols <- grep("(?i)difference", names(data), value = TRUE, perl = TRUE)
    participant_col <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
                            names(data), value = TRUE, perl = TRUE)

    exclude_patterns <- "(?i)med|avail"
    filtered_columns <- difference_cols[!grepl(exclude_patterns, difference_cols, perl = TRUE)]

    differences <- data[, .SD, .SDcols = c(participant_col, filtered_columns)]

    long_data <- melt(differences, id.vars = participant_col,
                      measure.vars = filtered_columns,
                      variable.name = "Services",
                      value.name = "Difference_Scores")

    long_data[, Services := factor(Services)]
    result <- aov(Difference_Scores ~ Services, data = na.omit(long_data))
    tukey_result <- TukeyHSD(result)

  } else if (anova_test == "ANOVA across Providers") {
    result <- aov(Median_Difference_Score ~ Provider,
                  data = na.omit(data[, c("Median_Difference_Score", "Provider")]))
    tukey_result <- TukeyHSD(result)

  } else {
    return(NULL)
  }

  list(anova = result, tukey = tukey_result)
}


