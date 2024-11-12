#' Visualize Scores Data
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

visualize_scores <- function(data, option = c("overview", "across_service",
                                  "across_provider"), one_window = TRUE) {
  option <- match.arg(option)

  if (option == "overview") {

    if (one_window == TRUE) { par(mfrow = c(3, 1)) }

    ## PLOT 1
    hist(data$Median_Difference_Score,
         col = "lightsteelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")

    ## PLOT 2
    hist(data$Median_Time_Passed_Days,
         col = "lightsteelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

    ## PLOT 3
    hist(data$Differences_Available,
         col = "lightsteelblue",
         main = "Distribution of Counts of Difference Scores",
         xlab = "Number of Program Difference Scores per Individual")

    if (one_window == TRUE) { par(mfrow = c(1, 1)) }
  }
  else if (option == "across_service") {

    if (one_window == TRUE) { par(mfrow = c(3, 1)) }

    ## PLOT 1
    difference_cols <- grep("(?i)difference", names(data),
                            value = TRUE, perl = TRUE)
    # differences <- data[, .SD, .SDcols = difference_cols]

    # Exclude columns that contain "med" or "avail"
    exclude_patterns <- "(?i)med|avail"
    filtered_columns <- difference_cols[!grepl(exclude_patterns,
                                               difference_cols,
                                               perl = TRUE)]

    differences <- data[, .SD, .SDcols = filtered_columns]

    # Find the overall median for all differences scores
    differences_scores_vector <- as.vector(unlist(differences))
    differences_median <- median(differences_scores_vector, na.rm = TRUE)

    par(las = 2)
    boxplot(differences,
            names = sub("Difference_", "", names(differences)),
            main = "Distributions of Difference Scores Across Services",
            ylab = "Difference Scores",
            xlab = "Service Test Category",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = differences_median, lty = 1, lwd = 3, col = "steelblue")
    par(las = 1)

    ## PLOT 2
    pre_cols <- grep("(?i)pre", names(data),
                     value = TRUE, perl = TRUE)
    pre_scores <- data[, .SD, .SDcols = pre_cols]

    # Find the overall median for all differences scores
    pre_scores_vector <- as.vector(unlist(pre_scores))
    pre_scores_median <- median(pre_scores_vector, na.rm = TRUE)

    par(las = 2)
    boxplot(pre_scores,
            names = sub("Pre_Score_", "", names(pre_scores)),
            main = "Distributions of Pre Scores Across Services",
            ylab = "Pre Scores",
            xlab = "Service Test Category",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = pre_scores_median, lty = 2, lwd = 3, col = "steelblue")
    par(las = 1)

    ## PLOT 3
    post_cols <- grep("(?i)post", names(data),
                      value = TRUE, perl = TRUE)
    post_scores <- data[, .SD, .SDcols = post_cols]

    # Find the overall median for all differences scores
    post_scores_vector <- as.vector(unlist(post_scores))
    post_scores_median <- median(post_scores_vector, na.rm = TRUE)

    ########## need this for second line that shows pre_score_median too
    pre_cols <- grep("(?i)pre", names(data),
                     value = TRUE, perl = TRUE)
    pre_scores <- data[, .SD, .SDcols = pre_cols]

    # Find the overall median for all differences scores
    pre_scores_vector <- as.vector(unlist(pre_scores))
    pre_scores_median <- median(pre_scores_vector, na.rm = TRUE)
    ##########

    par(las = 2)
    boxplot(post_scores,
            names = sub("Post_Score_", "", names(post_scores)),
            main = "Distributions of Post Scores Across Services",
            ylab = "Post Scores",
            xlab = "Service Test Category",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = pre_scores_median, lty = 2, lwd = 3, col = "steelblue")
    abline(h = post_scores_median, lty = 3, lwd = 3, col = "steelblue")
    par(las = 1)

    if (one_window == TRUE) { par(mfrow = c(1, 1)) }
  }
  else if (option == "across_provider") {

    ## PLOT
    median_diff_col <- "Median_Difference_Score"

    median_diff_scores <- data[, .SD, .SDcols = median_diff_col]

    # Find the overall median for all median differences scores
    median_diff_scores_vector <- as.vector(unlist(median_diff_scores))
    overall_median <- median(median_diff_scores_vector, na.rm = TRUE)

    par(las = 2)
    boxplot(Median_Difference_Score ~ Provider,
            data = data,
            main = "Median Difference Scores Across Providers",
            ylab = "Median Difference Score",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = overall_median, lty = 1, lwd = 3, col = "steelblue")
    par(las = 1)

  }

}



