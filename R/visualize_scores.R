#' Visualize Scores Data
#'
#' This function
#'
#' @param cat_var
#' @param num_var
#' @param cat_var_name
#' @param num_var_name
#' @param level_labels
#' @param xlab
#' @param main
#' @param colors
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

    hist(data$Median_Difference_Score,
         col = "lightsteelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")

    hist(data$Median_Time_Passed_Days,
         col = "lightsteelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

    hist(data$Differences_Available,
         col = "lightsteelblue",
         main = "Distribution of Counts of Difference Scores",
         xlab = "Number of Program Difference Scores per Individual")

    if (one_window == TRUE) { par(mfrow = c(1, 1)) }
  }

}



