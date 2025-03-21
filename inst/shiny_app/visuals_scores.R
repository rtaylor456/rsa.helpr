## SCORES plots
##################
## Overview
##################
overview_plot1 <- function() {
  renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Median_Difference_Score,
         col = "lightsteelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")

  })
}

# Download handler for overview_plot1
download_overview_plot1 <- function() {
  downloadHandler(
    filename = function() { "median_difference_scores.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Median_Difference_Score,
           col = "lightsteelblue",
           main = "Distribution of Median Difference Scores",
           xlab = "Median Difference Scores")
      dev.off()
    }
  )
}


overview_plot2 <- function() {
  renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Median_Time_Passed_Days,
         col = "lightsteelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

  })
}

# Download handler for overview_plot2
download_overview_plot2 <- function() {
  downloadHandler(
    filename = function() { "median_time_passed.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Median_Time_Passed_Days,
           col = "lightsteelblue",
           main = "Distribution of Time in Programs",
           xlab = "Median Time in Program (per individual)")
      dev.off()
    }
  )
}


overview_plot3 <- function() {
  renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Differences_Available,
         col = "lightsteelblue",
         main = "Distribution of Counts of Difference Scores",
         xlab = "Number of Program Difference Scores per Individual")

  })
}

# Download handler for overview_plot3
download_overview_plot3 <- function() {
  downloadHandler(
    filename = function() { "differences_available.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Differences_Available,
           col = "lightsteelblue",
           main = "Distribution of Counts of Difference Scores",
           xlab = "Number of Program Difference Scores per Individual")
      dev.off()
    }
  )
}

##################
## Across Services
##################
services_plot1 <- function() {
  renderPlot({
    req(selected_data())
    data <- selected_data()

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

  })
}

# Download handler for services_plot1
download_services_plot1 <- function() {
  downloadHandler(
    filename = function() {
      paste("services_plot1_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      difference_cols <- grep("(?i)difference", names(selected_data()),
                              value = TRUE, perl = TRUE)
      exclude_patterns <- "(?i)med|avail"
      filtered_columns <- difference_cols[!grepl(exclude_patterns,
                                                 difference_cols, perl = TRUE)]
      differences <- selected_data()[, .SD, .SDcols = filtered_columns]
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
      dev.off()
    }
  )
}


services_plot2 <- function() {
  renderPlot({
    req(selected_data())
    data <- selected_data()

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
  })
}

# For services_plot2
download_services_plot2 <- function() {
  downloadHandler(
    filename = function() {
      paste("services_plot2_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      pre_cols <- grep("(?i)pre", names(selected_data()), value = TRUE,
                       perl = TRUE)
      pre_scores <- selected_data()[, .SD, .SDcols = pre_cols]
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
      dev.off()
    }
  )
}


services_plot3 <- function() {
  renderPlot({
    req(selected_data())
    data <- selected_data()

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
  })
}

# For services_plot3
download_services_plot3 <- function() {
  downloadHandler(
    filename = function() {
      paste("services_plot3_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      post_cols <- grep("(?i)post", names(selected_data()), value = TRUE,
                        perl = TRUE)
      post_scores <- selected_data()[, .SD, .SDcols = post_cols]
      post_scores_vector <- as.vector(unlist(post_scores))
      post_scores_median <- median(post_scores_vector, na.rm = TRUE)

      pre_cols <- grep("(?i)pre", names(selected_data()), value = TRUE,
                       perl = TRUE)
      pre_scores <- selected_data()[, .SD, .SDcols = pre_cols]
      pre_scores_vector <- as.vector(unlist(pre_scores))
      pre_scores_median <- median(pre_scores_vector, na.rm = TRUE)

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
      dev.off()
    }
  )
}

####################
## Across Providers
####################
providers_plot1 <- function() {
  renderPlot({
    req(selected_data())
    data <- selected_data()

    # provider_col <- grep("(?i)provider", names(data),
    #                   value = TRUE, perl = TRUE)
    #
    # providers <- data[, .SD, .SDcols = provider_col]

    # want to create a table that prints the distribution of providers too

    # we know this exact variable name because it's created during our
    #   cleaning
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
  })

}

# For providers_plot1
download_providers_plot1 <- function() {
  downloadHandler(
    filename = function() {
      paste("providers_plot1_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      data <- selected_data()
      median_diff_col <- "Median_Difference_Score"
      median_diff_scores <- data[, .SD, .SDcols = median_diff_col]
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
      dev.off()
    }
  )
}


################################################################################
## PROVIDER DATA plots
################################################################################

# VERY ROUGH, NOT finalized plots. Placeholders.
provider_data_plot1 <- function() {
  renderPlot({
    req(rv$provider_data_cleaned)
    data <- rv$provider_data_cleaned

    barplot(table(data$Caseload))

  })
}

# Download handler for overview_plot1
download_provider_data_plot1 <- function() {
  downloadHandler(
    filename = function() { "provider_caseload.png" },
    content = function(file) {
      png(file)
      req(rv$provider_data_cleaned)
      barplot(table(data$Caseload))
      dev.off()
    }
  )
}


provider_data_plot2 <- function() {
  renderPlot({
    req(rv$provider_data_cleaned)
    data <- rv$provider_data_cleaned
    barplot(table(data$Group_freq))

  })
}

download_provider_data_plot2 <- function() {
  downloadHandler(
    filename = function() { "provider_group_freq.png" },
    content = function(file) {
      png(file)
      req(rv$provider_data_cleaned)
      data <- rv$provider_data_cleaned
      barplot(table(data$Group_freq))
      dev.off()
    }
  )
}



provider_data_plot3 <- function() {
  renderPlot({
    req(rv$provider_data_cleaned)
    data <- rv$provider_data_cleaned
    barplot(table(data$Online_freq))

  })
}

download_provider_data_plot3 <- function() {
  downloadHandler(
    filename = function() { "provider_online_freq.png" },
    content = function(file) {
      png(file)
      req(rv$provider_data_cleaned)
      data <- rv$provider_data_cleaned
      barplot(table(data$Online_freq))
      dev.off()
    }
  )
}



provider_data_plot4 <- function() {
  renderPlot({
    req(rv$provider_data_cleaned)
    data <- rv$provider_data_cleaned

    barplot(table(data$Rural_freq))

  })
}

download_provider_data_plot4 <- function() {
  downloadHandler(
    filename = function() { "provider_rural_freq.png" },
    content = function(file) {
      png(file)
      req(rv$provider_data_cleaned)
      data <- rv$provider_data_cleaned
      barplot(table(data$rural_freq))
      dev.off()
    }
  )
}


provider_data_plot5 <- function() {
  renderPlot({
    req(rv$provider_data_cleaned)
    data <- rv$provider_data_cleaned

    barplot(table(data$Mode))

  })
}

download_provider_data_plot5 <- function() {
  downloadHandler(
    filename = function() { "provider_mode.png" },
    content = function(file) {
      png(file)
      req(rv$provider_data_cleaned)
      data <- rv$provider_data_cleaned
      barplot(table(data$Mode))
      dev.off()
    }
  )
}
