########################
# TEXT and TABLE OUTPUTS
########################
social_variables_text <- function(data) {
  renderUI({
    # data <- selected_data()
    cult_bar_col <- grep("(?i)(cult).*?(barrier)(?!.*(?i)_desc)",
                         names(data), value = TRUE, perl = TRUE)

    # Check if a matching column was found
    if (length(cult_bar_col) > 0) {
      cultural_count <- sum(data[[cult_bar_col]] == 1, na.rm = TRUE)
    } else {
      cultural_count <- 0  # Fallback if no matching column is found
    }

    eng_learn_col <- grep("(?i)(english).*?(learn)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)
    if (length(eng_learn_col) > 0) {
      eng_learn_count <- sum(data[[eng_learn_col]] == 1,
                             na.rm = TRUE)
    } else {
      eng_learn_count <- 0  # Fallback if no matching column is found
    }

    any_cled <- cultural_count + eng_learn_count


    skills_def_col <- grep("(?i)(skills).*?(def)(?!.*(?i)_desc)",
                           names(data), value = TRUE, perl = TRUE)

    skills_def_count <- sum(data[[skills_def_col]] == 1, na.rm = TRUE)

    if (length(skills_def_col) > 0) {
      skills_def_count <- sum(data[[skills_def_col]] == 1,
                              na.rm = TRUE)
    } else {
      skills_def_count <- 0  # Fallback if no matching column is found
    }


    low_inc_col <- grep("(?i)(low).*?(inc)(?!.*(?i)_desc)",
                        names(data), value = TRUE, perl = TRUE)
    if (length(low_inc_col) > 0) {
      low_inc_count <- sum(data[[low_inc_col]] == 1,
                           na.rm = TRUE)
    } else {
      low_inc_count <- 0  # Fallback if no matching column is found
    }

    homeless_col <- grep("(?i)(homeless)(?!.*(?i)_desc)",
                         names(data), value = TRUE, perl = TRUE)
    if (length(homeless_col) > 0) {
      homeless_count <- sum(data[[homeless_col]] == 1,
                            na.rm = TRUE)
    } else {
      homeless_count <- 0  # Fallback if no matching column is found
    }

    tanf_col <- grep("(?i)(tanf.*?plan|plan.*?tanf)(?!.*(?i)_desc)",
                     names(data), value = TRUE, perl = TRUE)
    if (length(tanf_col) > 0) {
      tanf_count <- sum(data[[tanf_col]] == 1,
                        na.rm = TRUE)
    } else {
      tanf_count <- 0  # Fallback if no matching column is found
    }


    econ_marginalized <- low_inc_count + homeless_count + tanf_count


    foster_col <- grep("(?i)(foster)(?!.*(?i)_desc)",
                       names(data), value = TRUE, perl = TRUE)
    if (length(foster_col) > 0) {
      foster_count <- sum(data[[foster_col]] == 1,
                          na.rm = TRUE)
    } else {
      foster_count <- 0  # Fallback if no matching column is found
    }

    HTML(paste(
      "<div style='padding: 10px; background-color: #f9f9f9; border-left: 5px solid #007bff;'>",
      "<h3 style='color: #007bff;'>Social Factors Summary</h3>",
      "<p style='font-size: 18px;'> <strong>", any_cled,
      "</strong> participants are facing cultural barriers.</p>",
      "<p style='font-size: 18px;'> <strong>", skills_def_count,
      "</strong> participants have skill deficiencies.</p>",
      "<p style='font-size: 18px;'> <strong>", econ_marginalized,
      "</strong> participants are facing economic challenges.</p>",
      "</div>"
    ))

  })
}


# Time Summary label
gen_demo_label1 <- function() {
  renderUI({
    HTML(paste(
      "<div style='padding: 10px; background-color: #f9f9f9; border-left: 5px solid #007bff;'>",
      "<h3 style='color: #007bff;'>Time Summary</h3>",
      "</ul>",
      "</div>"
    ))
  })
}


gen_demo_label2 <- function() {
  renderUI({

    HTML(paste(
      "<div style='padding: 10px; background-color: #f9f9f9; border-left: 5px solid #007bff;'>",
      "<h3 style='color: #007bff;'>Gender Summary</h3>",
      "</ul>",
      "</div>"
    ))

  })
}


gen_demo_label_race <- function() {
  renderUI({

    HTML(paste(
      "<div style='padding: 10px; background-color: #f9f9f9; border-left: 5px solid #007bff;'>",
      "<h3 style='color: #007bff;'>Race Summary</h3>",
      "</ul>",
      "</div>"
    ))

  })
}

gen_demo_label_disability <- function() {
  renderUI({

    HTML(paste(
      "<div style='padding: 10px; background-color: #f9f9f9; border-left: 5px solid #007bff;'>",
      "<h3 style='color: #007bff;'>Disability Summary</h3>",
      "</ul>",
      "</div>"
    ))

  })
}

post_secondary_text <- function(data) {
  renderUI({

    post_sec_col <- grep("(?i)(post).*?(sec).*?(enroll)(?!.*(?i)(_desc|completion))",
                         names(data), value = TRUE, perl = TRUE)


    if (length(post_sec_col) > 0) {
      post_sec_counts <- table(factor(data[[post_sec_col]], levels = 0:3))
    } else {
      post_sec_counts <- setNames(rep(0, 4), 0:3)
    }

    HTML(paste(
      "<div style='padding: 10px; background-color: #f9f9f9; border-left: 5px solid #007bff;'>",
      "<h3 style='color: #007bff;'>Post-Secondary Enrollment Summary</h3>",
      # "<p style='font-size: 18px;'>Participants by enrollment status:</p>",
      # "<ul style='font-size: 16px;'>",
      # "<li><strong>0:</strong> Not enrolled — ", post_sec_counts["0"], " participants</li>",
      # "<li><strong>1:</strong> Enrolled in some post-secondary education — ", post_sec_counts["1"], " participants</li>",
      # "<li><strong>2:</strong> Enrolled in vocational/technical training — ", post_sec_counts["2"], " participants</li>",
      # "<li><strong>3:</strong> Enrolled in higher education (university/college) — ", post_sec_counts["3"], " participants</li>",
      "</ul>",
      "</div>"
    ))

  })
}


meta_gen_demo_table <- function(data) {
  renderTable({
    # req(selected_data())  # Ensure data is loaded
    # data <- selected_data()

    # Identify the column for gender/sex dynamically
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    if (length(sex_col) > 0) {
      gender_data <- as.character(data[[sex_col]])  # Convert to character for proper mapping
    } else {
      gender_data <- character(0)  # If no column found, empty dataset
    }

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Replace numeric values with labels
    labeled_gender_data <- factor(gender_data, levels = names(gender_labels),
                                  labels = gender_labels)

    # Calculate counts
    gender_counts <- table(labeled_gender_data)

    # Get total participants for percentage calculation
    total <- sum(gender_counts)

    # Ensure proper percentage calculation
    percentages <- if (total > 0) {
      round((as.numeric(gender_counts) / total) * 100, 1)
    } else {
      rep(0, length(gender_labels))  # Avoid division by zero
    }

    # Create a data frame for the table
    data.frame(
      `Gender` = names(gender_counts),
      `Number of Participants` = as.integer(gender_counts),
      `Percentage` = paste0(percentages, "%"),
      check.names = FALSE  # Prevents R from modifying column names
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")
}


post_secondary_table <- function(data) {
  renderTable({
    # req(selected_data())  # Ensure data is loaded

    post_sec_col <- grep("(?i)(post).*?(sec).*?(enroll)(?!.*(?i)(_desc|completion))",
                         names(data), value = TRUE, perl = TRUE)

    if (length(post_sec_col) > 0) {
      post_sec_counts <- table(factor(data[[post_sec_col]], levels = 0:3))
    } else {
      post_sec_counts <- setNames(rep(0, 4), 0:3)
    }

    total <- sum(post_sec_counts)  # Get the total number of participants

    # Ensure percentages are calculated correctly
    percentages <- if (total > 0) {
      round((as.numeric(post_sec_counts) / total) * 100, 1)
    } else {
      rep(0, 4)  # Avoid division by zero
    }

    # Create a data frame for the table
    data.frame(
      `Enrollment Status` = c("Not enrolled",
                              "Enrolled in some post-secondary education",
                              "Enrolled in vocational/technical training",
                              "Enrolled in higher education (university/college)"),
      `Number of Participants` = as.integer(post_sec_counts),  # Ensure whole numbers
      `Percentage` = paste0(percentages, "%"),  # Format as percentages
      check.names = FALSE  # Prevents R from changing column names
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")

}


## METADATA plots

######################
# GENERAL DEMOGRAPHICS
######################

# Time passed in program
# meta_gen_demo_plot1 <- function(selected_data) {
#   renderPlot({
#     req(selected_data())
#     data <- selected_data()
#
#     median_time_passed <- grep("(?i)median.*time.*passed.*days", names(data),
#                                value = TRUE,
#                                perl = TRUE)
#
#     hist(as.numeric(data[[median_time_passed]]),
#          col = "steelblue",
#          main = "Distribution of Time in Programs",
#          xlab = "Median Time in Program (per individual)")
#
#   })
# }

meta_gen_demo_plot1 <- function(data) {
  # req(selected_data())  # Ensure data is available
  # data <- selected_data()

  median_time_passed <- grep("(?i)median.*time.*passed.*days", names(data),
                             value = TRUE,
                             perl = TRUE)

  # Return a ggplot or base R plot object
  hist(as.numeric(data[[median_time_passed]]),
       col = "steelblue",
       main = "Distribution of Time in Programs",
       xlab = "Median Time in Program (per individual)")
}


# Download handler for Time in Programs Plot
download_meta_gen_demo_plot1 <- function(data) {
  downloadHandler(
    filename = function() { "time_in_programs_plot.png" },
    content = function(file) {
      png(file)
      hist(data$Median_Time_Passed_Days,
           col = "steelblue",
           main = "Distribution of Time in Programs",
           xlab = "Median Time in Program (per individual)")
      dev.off()
    }
  )
}

# Enrollment length
# meta_gen_demo_plot2 <- function(data) {
#   req(data)
#   # renderPlot({
#     # req(selected_data())
#     # data <- selected_data()
#   hist(data$Enroll_Length,
#          col = "steelblue",
#          main = "Distribution of Enrollment Lengths",
#          xlab = "Enrollment Length (Quarters)")
#
#   # })
# }

meta_gen_demo_plot2 <- function(data) {
  req(data)  # Ensure data is available

  # Check if Enroll_Length column exists
  if (!"Enroll_Length" %in% names(data)) {
    stop("The column 'Enroll_Length' is missing from the dataset.")
  }

  # Convert to numeric if necessary
  hist(as.numeric(data$Enroll_Length),
       col = "steelblue",
       main = "Distribution of Enrollment Lengths",
       xlab = "Enrollment Length (Quarters)")
}


# Download handler for Enrollment Length Plot
download_meta_gen_demo_plot2 <- function(data) {
  downloadHandler(
    filename = function() { "enrollment_length_plot.png" },
    content = function(file) {
      png(file)
      hist(data$Enroll_Length,
           col = "steelblue",
           main = "Distribution of Enrollment Lengths",
           xlab = "Enrollment Length (Quarters)")
      dev.off()
    }
  )
}


# Race
meta_gen_demo_plot4 <- function(data) {
  # renderPlot({
    # req(selected_data())
    # data <- selected_data()

    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    final_employ_col <- grep("(?i)(final).*?(employ)(?!.*(?i)_desc)",
                             names(data), value = TRUE, perl = TRUE)

    data_subset <- data[, .SD, .SDcols = c(final_employ_col, race_cols)]

    long_data <- melt(data_subset,
                      id.vars = final_employ_col,
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")

    filtered_data <- long_data[Has_Race == 1]

    # Create contingency table and order by count (descending)
    race_table <- table(filtered_data$Race)
    race_table <- sort(race_table, decreasing = TRUE)

    # Order race_cols based on the sorted race_table names
    ordered_race_cols <- names(race_table)

    par(oma = c(0, 0, 0, 0) + 0.6)
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
         labels = gsub("^E[0-9]+_|_911$", "", ordered_race_cols),
         xpd = NA,
         srt = 45,  # Rotate the labels by 45 degrees
         cex = .8,
         adj = c(1, 1))  # Adjust text alignment to center under bars
  # })
}


# Download handler for Race Distribution Plot
download_meta_gen_demo_plot4 <- function(data) {
  downloadHandler(
    filename = function() { "race_distribution_plot.png" },
    content = function(file) {
      png(file)
      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)

      data_subset <- data[, .SD, .SDcols = c("Final_Employment", race_cols)]
      long_data <- melt(data_subset, id.vars = "Final_Employment",
                        measure.vars = race_cols,
                        variable.name = "Race", value.name = "Has_Race")
      filtered_data <- long_data[Has_Race == 1]
      race_table <- table(filtered_data$Race)

      barplot_heights <- barplot(race_table, beside = TRUE,
                                 ylab = "Count",
                                 xaxt = "n",
                                 yaxt = "n",
                                 xlab = "",
                                 main = "Distribution of Race", las = 2,
                                 col = "steelblue")
      axis(side = 2, las = 2, mgp = c(3, 0.75, 0))
      text(x = barplot_heights, y = par("usr")[3] - 0.45,
           labels = gsub("^E[0-9]+_|_911$", "", race_cols),
           xpd = NA,
           srt = 45,
           cex = .8,
           adj = c(1, 1))
      dev.off()
    }
  )
}


# Severity
meta_gen_demo_plot5 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    barplot(table(data[[severity_col]]),
            main = "Distribution of Disability Severity",
            xlab = "Severity",
            names = c("Non-significant", "Significant", "Most significant"),
            col = c("lightsteelblue", "steelblue", "darkblue"))

  })
}

# Download handler for Severity Distribution Plot
download_meta_gen_demo_plot5 <- function(data) {
  downloadHandler(
    filename = function() { "severity_distribution_plot.png" },
    content = function(file) {
      png(file)
      # req(selected_data())
      # data <- selected_data()

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      barplot(table(data[[severity_col]]),
              main = "Distribution of Disability Severity",
              xlab = "Severity",
              names = c("Non-significant", "Significant", "Most significant"),
              col = c("lightsteelblue", "steelblue", "darkblue"))
      dev.off()
    }
  )
}


# Primary impairment
meta_gen_demo_plot6 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    barplot(table(data$Primary_Impairment_Group),
            main = "Distribution of Primary Impairments",
            xlab = "Primary Impairment",
            col = "steelblue")

  })
}

# Download handler for Primary Impairment Distribution Plot
download_meta_gen_demo_plot6 <- function(data) {
  downloadHandler(
    filename = function() { "primary_impairment_distribution_plot.png" },
    content = function(file) {
      png(file)
      # req(selected_data())
      # data <- selected_data()

      barplot(table(data$Primary_Impairment_Group),
              main = "Distribution of Primary Impairments",
              xlab = "Primary Impairment",
              col = "steelblue")
      dev.off()
    }
  )
}

# Primary impairment
meta_gen_demo_plot7 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    barplot(table(data$Secondary_Impairment_Group),
            main = "Distribution of Secondary Impairments",
            xlab = "Secondary Impairment",
            col = "steelblue")

  })
}

# Download handler for Secondary Impairment Distribution Plot
download_meta_gen_demo_plot7 <- function(data) {
  downloadHandler(
    filename = function() { "secondary_impairment_distribution_plot.png" },
    content = function(file) {
      png(file)
      # req(selected_data())
      # data <- selected_data()

      barplot(table(data$Secondary_Impairment_Group),
              main = "Distribution of Secondary Impairments",
              xlab = "Secondary Impairment",
              col = "steelblue")
      dev.off()
    }
  )
}

####################
# DIFFERENCE SCORES
####################

meta_diff_plot1 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    hist(data$Median_Difference_Score,
         col = "steelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")
  })
}

download_meta_diff_plot1 <- function(data) {
  downloadHandler(
    filename = function() { "difference_scores_across_time_plot.png" },
    content = function(file) {
      png(file)
      # req(selected_data())
      # data <- selected_data()

      hist(data$Median_Difference_Score,
           col = "steelblue",
           main = "Distribution of Median Difference Scores",
           xlab = "Median Difference Scores")
      dev.off()
    }
  )
}


meta_diff_plot2 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         data$Median_Difference_Score,
         main = "Difference Scores Across Time in Program",
         ylab = "Median Difference Scores",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)
  })
}

# Download handler for meta_diff_plot2
download_meta_diff_plot2 <- function(data) {
  downloadHandler(
    filename = function() { "difference_scores_across_time_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()
      plot(data$Median_Time_Passed_Days,
           data$Median_Difference_Score,
           main = "Difference Scores Across Time in Program",
           ylab = "Median Difference Scores",
           xlab = "Median Days Spent in Programs (per individual)",
           col = "steelblue",
           pch = 3)
      dev.off()
    }
  )
}


meta_diff_plot3 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    boxplot(Median_Difference_Score ~ data[["Enroll_Length_Grp"]], data = data,
            main = "Difference Scores Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Median Difference Scores",
            col = "steelblue")

  })
}

# Download handler for meta_diff_plot3
download_meta_diff_plot3 <- function(data) {
  downloadHandler(
    filename = function() { "difference_scores_across_quarters_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()
      boxplot(Median_Difference_Score ~ data[["Enroll_Length_Grp"]], data = data,
              main = "Difference Scores Across Quarters Enrolled",
              xlab = "Enrollment Length (total quarters)",
              ylab = "Median Difference Scores",
              col = "steelblue")
      dev.off()
    }
  )
}


meta_diff_plot4 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    # Identify the gender/sex column dynamically
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Replace numeric values with labels
    labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                  labels = gender_labels)

    # Ensure Median_Difference_Score has enough unique values for each gender
    if (all(sapply(levels(labeled_gender_data), function(g) {
      sum(data[[sex_col]] == g & !is.na(data$Median_Difference_Score)) >= 2
    }))) {
      # Compute density for each gender
      density_data <- lapply(levels(labeled_gender_data), function(g) {
        density(data$Median_Difference_Score[labeled_gender_data == g], na.rm = TRUE)
      })

      # Determine maximum y-limit for all densities
      max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

      # Plot the first density curve to set up the plot
      plot(density_data[[1]], col = "steelblue4", lwd = 2,
           main = "Density Plot of Median Difference Scores by Gender",
           xlab = "Median Difference Scores", ylab = "Density",
           xlim = range(sapply(density_data, function(d) range(d$x))),
           ylim = c(0, max_density_value * 1.1))

      # Fill under the first density curve
      polygon(c(density_data[[1]]$x, rev(density_data[[1]]$x)),
              c(rep(0, length(density_data[[1]]$x)), rev(density_data[[1]]$y)),
              col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)

      # Add the remaining density curves and their fills
      colors <- c("darkblue", "darkgray", "lightsteelblue")
      for (i in 2:length(density_data)) {
        lines(density_data[[i]], col = colors[i - 1], lwd = 2)
        polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
      }

      # Add a legend
      legend("topright", legend = levels(labeled_gender_data),
             col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
    } else {
      # Fallback: Create a boxplot if densities cannot be plotted
      boxplot(Median_Difference_Score ~ labeled_gender_data, data = data,
              main = "Difference Scores by Gender",
              xlab = "Gender",
              ylab = "Median Difference Scores",
              col = "steelblue")
    }
  })

}


# Download handler for meta_diff_plot4
download_meta_diff_plot4 <- function(data) {
  downloadHandler(
    filename = function() { "difference_scores_by_gender_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      # Identify the gender/sex column dynamically
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

      # Define the mapping of numeric values to labels
      gender_labels <- c(
        "1" = "Male",
        "2" = "Female",
        "3" = "Other",
        "9" = "Did not identify"
      )

      # Replace numeric values with labels
      labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                    labels = gender_labels)

      # Ensure Median_Difference_Score has enough unique values for each gender
      if (all(sapply(levels(labeled_gender_data), function(g) {
        sum(data[[sex_col]] == g & !is.na(data$Median_Difference_Score)) >= 2
      }))) {
        # Compute density for each gender
        density_data <- lapply(levels(labeled_gender_data), function(g) {
          density(data$Median_Difference_Score[labeled_gender_data == g], na.rm = TRUE)
        })

        # Determine maximum y-limit for all densities
        max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

        # Plot the first density curve to set up the plot
        plot(density_data[[1]], col = "steelblue4", lwd = 2,
             main = "Density Plot of Median Difference Scores by Gender",
             xlab = "Median Difference Scores", ylab = "Density",
             xlim = range(sapply(density_data, function(d) range(d$x))),
             ylim = c(0, max_density_value * 1.1))

        # Fill under the first density curve
        polygon(c(density_data[[1]]$x, rev(density_data[[1]]$x)),
                c(rep(0, length(density_data[[1]]$x)), rev(density_data[[1]]$y)),
                col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)

        # Add the remaining density curves and their fills
        colors <- c("darkblue", "darkgray", "lightsteelblue")
        for (i in 2:length(density_data)) {
          lines(density_data[[i]], col = colors[i - 1], lwd = 2)
          polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                  c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                  col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
        }

        # Add a legend
        legend("topright", legend = levels(labeled_gender_data), col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
      } else {
        # Fallback: Create a boxplot if densities cannot be plotted
        boxplot(Median_Difference_Score ~ labeled_gender_data, data = data,
                main = "Difference Scores by Gender",
                xlab = "Gender",
                ylab = "Median Difference Scores",
                col = "steelblue")
      }

      dev.off()
    }
  )
}


meta_diff_plot5 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

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
    par(oma = c(0, 0, 0, 0) + 0.6)
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

  })
}

# Download handler for meta_diff_plot5
download_meta_diff_plot5 <- function(data) {
  downloadHandler(
    filename = function() { "difference_scores_by_race_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)

      data_subset <- data[, .SD, .SDcols = c("Median_Difference_Score", race_cols)]

      long_data <- melt(data_subset,
                        id.vars = "Median_Difference_Score",
                        measure.vars = race_cols,
                        variable.name = "Race",
                        value.name = "Has_Race")
      filtered_data <- long_data[Has_Race == 1]

      par(oma = c(0, 0, 0, 0) + 0.6)
      boxplot(Median_Difference_Score ~ Race, data = filtered_data,
              col = "steelblue",
              xaxt = "n", yaxt = "n",
              xlab = "", ylab = "Median Difference Score",
              main = "Difference Scores Across Race")
      axis(side = 2, las = 2, mgp = c(3, 0.75, 0))
      text(x = 1:length(race_cols),
           y = par("usr")[3] - 0.45,
           labels = gsub("^E[0-9]+_|_911$", "", race_cols),
           xpd = NA, srt = 45, cex = .8, adj = 1)
      dev.off()
    }
  )
}


meta_diff_plot6 <- function(data) {
  renderPlot({
    req(selected_data())
    # data <- selected_data()

    # Identify the severity column dynamically
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    if (length(unique(data$Median_Difference_Score[
      data[[severity_col]] == 0])) >= 2 &
      length(unique(data$Median_Difference_Score[
        data[[severity_col]] == 1])) >= 2 &
      length(unique(data$Median_Difference_Score[
        data[[severity_col]] == 2])) >= 2
    ) {
      # Create density for each group
      non_significant_density <- density(data$Median_Difference_Score[
        data[[severity_col]] == 0], na.rm = TRUE)
      significant_density <- density(data$Median_Difference_Score[
        data[[severity_col]] == 1], na.rm = TRUE)
      most_significant_density <- density(data$Median_Difference_Score[
        data[[severity_col]] == 2], na.rm = TRUE)

      # Determine the maximum y value for setting y limits
      max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                 max(significant_density$y, na.rm = TRUE),
                                 max(most_significant_density$y, na.rm = TRUE)),
                               na.rm = TRUE)

      # Initialize the plot with dynamic y-limits
      plot(non_significant_density, col = "steelblue4", lwd = 2,
           main = "Density Plot of Median Difference Scores by Disability Severity",
           xlab = "Median Difference Scores", ylab = "Density",
           xlim = range(c(non_significant_density$x, significant_density$x,
                          most_significant_density$x)),
           ylim = c(0, max_density_value * 1.1))
      # Add a bit of padding for y-limits

      # Fill under the non-significant density curve
      polygon(c(non_significant_density$x, rev(non_significant_density$x)),
              c(rep(0, length(non_significant_density$x)),
                rev(non_significant_density$y)),
              col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
      # Light blue fill

      # Add the density lines for other groups
      lines(significant_density, col = "darkblue", lwd = 2)
      lines(most_significant_density, col = "darkgray", lwd = 2)

      # Fill under the significant density curve
      polygon(c(significant_density$x, rev(significant_density$x)),
              c(rep(0, length(significant_density$x)),
                rev(significant_density$y)),
              col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
      # Light dark blue fill

      # Fill under the most significant density curve
      polygon(c(most_significant_density$x, rev(most_significant_density$x)),
              c(rep(0, length(most_significant_density$x)),
                rev(most_significant_density$y)),
              col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
      # Light gray fill

      # Add a legend
      legend("topright", legend = c("Non Significant", "Significant",
                                    "Most Significant"),
             col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
    } else{
      boxplot(Median_Difference_Score ~ data[[severity_col]], data = data,
              main = "Difference Scores by Disability Severity",
              names = c("Non significant", "Significant",
                        "Most significant"),
              xlab = "Disability Severity",
              ylab = "Median Difference Scores",
              col = "steelblue")
    }


  })
}


# Download handler for meta_diff_plot6
download_meta_diff_plot6 <- function(data) {
  downloadHandler(
    filename = function() { "difference_scores_by_severity_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      if (length(unique(data$Median_Difference_Score[
        data[[severity_col]] == 0])) >= 2 &
        length(unique(data$Median_Difference_Score[
          data[[severity_col]] == 1])) >= 2 &
        length(unique(data$Median_Difference_Score[
          data[[severity_col]] == 2])) >= 2
      ) {
        non_significant_density <- density(data$Median_Difference_Score[
          data[[severity_col]] == 0], na.rm = TRUE)
        significant_density <- density(data$Median_Difference_Score[
          data[[severity_col]] == 1], na.rm = TRUE)
        most_significant_density <- density(data$Median_Difference_Score[
          data[[severity_col]] == 2], na.rm = TRUE)

        max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                   max(significant_density$y, na.rm = TRUE),
                                   max(most_significant_density$y, na.rm = TRUE)),
                                 na.rm = TRUE)

        plot(non_significant_density, col = "steelblue4", lwd = 2,
             main = "Density Plot of Median Difference Scores by Disability Severity",
             xlab = "Median Difference Scores", ylab = "Density",
             xlim = range(c(non_significant_density$x, significant_density$x,
                            most_significant_density$x)),
             ylim = c(0, max_density_value * 1.1))
        polygon(c(non_significant_density$x, rev(non_significant_density$x)),
                c(rep(0, length(non_significant_density$x)),
                  rev(non_significant_density$y)),
                col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
        lines(significant_density, col = "darkblue", lwd = 2)
        lines(most_significant_density, col = "darkgray", lwd = 2)
        polygon(c(significant_density$x, rev(significant_density$x)),
                c(rep(0, length(significant_density$x)),
                  rev(significant_density$y)),
                col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
        polygon(c(most_significant_density$x, rev(most_significant_density$x)),
                c(rep(0, length(most_significant_density$x)),
                  rev(most_significant_density$y)),
                col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
        legend("topright", legend = c("Non Significant", "Significant",
                                      "Most Significant"),
               col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
      } else {
        boxplot(Median_Difference_Score ~ data[[severity_col]], data = data,
                main = "Difference Scores by Disability Severity",
                names = c("Non significant", "Significant",
                          "Most significant"),
                xlab = "Disability Severity",
                ylab = "Median Difference Scores",
                col = "steelblue")
      }
      dev.off()
    }
  )
}



meta_diff_plot7 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
    #                      names(data), value = TRUE, perl = TRUE)

    boxplot(Median_Difference_Score ~
              Primary_Impairment_Group,
            data = data,
            main = "Difference Scores by Primary Disability Type",
            xlab = "Primary Disability",
            ylab = "Median Difference Scores",
            col = "steelblue")

  })
}

# Download handler for meta_diff_plot7
download_meta_diff_plot7 <- function(data) {
  downloadHandler(
    filename = function() { "difference_scores_by_disability_type_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      boxplot(Median_Difference_Score ~ Primary_Impairment_Group,
              data = data,
              main = "Difference Scores by Primary Disability Type",
              xlab = "Primary Disability",
              ylab = "Median Difference Scores",
              col = "steelblue")
      dev.off()
    }
  )
}

#########
# WAGE ##
#########

meta_wage_plot1 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    if (is.null(data)) {
      return("No data available.")
    }

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    hist(wages_vector,
         col = "steelblue",
         main = "Distribution of Exit Wages",
         xlab = "Exit Wage ($ per hour)")

  })
}

# Download handler for meta_wage_plot1
download_meta_wage_plot1 <- function(data) {
  downloadHandler(
    filename = function() { "exit_wages_distribution_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      if (is.null(data)) {
        return("No data available.")
      }

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      hist(wages_vector,
           col = "steelblue",
           main = "Distribution of Exit Wages",
           xlab = "Exit Wage ($ per hour)")
      dev.off()
    }
  )
}




meta_wage_plot2 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         wages_vector,
         main = "Exit Wages Across Days in Program",
         ylab = "Exit Wages ($ per hour)",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)
  })
}

# Download handler for meta_wage_plot2
download_meta_wage_plot2 <- function(data) {
  downloadHandler(
    filename = function() { "exit_wages_across_days_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      plot(data$Median_Time_Passed_Days,
           wages_vector,
           main = "Exit Wages Across Days in Program",
           ylab = "Exit Wages ($ per hour)",
           xlab = "Median Days Spent in Programs (per individual)",
           col = "steelblue",
           pch = 3)
      dev.off()
    }
  )
}


meta_wage_plot3 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))


    boxplot(wages_vector ~ data[["Enroll_Length_Grp"]],
            main = "Exit Wages Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

  })

}

# Download handler for meta_wage_plot3
download_meta_wage_plot3 <- function(data) {
  downloadHandler(
    filename = function() { "exit_wages_across_quarters_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      boxplot(wages_vector ~ data[["Enroll_Length_Grp"]],
              main = "Exit Wages Across Quarters Enrolled",
              xlab = "Enrollment Length (total quarters)",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
      dev.off()
    }
  )
}


meta_wage_plot4 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    # Identify the column with wage data (exit wage column)
    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages)) # Convert to vector if needed

    # Identify the column for gender/sex
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    if (length(sex_col) == 0) {
      stop("No gender/sex column found in the dataset.")
    }

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Replace numeric values with labels
    labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                  labels = gender_labels)

    # Check if each gender group has enough unique values for density computation
    if (all(sapply(levels(labeled_gender_data), function(g) {
      sum(labeled_gender_data == g & !is.na(wages_vector)) >= 2
    }))) {
      # Compute density for each gender group
      density_data <- lapply(levels(labeled_gender_data), function(g) {
        density(wages_vector[labeled_gender_data == g], na.rm = TRUE)
      })

      # Determine maximum y-limit for all densities
      max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

      # Plot the first density curve to set up the plot
      plot(density_data[[1]], col = "steelblue4", lwd = 2,
           main = "Density Plot of Exit Wages by Gender",
           xlab = "Exit Wages ($ per Hour)", ylab = "Density",
           xlim = range(sapply(density_data, function(d) range(d$x))),
           ylim = c(0, max_density_value * 1.1))

      # Add density curves and fill areas for each group
      colors <- c("steelblue4", "darkblue", "darkgreen", "darkgray")
      for (i in seq_along(density_data)) {
        lines(density_data[[i]], col = colors[i], lwd = 2)
        polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                col = adjustcolor(colors[i], alpha.f = 0.3), border = NA)
      }

      # Add legend
      legend("topright", legend = levels(labeled_gender_data),
             col = colors, lwd = 2)
    } else {
      # Fallback: Create a boxplot if densities cannot be computed
      boxplot(wages_vector ~ labeled_gender_data, data = data,
              main = "Exit Wages by Gender",
              xlab = "Gender",
              ylab = "Exit Wages ($ per Hour)",
              col = c("steelblue", "darkblue", "darkgreen", "darkgray"))
    }
  })

}

# Download handler for meta_wage_plot4
download_meta_wage_plot4 <- function(data) {
  downloadHandler(
    filename = function() { "exit_wages_by_gender_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      # Identify the column with wage data (exit wage column)
      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      # Identify the column for gender/sex
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

      if (length(sex_col) == 0) {
        stop("No gender/sex column found in the dataset.")
      }

      # Define the mapping of numeric values to labels
      gender_labels <- c(
        "1" = "Male",
        "2" = "Female",
        "3" = "Other",
        "9" = "Did not identify"
      )

      # Replace numeric values with labels
      labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                    labels = gender_labels)

      # Check if each gender group has enough unique values for density computation
      if (all(sapply(levels(labeled_gender_data), function(g) {
        sum(labeled_gender_data == g & !is.na(wages_vector)) >= 2
      }))) {
        # Compute density for each gender group
        density_data <- lapply(levels(labeled_gender_data), function(g) {
          density(wages_vector[labeled_gender_data == g], na.rm = TRUE)
        })

        # Determine maximum y-limit for all densities
        max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

        # Plot the first density curve to set up the plot
        plot(density_data[[1]], col = "steelblue4", lwd = 2,
             main = "Density Plot of Exit Wages by Gender",
             xlab = "Exit Wages ($ per Hour)", ylab = "Density",
             xlim = range(sapply(density_data, function(d) range(d$x))),
             ylim = c(0, max_density_value * 1.1))

        # Add density curves and fill areas for each group
        colors <- c("steelblue4", "darkblue", "darkgreen", "darkgray")
        for (i in seq_along(density_data)) {
          lines(density_data[[i]], col = colors[i], lwd = 2)
          polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                  c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                  col = adjustcolor(colors[i], alpha.f = 0.3), border = NA)
        }

        # Add legend
        legend("topright", legend = levels(labeled_gender_data),
               col = colors, lwd = 2)
      } else {
        # Fallback: Create a boxplot if densities cannot be computed
        boxplot(wages_vector ~ labeled_gender_data, data = data,
                main = "Exit Wages by Gender",
                xlab = "Gender",
                ylab = "Exit Wages ($ per Hour)",
                col = c("steelblue", "darkblue", "darkgreen", "darkgray"))
      }

      dev.off()
    }
  )
}


meta_wage_plot6 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # Identify the severity column dynamically
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    if (length(unique(wage_col[data[[severity_col]] == 0])) >= 2 &
        length(unique(wage_col[data[[severity_col]] == 1])) >= 2 &
        length(unique(wage_col[data[[severity_col]] == 2])) >= 2
    ) {
      # Create density for each group
      non_significant_density <- density(wage_col[
        data[[severity_col]] == 0], na.rm = TRUE)
      significant_density <- density(wage_col[
        data[[severity_col]] == 1], na.rm = TRUE)
      most_significant_density <- density(wage_col[
        data[[severity_col]] == 2], na.rm = TRUE)

      # Determine the maximum y value for setting y limits
      max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                 max(significant_density$y, na.rm = TRUE),
                                 max(most_significant_density$y, na.rm = TRUE)),
                               na.rm = TRUE)

      # Initialize the plot with dynamic y-limits
      plot(non_significant_density, col = "steelblue4", lwd = 2,
           main = "Density Plot of Ending Wages by Disability Severity",
           xlab = "Median Difference Scores", ylab = "Density",
           xlim = range(c(non_significant_density$x, significant_density$x,
                          most_significant_density$x)),
           ylim = c(0, max_density_value * 1.1))
      # Add a bit of padding for y-limits

      # Fill under the non-significant density curve
      polygon(c(non_significant_density$x, rev(non_significant_density$x)),
              c(rep(0, length(non_significant_density$x)),
                rev(non_significant_density$y)),
              col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
      # Light blue fill

      # Add the density lines for other groups
      lines(significant_density, col = "darkblue", lwd = 2)
      lines(most_significant_density, col = "darkgray", lwd = 2)

      # Fill under the significant density curve
      polygon(c(significant_density$x, rev(significant_density$x)),
              c(rep(0, length(significant_density$x)),
                rev(significant_density$y)),
              col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
      # Light dark blue fill

      # Fill under the most significant density curve
      polygon(c(most_significant_density$x, rev(most_significant_density$x)),
              c(rep(0, length(most_significant_density$x)),
                rev(most_significant_density$y)),
              col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
      # Light gray fill

      # Add a legend
      legend("topright", legend = c("Non Significant", "Significant",
                                    "Most Significant"),
             col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
    } else {
      boxplot(wages_vector ~ data[[severity_col]],
              main = "Exit Wages by Disability Severity",
              names = c("Non significant", "Significant",
                        "Most significant"),
              xlab = "Disability Severity",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
    }
  })
}

# Download handler
download_meta_wage_plot6 <- function(data) {
  downloadHandler(
    filename = function() { "exit_wages_by_severity_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      if (length(unique(wage_col[data[[severity_col]] == 0])) >= 2 &
          length(unique(wage_col[data[[severity_col]] == 1])) >= 2 &
          length(unique(wage_col[data[[severity_col]] == 2])) >= 2
      ) {
        non_significant_density <- density(wage_col[data[[severity_col]] == 0], na.rm = TRUE)
        significant_density <- density(wage_col[data[[severity_col]] == 1], na.rm = TRUE)
        most_significant_density <- density(wage_col[data[[severity_col]] == 2], na.rm = TRUE)

        max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                   max(significant_density$y, na.rm = TRUE),
                                   max(most_significant_density$y, na.rm = TRUE)),
                                 na.rm = TRUE)

        plot(non_significant_density, col = "steelblue4", lwd = 2,
             main = "Density Plot of Exit Wages by Disability Severity",
             xlab = "Median Difference Scores", ylab = "Density",
             xlim = range(c(non_significant_density$x, significant_density$x, most_significant_density$x)),
             ylim = c(0, max_density_value * 1.1))

        polygon(c(non_significant_density$x, rev(non_significant_density$x)),
                c(rep(0, length(non_significant_density$x)),
                  rev(non_significant_density$y)),
                col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)

        lines(significant_density, col = "darkblue", lwd = 2)
        lines(most_significant_density, col = "darkgray", lwd = 2)

        polygon(c(significant_density$x, rev(significant_density$x)),
                c(rep(0, length(significant_density$x)),
                  rev(significant_density$y)),
                col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)

        polygon(c(most_significant_density$x, rev(most_significant_density$x)),
                c(rep(0, length(most_significant_density$x)),
                  rev(most_significant_density$y)),
                col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)

        legend("topright", legend = c("Non Significant", "Significant", "Most Significant"),
               col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
      } else {
        boxplot(wages_vector ~ data[[severity_col]],
                main = "Exit Wages by Disability Severity",
                names = c("Non significant", "Significant", "Most significant"),
                xlab = "Disability Severity",
                ylab = "Exit Wages ($ per Hour)",
                col = "steelblue")
      }
      dev.off()
    }
  )
}


meta_wage_plot7 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
    #                      names(data), value = TRUE, perl = TRUE)

    boxplot(wages_vector ~ Primary_Impairment_Group,
            data = data,
            main = "Exit Wages by Primary Impairment Type",
            xlab = "Primary Impairment",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

  })
}


# Download handler
download_meta_wage_plot7 <- function(data) {
  downloadHandler(
    filename = function() { "exit_wages_by_impairment_type_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      boxplot(wages_vector ~ Primary_Impairment_Group,
              data = data,
              main = "Exit Wages by Primary Impairment Type",
              xlab = "Primary Impairment",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
      dev.off()
    }
  )
}



meta_wage_plot5 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

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
    par(oma = c(0, 0, 0, 0) + 0.6)
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

  })
}

# Download handler
download_meta_wage_plot5 <- function(data) {
  downloadHandler(
    filename = function() { "exit_wages_by_race_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)

      data_subset <- data[, .SD, .SDcols = race_cols]

      boxplot(wages_vector ~ data[[race_cols[1]]],
              main = "Exit Wages by Race/Ethnicity",
              xlab = "Race/Ethnicity",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
      dev.off()
    }
  )
}

#############
# EMPLOYMENT
#############

meta_employ_plot1 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    barplot(table(data$Final_Employment),
            main = "Distribution of Exit Employment",
            names = c("Non-competitive", "Competitive"),
            xlab = "Exit Employment Status",
            col = c("lightsteelblue", "steelblue"))

  })
}
 # Download handler
download_meta_employ_plot1 <- function(data) {
  downloadHandler(
    filename = function() { "exit_employment_distribution_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      barplot(table(data$Final_Employment),
              main = "Distribution of Exit Employment",
              names = c("Non-competitive", "Competitive"),
              xlab = "Exit Employment Status",
              col = c("lightsteelblue", "steelblue"))
      dev.off()
    }
  )
}




meta_employ_plot2 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         as.character(data$Final_Employment),
         main = "Exit Employment Across Time in Program",
         ylab = "Exit Employment",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 8)

  })
}

download_meta_employ_plot2 <- function(data) {
  downloadHandler(
    filename = function() { "exit_employment_across_time_in_program_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                            names(data), value = TRUE, perl = TRUE)

      plot(data$Median_Time_Passed_Days,
           as.character(data$Final_Employment),
           main = "Exit Employment Across Time in Program",
           ylab = "Exit Employment",
           xlab = "Median Days Spent in Programs (per individual)",
           col = "steelblue",
           pch = 8)
      dev.off()
    }
  )
}



meta_employ_plot3 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names

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

  })
}

download_meta_employ_plot3 <- function(data) {
  downloadHandler(
    filename = function() { "exit_employment_across_enrollment_length_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      employment_enroll_table <- table(data$Final_Employment,
                                       data[["Enroll_Length_Grp"]])

      rownames(employment_enroll_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")

      colnames(employment_enroll_table) <- c("<5", "5-10", "11+")

      barplot(employment_enroll_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topright", bty = "n",
                                 title = "Employment Type"),
              xlab = "Enrollment Length (total quarters)", ylab = "Count",
              main = "Exit Employment Across Quarters Enrolled")
      dev.off()
    }
  )
}


meta_employ_plot4 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    # Identify the gender/sex column dynamically
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    if (length(sex_col) == 0) {
      stop("No gender/sex column found in the dataset.")
    }

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Replace numeric values with labels
    labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                  labels = gender_labels)

    # Create a contingency table of Final_Employment by Gender
    employment_gender_table <- table(data$Final_Employment, labeled_gender_data)

    # Assign row names for employment types
    rownames(employment_gender_table) <- c("Non-competitive Employment",
                                           "Competitive Employment")

    # Create a bar plot with bars broken up by gender
    barplot(employment_gender_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topright", bty = "n",
                               title = "Employment Type"),
            xlab = "Gender", ylab = "Count",
            main = "Exit Employment by Gender")
  })

}


download_meta_employ_plot4 <- function(data) {
  downloadHandler(
    filename = function() { "exit_employment_by_gender_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      # Identify the gender/sex column dynamically
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

      if (length(sex_col) == 0) {
        stop("No gender/sex column found in the dataset.")
      }

      # Define the mapping of numeric values to labels
      gender_labels <- c(
        "1" = "Male",
        "2" = "Female",
        "3" = "Other",
        "9" = "Did not identify"
      )

      # Replace numeric values with labels
      labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                    labels = gender_labels)

      # Create a contingency table of Final_Employment by Gender
      employment_gender_table <- table(data$Final_Employment, labeled_gender_data)

      # Assign row names for employment types
      rownames(employment_gender_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")

      # Create a bar plot with bars broken up by gender
      barplot(employment_gender_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topright", bty = "n",
                                 title = "Employment Type"),
              xlab = "Gender", ylab = "Count",
              main = "Exit Employment by Gender")

      dev.off()
    }
  )
}



meta_employ_plot5 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

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
    par(oma = c(0, 0, 0, 0) + 0.6)

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

  })
}

download_meta_employ_plot5 <- function(data) {
  downloadHandler(
    filename = function() { "exit_employment_by_race_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)

      data_subset <- data[, .SD, .SDcols = c("Final_Employment",
                                             race_cols)]

      long_data <- melt(data_subset,
                        id.vars = "Final_Employment",
                        measure.vars = race_cols,
                        variable.name = "Race",
                        value.name = "Has_Race")
      filtered_data <- long_data[Has_Race == 1]

      employment_race_table <- table(filtered_data$Final_Employment,
                                     filtered_data$Race)

      bar_midpoints <- barplot(employment_race_table, beside = TRUE,
                               col = c("lightsteelblue", "steelblue"),
                               legend.text = c("Non-competitive", "Competitive"),
                               args.legend = list(x = "topleft", bty = "n",
                                                  title = "Employment Type"),
                               ylab = "Count",
                               xaxt = "n", yaxt = "n", xlab = "",
                               main = "Final Employment by Race", las = 2)

      axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

      text(x = colMeans(bar_midpoints),
           y = par("usr")[3] - 0.45,
           labels = gsub("^E[0-9]+_|_911$", "", race_cols),
           xpd = NA,
           srt = 45,
           cex = .8,
           adj = 1)
      dev.off()
    }
  )
}



meta_employ_plot6 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

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

  })
}


download_meta_employ_plot6 <- function(data) {
  downloadHandler(
    filename = function() { "exit_employment_by_severity_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      employment_severity_table <- table(data$Final_Employment,
                                         data[[severity_col]])

      rownames(employment_severity_table) <- c("Non-competitive Employment",
                                               "Competitive Employment")

      colnames(employment_severity_table) <- c("Non-significant",
                                               "Significant",
                                               "Most significant")

      barplot(employment_severity_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topleft", bty = "n",
                                 title = "Employment Type"),
              xlab = "Disability Severity", ylab = "Count",
              main = "Exit Employment by Disability Severity")
      dev.off()
    }
  )
}


meta_employ_plot7 <- function(data) {
  renderPlot({
    # req(selected_data())
    # data <- selected_data()

    # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
    #                      names(data), value = TRUE, perl = TRUE)

    # Create a contingency table of Final_Employment by Gender
    # employment_prim_dis_table <- table(data$Final_Employment,
    #                                    data[[prim_dis_col]])

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

  })
}


download_meta_employ_plot7 <- function(data) {
  downloadHandler(
    filename = function() { "exit_employment_by_primary_impairment_plot.png" },
    content = function(file) {
      png(file)
      # data <- selected_data()

      employment_prim_dis_table <- table(data$Final_Employment,
                                         data$Primary_Impairment_Group)

      rownames(employment_prim_dis_table) <- c("Non-competitive Employment",
                                               "Competitive Employment")

      barplot(employment_prim_dis_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topleft", bty = "n",
                                 title = "Employment Type"),
              xlab = "Primary Impairment", ylab = "Count",
              main = "Exit Employment by Primary Impairment")
      dev.off()
    }
  )
}
