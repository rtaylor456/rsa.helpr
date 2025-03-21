# working as of 3_20_25
visualize_densities <- function(cat_var, num_var,
                                cat_var_name = "Categorical Variable",
                                num_var_name = "Numeric Variable",
                                level_labels = NULL,
                                xlab = NULL, main = NULL,
                                colors = NULL) {

  # check that there are at least two factor levels in the variable
  if (length(levels(cat_var)) < 1
  ) {
    stop("There must be at least one populated level of the categorical variable.")
  }

  # find out the counts per level--mark FALSE for levels with >= 2 observations
  #   and TRUE for sparse levels, where there are < 2 observations.
  data_counts <- sapply(split(num_var, cat_var),
                        function(x) length(unique(x[!is.na(x)])) < 2)

  # find out how many levels have enough data for density plots (ie. the sum of
  #   levels where data_counts != TRUE)
  valid_levels <- sum(!data_counts)

  # if fewer than 2 levels have sufficient data, produce density plot with
  #   warning
  if (valid_levels < 2) {
    warning("Not enough observations per level for density plots. Displaying boxplots instead.")

    # Plot side-by-side boxplots
    # if (is.null(level_labels)) level_labels <- unique(na.omit(cat_var))
    if (is.null(level_labels)) level_labels <- levels(cat_var)
    if (is.null(xlab)) xlab <- cat_var_name
    if (is.null(main)) main <- paste("Boxplot of", num_var_name, "by",
                                     cat_var_name)

    boxplot(num_var ~ cat_var, col = colors, main = main, xlab = xlab,
            ylab = num_var_name,
            names = level_labels)
    return()
  }
  # if we meet these conditions, run the visualization code
  else {

    invalid_level_labels <- names(data_counts)[data_counts]

    if (valid_levels != length(levels(cat_var))) {
      warning(paste0("The following level(s) have inadequate data (< 2 obs.) for density plots: ",
                     "'", invalid_level_labels, "'",". ",
                     "These levels will not be plotted."))
    }

    # find out what labels are associated with data_counts = FALSE--meaning,
    #.   find out what labels are associated with enough data in a level
    valid_level_labels <- names(data_counts)[!data_counts]

    # levels <- unique(cat_var)
    # levels <- levels(cat_var)
    # levels <- valid_level_labels

    # Set default values for plotting labels if not provided
    if (is.null(xlab)) xlab <- num_var_name
    if (is.null(main)) main <- paste("Density Plot of", num_var_name, "by",
                                     cat_var_name)
    # if the user did not provide labels for the categorical variable levels, or
    #.  if they did, but the labels do not match the number of valid levels
    if (is.null(level_labels) ||
        (length(level_labels) != length(valid_level_labels))) {
      warning("Level labels will be automated based on the valid/populated levels of the categorical variable.")
      level_labels <- valid_level_labels
    }


    densities <- lapply(valid_level_labels, function(level) {
      density(num_var[cat_var == level], na.rm = TRUE)
    })

    # Determine maximum y value across all densities for setting y limits
    max_density_value <- max(sapply(densities,
                                    function(d) max(d$y, na.rm = TRUE)),
                             na.rm = TRUE)

    # Assign colors if not provided
    if (is.null(colors) || length(colors) < length(valid_level_labels)) {
      # Default to rainbow colors if not enough colors provided
      colors <- rainbow(length(valid_level_labels))
    }

    # Initialize the plot with dynamic x and y limits
    plot(densities[[1]], col = colors[1], lwd = 2,
         main = main,
         xlab = xlab,
         ylab = "Density",
         xlim = range(unlist(lapply(densities, `[[`, "x")), na.rm = TRUE),
         ylim = c(0, max_density_value * 1.1))


    # Loop through each density to add to the plot
    for (i in seq_along(densities)) {
      density_data <- densities[[i]]

      # Add filled area under the density curve
      polygon(c(density_data$x, rev(density_data$x)),
              c(rep(0, length(density_data$x)), rev(density_data$y)),
              col = adjustcolor(colors[i], alpha.f = 0.3), border = NA)

      # Add density line on top
      lines(density_data, col = colors[i], lwd = 2)
    }

    # Add a legend
    legend("topright", title = cat_var_name, legend = level_labels,
           col = colors, lwd = 2, cex = 0.8)

  }

}





# check that there are enough obs of the num var per level of cat var
# else if (all(sapply(split(num_var, cat_var), function(x) {
#   sum(!is.na(x)) >= 2} )) == FALSE
#            )
#   {
#   stop("There must be at least two observations of the numeric variable per level of the categorical variable.")
# }
#


# insufficient_data <- any(sapply(split(num_var, cat_var),
#                                 function(x) sum(!is.na(x)) < 2))

# insufficient_data <- any(sapply(split(num_var, cat_var),
#                                 function(x) length(unique(x[!is.na(x)])) < 2))


# Create densities for each group, skipping NA values in num_var
# densities <- lapply(levels, function(level) {
#   density(num_var[cat_var == level], na.rm = TRUE)
# })


# if (is.null(colors) || length(colors) < length(levels)) {
#   # Default to rainbow colors if not enough colors provided
#   colors <- rainbow(length(levels))
# }
