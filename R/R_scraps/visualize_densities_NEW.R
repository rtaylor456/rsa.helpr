visualize_densities <- function(cat_var, num_var,
                                cat_var_name = "Categorical Variable",
                                num_var_name = "Numeric Variable",
                                level_labels = NULL,
                                xlab = NULL, main = NULL,
                                colors = NULL) {

  # Set plotting area to default single panel
  par(mfrow = c(1, 1))

  # Check that there are at least two factor levels in the variable
  if (length(unique(cat_var)) < 1) {
    stop("There must be at least one populated level of the categorical variable.")
  }

  # Ensure level labels exclude NA
  if (is.null(level_labels)) level_labels <- unique(na.omit(cat_var))

  # Set default values for plotting labels if not provided
  if (is.null(xlab)) xlab <- num_var_name
  if (is.null(main)) main <- paste("Density Plot of", num_var_name, "by", cat_var_name)

  # Assign colors if not provided
  if (is.null(colors) || length(colors) < length(level_labels)) {
    colors <- rainbow(length(level_labels))
  }

  # Filter out NA values in num_var per level and ensure each has sufficient data for density
  densities <- lapply(level_labels, function(level) {
    subset_data <- num_var[cat_var == level]
    subset_data <- subset_data[!is.na(subset_data)]  # Explicitly remove NAs from subset_data

    if (length(subset_data) >= 2) {
      density(subset_data)
    } else {
      NULL  # Mark insufficient data with NULL
    }
  })

  # Remove any NULL densities from the list to avoid plotting errors
  valid_densities <- densities[!sapply(densities, is.null)]
  valid_labels <- level_labels[!sapply(densities, is.null)]
  valid_colors <- colors[!sapply(densities, is.null)]

  if (length(valid_densities) == 0) {
    stop("Not enough data to plot density for any group.")
  }

  # Determine maximum y value across all valid densities for setting y limits
  max_density_value <- max(sapply(valid_densities, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

  # Initialize the plot with dynamic x and y limits
  plot(valid_densities[[1]], col = valid_colors[1], lwd = 2,
       main = main,
       xlab = xlab,
       ylab = "Density",
       xlim = range(unlist(lapply(valid_densities, `[[`, "x")), na.rm = TRUE),
       ylim = c(0, max_density_value * 1.1))

  # Add densities and filled areas
  for (i in seq_along(valid_densities)) {
    density_data <- valid_densities[[i]]

    # Fill area under the density curve
    polygon(c(density_data$x, rev(density_data$x)),
            c(rep(0, length(density_data$x)), rev(density_data$y)),
            col = adjustcolor(valid_colors[i], alpha.f = 0.3), border = NA)

    # Add density line on top
    lines(density_data, col = valid_colors[i], lwd = 2)
  }

  # Add a legend
  legend("topright", title = cat_var_name, legend = valid_labels,
         col = valid_colors, lwd = 2, cex = 0.8)
}
