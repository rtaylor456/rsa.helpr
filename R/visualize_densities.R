#' Visualize Densities
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

visualize_densities <- function(cat_var, num_var,
                                cat_var_name = "Categorical Variable",
                                num_var_name = "Numeric Variable",
                                level_labels = NULL,
                                xlab = NULL, main = NULL,
                                colors = NULL) {

  # check that there are at least two factor levels in the variable
  if (length(unique(cat_var)) < 2
  ) {
    stop("There must be at least two populated levels of the categorical variable.")
  }
  # check that there are enough obs of the num var per level of cat var
  else if (all(sapply(split(num_var, cat_var), function(x) {
    sum(!is.na(x)) >= 2} )) == FALSE
             )
    {
    stop("There must be at least two observations of the numeric variable per level of the categorical variable.")
  }
  # if we meet these conditions, run the visualization code
  else {

    levels <- unique(cat_var)

    # Set default values for plotting labels if not provided
    if (is.null(xlab)) xlab <- num_var_name
    if (is.null(main)) main <- paste("Density Plot of", num_var_name, "by",
                                     cat_var_name)
    if (is.null(level_labels)) level_labels <- levels


    # Create densities for each group, skipping NA values in num_var
    densities <- lapply(levels, function(level) {
      density(num_var[cat_var == level], na.rm = TRUE)
    })

    # Determine maximum y value across all densities for setting y limits
    max_density_value <- max(sapply(densities,
                                    function(d) max(d$y, na.rm = TRUE)),
                             na.rm = TRUE)

    # Assign colors if not provided
    if (is.null(colors) || length(colors) < length(levels)) {
      # Default to rainbow colors if not enough colors provided
      colors <- rainbow(length(levels))
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
