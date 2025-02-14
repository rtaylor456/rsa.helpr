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
