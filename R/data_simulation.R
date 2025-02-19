# Keeps variable relationships but removes direct identifiers.

library(data.table)
setDT(data)
scrambled_data <- data[sample(.N)]


# Scramble individual columns independently.

scrambled_data <- copy(data)
scrambled_data[, (names(data)) := lapply(.SD, sample), .SDcols = names(data)]


# Maintain some structure but mix within subgroups.
scrambled_data <- data[, lapply(.SD, sample), by = some_group]


# Add small noise to numeric variables while keeping their distributions.

scrambled_data[, numeric_var := numeric_var + rnorm(.N, mean = 0, sd = 1)]


# Swap values between random pairs.

swap_values <- function(vec) {
  n <- length(vec)
  idx <- sample(n, n, replace = FALSE)
  vec[idx]
}
scrambled_data <- data[, lapply(.SD, swap_values), .SDcols = names(data)]
