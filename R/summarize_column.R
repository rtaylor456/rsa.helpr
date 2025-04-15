summarize_column <- function(x) {
  # Remove NAs
  x <- x[!is.na(x)]

  # Return a one-row data frame with summary statistics
  data.frame(
    mean   = mean(x),
    sd     = sd(x),
    median = median(x),
    IQR    = IQR(x),
    count  = length(x),
    row.names = NULL
  )
}

