handle_numerics <- function(data, column_name) {
  data[, (column_name) := fifelse(
    # If value is mixed numeric and non-numeric
    grepl("\\d", get(column_name)) & grepl("\\D", get(column_name)),
    # Then keep only numeric parts
    gsub("\\D", "", get(column_name)),
    # If value is fully non-numeric (including NAs)
    fifelse(
      grepl("\\D", get(column_name)),
      # Then convert to NA
      NA_character_,
      # Otherwise, ensure the value stays as a character
      as.character(get(column_name))
    )
  )]
  return(data)
}


handle_numerics2 <- function(column) {
  # Apply the logic to the individual column
  column <- fifelse(
    # If value is mixed numeric and non-numeric
    grepl("\\d", column) & grepl("\\D", column),
    # Then keep only numeric parts
    gsub("\\D", "", column),
    # If value is fully non-numeric (including NAs)
    fifelse(
      grepl("\\D", column),
      # Then convert to NA
      NA_character_,
      # Otherwise, ensure the value stays as a character
      as.character(column)
    )
  )
  column <- as.numeric(column)
  return(column)
}

