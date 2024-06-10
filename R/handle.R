#' Convert date variable.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A date variable, written as a numeric YYYYMMDD.
#' @returns The converted date variable.
#' @export
handle_date <- function(x) {
  date <- as.Date(as.character(x), format = "%Y%m%d")
  return(date)
}

handle_excel_date <- function(x){
  date <- as.Date(as.numeric(x), origin = "1899-12-30")
  return(date)
}

handle_year <- function(x) {
  year <- as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x))
  return(year)
}


handle_nines <- function(x, unidentified_to_0 = FALSE) {
  x <- as.numeric(x)
  # Replace NA values with 9
  x[is.na(x)] <- 9

  # If convert_9s is TRUE, convert 9s to 0s
  if (unidentified_to_0) {
    x[x == 9] <- 0
  }

  return(x)
}


handle_sex <- function(x, convert_sex = FALSE) {
  x <- as.numeric(x)
  if (convert_sex) {
    sex <- ifelse(x == 2, 0, x)
  } else {
    sex <- ifelse(x == 0, 2, x)
  }

  # Replace NA values in the sex vector with 9
  sex[is.na(sex)] <- 9

  return(sex)
}


handle_blanks <- function(x) {
  # Identify rows with values equal to " " or "NULL" in the specified column
  x[x %in% c(" ", "NULL")] <- 0
  return(x)
}

handle_code <- function(x){
  x[x %in% c(" ", "NULL", NA, "NA", "")] <- NA
  return(x)
}


handle_values <- function(x, values){
  x <- as.numeric(x)
  x[is.na(x) | !(x %in% values)] <- 0
  return(x)
}
