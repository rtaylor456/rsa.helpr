#' Clean date variable.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A date variable, written as a numeric YYYYMMDD.
#' @returns The converted date variable.
#' @export
handle_date <- function(x) {
  date <- suppressWarnings(as.Date(as.character(x), format = "%Y%m%d"))
  return(date)
}

# handle_date <- function(x) {
#   suppress_na_warning(as.Date(as.character(x), format = "%Y%m%d"))
# }


#' Clean Excel-based date variable.
#'
#' This function converts a RSA-911 Excel-based date variable written as one
#'    number with origin date 1899-12-30 to the appropriate date.
#'
#' @param x An Excel date variable, origin 1899-12-30.
#' @returns The converted date variable.
#' @export
handle_excel_date <- function(x){
  date <- suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30"))
  return(date)
}

# handle_excel_date <- function(x) {
#   suppress_na_warning(as.Date(as.numeric(x), origin = "1899-12-30"))
# }


#' Clean year variable.
#'
#' This function cleans a RSA-911 year variable containing a 4-digit year to
#'   the extracted 4-digit year.
#'
#' @param x A year variable, containing a 4-digit year somewhere in string.
#' @returns The converted year variable, just the digits.
#' @export
handle_year <- function(x) {
  year <- suppressWarnings(as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x)))
  return(year)
}
#
# handle_year <- function(x) {
#   suppress_na_warning(as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x)))
# }


#' Convert date variable.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A date variable, written as a numeric YYYYMMDD.
#' @returns The converted date variable.
#' @export
handle_nines <- function(x, unidentified_to_0 = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  # Replace NA values with 9
  x[is.na(x)] <- 9

  # If convert_9s is TRUE, convert 9s to 0s
  if (unidentified_to_0) {
    x[x == 9] <- 0
  }

  return(x)
}

# handle_nines <- function(x, unidentified_to_0 = TRUE) {
#   x <- suppress_na_warning(as.numeric(x))
#
#   # Replace NA values with 9
#   x[is.na(x)] <- 9
#
#   # If unidentified_to_0 is TRUE, convert 9s to 0s
#   if (unidentified_to_0) {
#     x[x == 9] <- 0
#   }
#
#   return(x)
# }


#' Clean sex/gender variable.
#'
#' This function cleans a RSA-911 sex/gender variable, based on user input.
#'
#' @param x A sex/gender variable, with values 1, 2, 9 (or NA), or 1, 0, 9.
#' @param convert_sex True or False, default is False.
#'   Converts 2s to 0s if True.
#' @returns The converted date variable.
#' @export
handle_sex <- function(x, convert_sex = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  if (convert_sex) {
    sex <- ifelse(x == 2, 0, x)
  } else {
    sex <- ifelse(x == 0, 2, x)
  }

  # Replace NA values in the sex vector with 9
  sex[is.na(sex)] <- 9

  return(sex)
}

# handle_sex <- function(x, convert_sex = FALSE) {
#   x <- suppress_na_warning(as.numeric(x))  # Suppress only NA coercion warnings
#
#   if (convert_sex) {
#     sex <- ifelse(x == 2, 0, x)
#   } else {
#     sex <- ifelse(x == 0, 2, x)
#   }
#
#   # Replace NA values in the sex vector with 9
#   sex[is.na(sex)] <- 9
#
#   return(sex)
# }


#' Clean a variable with blank values.
#'
#' This function cleans a RSA-911 variable with blanks and NULL values.
#'
#' @param x An RSA-911 variable.
#' @returns The cleaned variable, where blanks and NULLs are replaced with 0s.
#' @export
handle_blanks <- function(x) {
  # Identify rows with values equal to " " or "NULL" in the specified column
  x[x %in% c(" ", "NULL")] <- 0
  return(x)
}

# handle_blanks <- function(x) {
#   x[x %in% c(" ", "NULL")] <- 0  # Replace blanks and "NULL" with 0
#   x <- suppress_na_warning(as.numeric(x))  # Convert to numeric while suppressing coercion warnings
#   return(x)
# }


#' Convert character coded variables.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A variable containing code values, so values that have little
#'   meaning in numeric or factor form. Simply represent recorded ids.
#' @returns The cleaned code variable.
#' @export
handle_code <- function(x){
  x[x %in% c(" ", "NULL", NA, "NA", "")] <- NA
  return(x)
}

# handle_code <- function(x) {
#   x[x %in% c(" ", "NULL", "NA", NA, "")] <- NA  # Replace with NA
#   x <- suppress_na_warning(x)
#   return(x)
# }


#' Clean incorrect values.
#'
#' This function cleans incorrect values for a RSA-911 variable.
#'
#' @param x An RSA-911 variable with specific allowed values.
#' @param values A vector of permitted values for the variable.
#' @param blank_value The preferred replacement value for incorrect values.
#'   Default is to replace incorrect values to NA.
#' @returns The converted date variable.
#' @export
handle_values <- function(x, values, blank_value = NA){
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x) | !(x %in% values)] <- blank_value
  return(x)
}

# handle_values <- function(x, values, blank_value = NA) {
#   x <- suppress_na_warning(as.numeric(x))  # Convert to numeric while suppressing coercion warnings
#   x[is.na(x) | !(x %in% values)] <- blank_value  # Replace unwanted values with blank_value
#   return(x)
# }


#' CLean/abbreviate name variables.
#'
#' This function cleans and shortens name variables, for example, to applied on
#'   a variable such as Provider.
#'
#' @param x The name variable.
#' @returns The cleaned name variable, with abbreviated values.
#' @export
handle_abbrev <- function(x) {

  filler_words <- c("to", "of", "for", "and", "the", "in", "on", "at", "with",
                    "by", "from")

  sapply(x, function(name) {
    # Step 1: Remove special characters (e.g., (30), punctuation)
    name <- gsub("\\(.*?\\)|[^a-zA-Z\\s]", " ", name)

    # Step 2: Split name into words
    words <- unlist(strsplit(name, " "))

    # Step 3: Filter out filler words and keep only capitalized words
    words <- words[!(tolower(words) %in% filler_words)]  # Remove filler words
    capitalized_words <- words[grepl("^[A-Z]", words)]   # Keep only capitalized words

    # Step 4: Generate the acronym
    if (length(capitalized_words) > 1) {
      return(paste0(substr(capitalized_words, 1, 1), collapse = ""))  # Acronym from initials
    } else {
      return(name)  # Return full name if there's only one capitalized word
    }
  })
}

# handle_abbrev <- function(x) {
#   filler_words <- c("to", "of", "for", "and", "the", "in", "on", "at", "with", "by", "from")
#
#   sapply(x, function(name) {
#     name <- suppress_na_warning(name)  # Apply suppress_na_warning to avoid coercion warnings
#
#     if (is.na(name) || name == "") return(NA)  # Handle NA or empty strings
#
#     # Step 1: Remove special characters (e.g., (30), punctuation)
#     name <- gsub("\\(.*?\\)|[^a-zA-Z\\s]", " ", name)
#
#     # Step 2: Split name into words
#     words <- unlist(strsplit(name, " "))
#
#     # Step 3: Filter out filler words and keep only capitalized words
#     words <- words[!(tolower(words) %in% filler_words)]  # Remove filler words
#     capitalized_words <- words[grepl("^[A-Z]", words)]   # Keep only capitalized words
#
#     # Step 4: Generate the acronym
#     if (length(capitalized_words) > 1) {
#       return(paste0(substr(capitalized_words, 1, 1), collapse = ""))  # Acronym from initials
#     } else {
#       return(name)  # Return full name if there's only one capitalized word
#     }
#   })
# }


#' Clean variables in RSA-911 dataset with special characters.
#'
#' This function cleans variables with special characters by splitting values
#'   into separate variables that are separated by special characters.
#'
#' @param var An RSA-911 special character variable (vector) to be cleaned.
#' @param var_name A character vector of the name of the variable to be cleaned.
#' @param sep The special character that is separating values within the
#'   variable.
#' @returns The new variables derived from the inputted variable. They contain
#'   the cleaned, separated values without special characters.
#' @export
handle_splits <- function(var, var_name, sep = ";") {
  # Split the vector into a list
  split_values <- strsplit(as.character(var), split = sep, perl = TRUE)

  # Replace "NULL" strings and blanks with NA
  split_values <- lapply(split_values, function(x) ifelse(x %in% c("NULL", ""),
                                                          NA, x))

  # Determine the max number of splits
  max_splits <- max(lengths(split_values))

  # Generate new vector names
  new_var_names <- paste0(var_name, "_Place", seq_len(max_splits))

  # Create a matrix with values or NAs
  result_matrix <- t(sapply(split_values, function(x) {
    length(x) <- max_splits  # Ensure uniform length with NA padding
    x
  }))

  # Convert matrix to named list of vectors
  result_list <- setNames(as.list(as.data.frame(result_matrix,
                                                stringsAsFactors = FALSE)),
                          new_var_names)

  return(result_list)
}

# handle_splits <- function(var, var_name, sep = ";") {
#   # Apply suppress_na_warning to handle any warnings from coercion
#   var <- suppress_na_warning(var)
#
#   # Split the vector into a list
#   split_values <- strsplit(as.character(var), split = sep, perl = TRUE)
#
#   # Replace "NULL" strings and blanks with NA
#   split_values <- lapply(split_values, function(x) ifelse(x %in% c("NULL", ""),
#                                                           NA, x))
#
#   # Determine the max number of splits
#   max_splits <- max(lengths(split_values))
#
#   # Generate new vector names
#   new_var_names <- paste0(var_name, "_Place", seq_len(max_splits))
#
#   # Create a matrix with values or NAs
#   result_matrix <- t(sapply(split_values, function(x) {
#     length(x) <- max_splits  # Ensure uniform length with NA padding
#     x
#   }))
#
#   # Convert matrix to named list of vectors
#   result_list <- setNames(as.list(as.data.frame(result_matrix,
#                                                 stringsAsFactors = FALSE)),
#                           new_var_names)
#
#   return(result_list)
# }



#' Apply handle_splits() to RSA-911 dataset.
#'
#' This function cleans variables with special characters by splitting values
#'   into separate variables that are separated by special characters by
#'   applying the function within a dataset.
#'
#' @param data An RSA-911 dataset.
#' @param special_cols A character vector of the names of variables with special
#'   character values in the dataset.
#' @param sep The special character that is separating values within the
#'   variable.
#' @returns The cleaned RSA-911 dataset, with new variables that contain the
#'   cleaned, separated values without special characters.
#' @export
apply_handle_splits <- function(data, special_cols, sep = ";") {
  for (col in special_cols) {
    if (col %in% names(data)) {
      split_results <- handle_splits(data[[col]], col, sep)
      data <- cbind(data, as.data.frame(split_results, stringsAsFactors = FALSE))
      data[[col]] <- NULL  # Remove the original column
    }
  }
  return(data)
}

# apply_handle_splits <- function(data, special_cols, sep = ";") {
#   for (col in special_cols) {
#     if (col %in% names(data)) {
#       # Apply suppress_na_warning to the column data
#       data[[col]] <- suppress_na_warning(data[[col]])
#
#       # Apply handle_splits to the column
#       split_results <- handle_splits(data[[col]], col, sep)
#
#       # Combine the new split columns with the original data
#       data <- cbind(data, as.data.frame(split_results, stringsAsFactors = FALSE))
#
#       # Remove the original column
#       data[[col]] <- NULL
#     }
#   }
#   return(data)
# }


#' Clean primary and secondary disability variables in RSA-911 dataset.
#'
#' This function cleans primary and secondary disability variables with special
#'   characters by splitting impairment and cause values into separate variables
#'   that are separated by special characters.
#'
#' @param df An RSA-911 dataset to be cleaned.
#' @returns The cleaned RSA-911 dataset, with new, separate variables for
#'   primary and secondary cause and impairment, without special characters.
#' @export

separate_disability <- function(df) {
  # Convert to data.table if not already
  setDT(df)

  prim_disability <- grep("(?i)^(?=.*prim)(?=.*disab)(?!.*(desc))",
                       names(df), value = TRUE, perl = TRUE)

  second_disability <- grep("(?i)^(?=.*second)(?=.*disab)(?!.*(desc))",
                          names(df), value = TRUE, perl = TRUE)

  # Separate E43_Primary_Disability_911 into E43_Primary_Impairment_911 and
  #   E43_Primary_Cause_911
  df[, c("Primary_Impairment",
         "Primary_Cause") := tstrsplit(df[[prim_disability]],
                                               ";",
                                               fixed = TRUE)]

  # Separate E44_Secondary_Disability_911 into E44_Secondary_Impairment_911 and
  #   E44_Secondary_Cause_911
  df[, c("Secondary_Impairment",
         "Secondary_Cause") := tstrsplit(df[[second_disability]],
                                                 ";",
                                                 fixed = TRUE)]
  # remove original columns
  df[, (prim_disability) := NULL]
  df[, (second_disability) := NULL]

  return(df)
}

# separate_disability <- function(df) {
#   # Convert to data.table if not already
#   setDT(df)
#
#   # Apply suppress_na_warning to the primary and secondary disability columns
#   prim_disability <- grep("(?i)^(?=.*prim)(?=.*disab)(?!.*(desc))", names(df), value = TRUE, perl = TRUE)
#   second_disability <- grep("(?i)^(?=.*second)(?=.*disab)(?!.*(desc))", names(df), value = TRUE, perl = TRUE)
#
#   # Apply suppress_na_warning to columns before processing
#   df[[prim_disability]] <- suppress_na_warning(df[[prim_disability]])
#   df[[second_disability]] <- suppress_na_warning(df[[second_disability]])
#
#   # Separate E43_Primary_Disability_911 into E43_Primary_Impairment_911 and
#   #   E43_Primary_Cause_911
#   df[, c("Primary_Impairment", "Primary_Cause") := tstrsplit(df[[prim_disability]], ";", fixed = TRUE)]
#
#   # Separate E44_Secondary_Disability_911 into E44_Secondary_Impairment_911 and
#   #   E44_Secondary_Cause_911
#   df[, c("Secondary_Impairment", "Secondary_Cause") := tstrsplit(df[[second_disability]], ";", fixed = TRUE)]
#
#   # Remove original columns
#   df[, (prim_disability) := NULL]
#   df[, (second_disability) := NULL]
#
#   return(df)
# }


