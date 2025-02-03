#' Clean date variable.
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

#' Clean Excel-based date variable.
#'
#' This function converts a RSA-911 Excel-based date variable written as one
#'    number with origin date 1899-12-30 to the appropriate date.
#'
#' @param x An Excel date variable, origin 1899-12-30.
#' @returns The converted date variable.
#' @export
handle_excel_date <- function(x){
  date <- as.Date(as.numeric(x), origin = "1899-12-30")
  return(date)
}

#' Clean year variable.
#'
#' This function cleans a RSA-911 year variable containing a 4-digit year to
#'   the extracted 4-digit year.
#'
#' @param x A year variable, containing a 4-digit year somewhere in string.
#' @returns The converted year variable, just the digits.
#' @export
handle_year <- function(x) {
  year <- as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x))
  return(year)
}

#' Convert date variable.
#'
#' This function converts a RSA-911 date variable written in order YYYYMMDD as
#'    one number to the appropriate date.
#'
#' @param x A date variable, written as a numeric YYYYMMDD.
#' @returns The converted date variable.
#' @export
handle_nines <- function(x, unidentified_to_0 = TRUE) {
  x <- as.numeric(x)
  # Replace NA values with 9
  x[is.na(x)] <- 9

  # If convert_9s is TRUE, convert 9s to 0s
  if (unidentified_to_0) {
    x[x == 9] <- 0
  }

  return(x)
}

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

#' Cleaning incorrect values.
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
  x <- as.numeric(x)
  x[is.na(x) | !(x %in% values)] <- blank_value
  return(x)
}

# handle_splits <- function(data, var_name){
#   # split up the values by the ";"
#   split_list <- strsplit(data[[var_name]], ";")
#   # now, find out the max_length of elements after splitting
#   max_length <- max(sapply(split_list, length))
#   # now, add blanks to each element that doesn't have the same length as the max
#   split_list <- lapply(split_list, function(x) {
#     length(x) <- max_length; x[is.na(x)] <- ""; x
#     })
#   # create a matrix of these values
#   split_matrix <- do.call(rbind, split_list)
#
#   # new_var_list <- list()
#   # for (i in 1:max_length) {
#   #   var_name_i <- sub("_911$", paste0("_var", i), var_name)
#   #   new_var_list[[var_name_i]] <- split_matrix[, i]
#   # }
#
#   # Create the names for the new variables
#   var_names <- paste0(sub("_911$", "", var_name), "_var", seq_len(max_length),
#                       "_911")
#
#   # Create the new variable list with setNames
#   new_var_list <- setNames(as.list(as.data.frame(split_matrix,
#                                                  stringsAsFactors = FALSE)),
#                            var_names)
#   # Append the new variables to the data
#   data <- cbind(data, new_var_list)
#
#   # Remove the original variable
#   data <- subset(data, select = -which(names(data) == var_name))
#
#   return(data)
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
#'
# handle_splits <- function(data, var_names){
#   for(var_name in var_names) {
#     # split up the values by the ";"
#     split_list <- strsplit(data[[var_name]], ";")
#     # now, find out the max_length of elements after splitting
#     max_length <- max(sapply(split_list, length))
#     # now, add blanks to each element that doesn't have the same length as the
#     #    max
#     split_list <- lapply(split_list, function(x) {
#       length(x) <- max_length; x[is.na(x)] <- ""; x
#     })
#     # create a matrix of these values
#     split_matrix <- do.call(rbind, split_list)
#
#     # Create the names for the new variables
#     var_names_new <- paste0(sub("_911$", "", var_name), "_var",
#                             seq_len(max_length),
#                             "_911")
#
#     # Create the new variable list with setNames
#     new_var_list <- setNames(as.list(as.data.frame(split_matrix,
#                                                    stringsAsFactors = FALSE)),
#                              var_names_new)
#     # Append the new variables to the data
#     data <- cbind(data, new_var_list)
#   }
#
#   # Remove the original variables
#   data <- subset(data, select = -which(names(data) %in% var_names))
#
#   return(data)
# }

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
#'
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
#   # Filter columns that exist in the data
#   existing_cols <- intersect(special_cols, names(data))
#
#   # Apply handle_splits2 function to each column in existing_cols using map
#   split_results <- map(existing_cols, ~ handle_splits2(data[[.]], .x, sep))
#
#   # Combine the results with the original data
#   data <- bind_cols(data, as.data.frame(split_results, stringsAsFactors = FALSE))
#
#   # Remove the original columns
#   data <- select(data, -one_of(existing_cols))
#
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
