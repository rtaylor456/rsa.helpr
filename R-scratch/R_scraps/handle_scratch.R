# handle_impairment_group <- function(x) {
#   fifelse(x == 0, "None",
#           fifelse(x %in% c(1, 2, 8), "Visual",
#                   fifelse(x %in% c(3, 4, 5, 6, 7, 9), "Aud_Comm",
#                           fifelse(x %in% c(10, 11, 12, 13, 14, 15, 16),
#                                   "Physical",
#                                   fifelse(x == 17, "Intell_Learn",
#                                           fifelse(x %in% c(18, 19), "Psych",
#                                                   NA_character_)
#                                   )
#                           )
#                   )
#           )
#   )
# }




convert_mixed_dates <- function(x) {
  sapply(x, function(val) {
    if (is.na(val) || val %in% c("NULL", "")) {
      return(NA)  # Convert "NULL" and empty strings to NA
    } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", val)) {
      return(as.Date(val))  # Already a proper date format
    } else if (suppressWarnings(!is.na(as.numeric(val)))) {
      return(as.Date(as.numeric(val), origin = "1899-12-30"))  # Convert Excel serial date
    } else {
      return(suppressWarnings(as.Date(val, format = "%Y-%m-%d")))  # Convert character date
    }
  }, USE.NAMES = FALSE)  # Prevent named output
}


#' Suppress unhelpful NAs warning.
#'
#' This function suppresses a warning only when the warning is "NAs introduced
#'   by coercion". This warning is not helpful within our rsa.helpr functions.

# suppress_na_warning <- function(func) {
#   suppressWarnings(tryCatch(
#     func,
#     warning = function(w) {
#       if (grepl("NAs introduced by coercion", w$message)) {
#         invokeRestart("muffleWarning")  # Suppress only this warning
#       } else {
#         warning(w)  # Show other warnings
#       }
#     }
#   ))
# }

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


# handle_date <- function(x) {
#   suppress_na_warning(as.Date(as.character(x), format = "%Y%m%d"))
# }


# handle_excel_date <- function(x) {
#   suppress_na_warning(as.Date(as.numeric(x), origin = "1899-12-30"))
# }


#
# handle_year <- function(x) {
#   suppress_na_warning(as.numeric(gsub("\\D*(\\d{4})\\D*", "\\1", x)))
# }


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


# handle_blanks <- function(x) {
#   x[x %in% c(" ", "NULL")] <- 0  # Replace blanks and "NULL" with 0
#   x <- suppress_na_warning(as.numeric(x))  # Convert to numeric while suppressing coercion warnings
#   return(x)
# }


# handle_code <- function(x) {
#   x[x %in% c(" ", "NULL", "NA", NA, "")] <- NA  # Replace with NA
#   x <- suppress_na_warning(x)
#   return(x)
# }


# handle_values <- function(x, values, blank_value = NA) {
#   x <- suppress_na_warning(as.numeric(x))  # Convert to numeric while suppressing coercion warnings
#   x[is.na(x) | !(x %in% values)] <- blank_value  # Replace unwanted values with blank_value
#   return(x)
# }


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



# suppress_na_warning <- function(func) {
#   suppressWarnings({
#     tryCatch(
#       func,
#       warning = function(w) {
#         if (grepl("NAs introduced by coercion", w$message)) {
#           # Return NA without printing the warning
#           return(NA)
#         } else {
#           # Rethrow the warning if it's not related to coercion
#           warning(w)
#         }
#       }
#     )
#   })
# }



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



# shorten_provider_names <- function(names, method = c("initials",
#                                                      "first_full")) {
#   sapply(names, function(name) {
#     words <- unlist(strsplit(name, " "))  # Split name into words
#     if (length(words) == 1) {
#       return(words)  # Return as is if only one word
#     } else if (method == "first_full") {
#       return(paste0(words[1], paste(substr(words[-1], 1, 1),
#                                     collapse = "")))
#       # Keep first word, initials of the rest
#     } else {  # Default: initials of all words
#       return(paste0(substr(words, 1, 1), collapse = ""))
#     }
#   })
# }
