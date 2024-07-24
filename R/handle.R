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

# var_names is a character vector of variable names (place in quotes)
handle_splits <- function(data, var_names){
  for(var_name in var_names) {
    # split up the values by the ";"
    split_list <- strsplit(data[[var_name]], ";")
    # now, find out the max_length of elements after splitting
    max_length <- max(sapply(split_list, length))
    # now, add blanks to each element that doesn't have the same length as the
    #    max
    split_list <- lapply(split_list, function(x) {
      length(x) <- max_length; x[is.na(x)] <- ""; x
    })
    # create a matrix of these values
    split_matrix <- do.call(rbind, split_list)

    # Create the names for the new variables
    var_names_new <- paste0(sub("_911$", "", var_name), "_var",
                            seq_len(max_length),
                            "_911")

    # Create the new variable list with setNames
    new_var_list <- setNames(as.list(as.data.frame(split_matrix,
                                                   stringsAsFactors = FALSE)),
                             var_names_new)
    # Append the new variables to the data
    data <- cbind(data, new_var_list)
  }

  # Remove the original variables
  data <- subset(data, select = -which(names(data) %in% var_names))

  return(data)
}

# separate_disability <- function(df) {
#   df %>%
#     tidyr::separate(E43_Primary_Disability_911,
#                     into = c("E43_Primary_Impairment_911",
#                              "E43_Primary_Cause_911"),
#                     sep = ";") %>%
#     tidyr::separate(E44_Secondary_Disability_911,
#                     into = c("E44_Secondary_Impairment_911",
#                              "E44_Secondary_Cause_911"),
#                     sep = ";")
# }

separate_disability <- function(df) {
  # Convert to data.table if not already
  setDT(df)

  # Separate E43_Primary_Disability_911 into E43_Primary_Impairment_911 and
  #   E43_Primary_Cause_911
  df[, c("E43_Primary_Impairment_911",
         "E43_Primary_Cause_911") := tstrsplit(E43_Primary_Disability_911,
                                               ";",
                                               fixed = TRUE)]

  # Separate E44_Secondary_Disability_911 into E44_Secondary_Impairment_911 and
  #   E44_Secondary_Cause_911
  df[, c("E44_Secondary_Impairment_911",
         "E44_Secondary_Cause_911") := tstrsplit(E44_Secondary_Disability_911,
                                                 ";",
                                                 fixed = TRUE)]

  return(df)
}
