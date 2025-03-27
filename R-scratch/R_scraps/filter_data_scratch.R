filter_data <- function(data,
                        gender = NULL,
                        race = NULL,  # List of race variables to check for value 1
                        severity = NULL,
                        age_group = NULL,
                        grade_level = NULL,
                        prim_impairment = NULL,
                        response = NULL) {

  # Define dictionaries for intuitive values to internal values
  gender_dict <- c("male" = "1", "female" = "2", "other" = "3", "dni" = "9")  # Example: male=1, female=2
  severity_dict <- c("non-significant" = "0", "significant" = "1", "most significant" = "2")
  age_group_dict <- c("<5" = "<5", "5-7" = "5-7", "8-10" = "8-10", "11-13" = "11-13", "14-16" = "14-16",
                      "17-19" = "17-19", "20-22" = "20-22", "23-25" = "23-25", "26-30" = "26-30",
                      "31-40" = "31-40", "41+" = "41+")
  prim_impairment_dict <- c("visual" = "Visual", "auditory" = "Aud_Comm", "intellectual" = "Intell_Learn",
                            "physical" = "Physical", "psychological" = "Psych", "none" = "None")

  # Convert data to data.table for efficient filtering
  data <- as.data.table(data)

  # Apply filters only if the corresponding argument is not NULL
  if (!is.null(gender)) {
    gender_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data), value = TRUE, perl = TRUE)
    # Translate intuitive gender values using the dictionary
    gender_values <- gender_dict[tolower(gender)]
    if (length(gender_values) > 0) {
      gender_match <- sapply(gender_col, function(col) data[[col]] %in% gender_values)
      data <- data[rowSums(gender_match) > 0, ]
    } else {
      warning("Invalid gender values provided.")
    }
  }

  if (!is.null(race)) {
    race_keywords <- tolower(race)  # Convert user input to lowercase
    race_pattern <- paste0("(?i)_(", paste(race_keywords, collapse = "|"), ")(?!.*(?i)_desc)")  # Create dynamic regex for race columns
    race_cols <- grep(race_pattern, names(data), value = TRUE, perl = TRUE)

    if (length(race_cols) > 0) {
      race_match <- sapply(race_cols, function(col) data[[col]] == 1)
      data <- data[rowSums(race_match) > 0, ]
    } else {
      warning("No matching race columns found.")
    }
  }

  if (!is.null(severity)) {
    severity_col <- grep(paste0("((?i)_disability_priority|(?i)_secondary_enrollment)(?!.*(?i)_desc)"),
                         names(data), value = TRUE, perl = TRUE)
    severity_values <- severity_dict[tolower(severity)]  # Translate severity using dictionary
    if (length(severity_values) > 0) {
      severity_match <- sapply(severity_col, function(col) data[[col]] %in% severity_values)
      data <- data[rowSums(severity_match) > 0, ]
    } else {
      warning("Invalid severity values provided.")
    }
  }

  if (!is.null(age_group)) {
    age_group_values <- age_group_dict[tolower(age_group)]  # Translate age group using the dictionary
    if (length(age_group_values) > 0) {
      data <- data[Age_Group %in% age_group_values]
    } else {
      warning("Invalid age group values provided.")
    }
  }

  if (!is.null(prim_impairment)) {
    prim_impairment_values <- prim_impairment_dict[tolower(prim_impairment)]  # Translate primary impairment using dictionary
    if (length(prim_impairment_values) > 0) {
      data <- data[Primary_Impairment_Group %in% prim_impairment_values]
    } else {
      warning("Invalid primary impairment values provided.")
    }
  }

  # Reduce dataset to response variable if provided
  if (!is.null(response)) {
    data <- data[, ..response]  # Retain only the response variable columns
  }

  return(data)
}



try <- filter_data(metadata, gender = c("other", "female"),
                   age_group = c("20-22", "23-25"))
