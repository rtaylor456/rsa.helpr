filter_data <- function(data,
                        gender = NULL,
                        race = NULL,  # List of race variables to check for value 1
                        severity = NULL,
                        age_group = NULL,
                        grade_level = NULL,
                        prim_impairment = NULL,
                        response = NULL) {

  # Define dictionaries for intuitive values to internal values
  gender_dict <- c("Male" = "1", "Female" = "2", "Other" = "3",
                   "Did not identify" = "9")
    # Example: male=1, female=2
  severity_dict <- c("Non-significant" = "0",
                     "Significant" = "1",
                     "Most significant" = "2")

  age_group_dict <- c("<5" = "<5",
                      "5-7" = "5-7",
                      "8-10" = "8-10",
                      "11-13" = "11-13",
                      "14-16" = "14-16",
                      "17-19" = "17-19",
                      "20-22" = "20-22",
                      "23-25" = "23-25",
                      "26-30" = "26-30",
                      "31-40" = "31-40",
                      "41+" = "41+")

  prim_impairment_dict <- c("Visual" = "Visual",
                            "Auditory/Communicative" = "Aud_Comm",
                            "Intellectual/Learning" = "Intell_Learn",
                            "Physical" = "Physical",
                            "Psychological" = "Psych",
                            "None" = "None")

  response_dict <- c("Median Difference Score" = "Median_Difference_Score",
                     "Ending Wage" = "E359_Exit_Hourly_Wage_911",
                     "Employment Outcome" = "Final_Employment",
                     "Post-secondary Enrollment" = "E84_PostSecondary_Enrollment_911")

  # Convert data to data.table for efficient filtering
  data <- as.data.table(data)

  # Apply filters only if the corresponding argument is not NULL
  if (!is.null(gender)) {
    gender_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                     value = TRUE, perl = TRUE)
    # Translate intuitive gender values using the dictionary
    gender_values <- gender_dict[gender]

    # # data <- data[ , (gender_col) %in% gender_values]
    #
    # # Check each gender column and keep rows where any column matches one of the gender values
    # gender_match <- sapply(gender_col, function(col) data[[col]] %in% gender_values)
    # # Use rowSums to check if any gender column has a match (if > 0, then keep the row)
    # data <- data[rowSums(gender_match) > 0, ]

    # Check the selected gender column and keep rows where the value matches one of the gender values
    gender_match <- data[[gender_col]] %in% gender_values

    # Filter rows based on gender match
    data <- data[gender_match, ]

  }

  if (!is.null(race)) {
    # Dynamically build the race regex pattern based on user input
    race_keywords <- tolower(race)  # Convert user input to lowercase
    race_pattern <- paste0("(?i)_(", paste(race_keywords, collapse = "|"),
                           ")(?!.*(?i)_desc)")

    # Identify race columns based on dynamic regex
    race_cols <- grep(race_pattern, names(data), value = TRUE, perl = TRUE)

    # # Filter rows where any of the race columns equals 1
    # race_filter <- rowSums(data[, race_cols, with = FALSE] == 1) > 0
    # data <- data[race_filter]

    # Filter rows where any of the race columns equals 1
    race_match <- sapply(race_cols, function(col) data[[col]] == "1")
    data <- data[rowSums(race_match) > 0, ]
  }

  if (!is.null(severity)) {
    severity_col <- grep("((?i)_disability_priority)(?!.*(?i)_desc)",
                         names(data),
                         value = TRUE, perl = TRUE)

    # Translate severity using the dictionary
    severity_values <- severity_dict[severity]

    # severity_match <- sapply(severity_col,
    #                          function(col) data[[col]] %in% severity_values)
    # data <- data[rowSums(severity_match) > 0, ]


    severity_match <- data[[severity_col]] %in% severity_values

    # Filter rows based on gender match
    data <- data[severity_match, ]
  }

  if (!is.null(age_group)) {
    # Translate age group using the dictionary
    age_group_values <- age_group_dict[tolower(age_group)]
    data <- data[Age_Group %in% age_group_values]

  }

  if (!is.null(prim_impairment)) {
    # Translate primary impairment group using the dictionary
    prim_impairment_values <- prim_impairment_dict[tolower(prim_impairment)]
    data <- data[Primary_Impairment_Group %in% prim_impairment_values]

  }

  # # Reduce dataset to response variable if provided
  # if (!is.null(response)) {
  #   data <- data[, ..response]
  # }

  # Reduce dataset to response variable if provided using the response_dict
  if (!is.null(response)) {
    response_col <- response_dict[response]  # Map user input to internal column name
    if (!is.null(response_col) && response_col %in% names(data)) {
      data <- data[, ..response_col, with = FALSE]  # Select the column based on the mapped internal name
    } else {
      warning("Response column not found in data.")
    }
  }

  return(data)
}
