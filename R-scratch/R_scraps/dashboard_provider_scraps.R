read_and_clean_scores_data <- reactive({
  req(input$scores_data)

  withProgress(message = 'Cleaning Scores Data...', value = 0, {
    file_paths <- input$scores_data$datapath
    file_types <- tools::file_ext(input$scores_data$name)

    df_scores_list <- mapply(function(path, type) {
      if (type == "csv") {
        fread(path, stringsAsFactors = FALSE)
      } else {
        read_excel(path)
      }
    }, file_paths, file_types, SIMPLIFY = FALSE)

    incProgress(1/3, detail = "Combining data...")
    df_scores_combined <- do.call(rbind, df_scores_list)

    # Process state filter input
    states_of_interest <- if (nzchar(input$state_filter)) strsplit(input$state_filter, ",\\s*")[[1]] else NULL

    # Process ID column input
    id_col <- if (nzchar(input$ID_col)) input$ID_col else NULL

    incProgress(1/3, detail = "Cleaning data...")

    cleaned_scores <- clean_scores(
      data = df_scores_combined,
      state_filter = states_of_interest,
      clean_ID = input$clean_ID,
      aggregate = input$aggregate_scores,
      ID_col = id_col
    )

    # Check for required provider-related columns
    required_cols <- c(
      "provider", "service", "proctor", "mode", "pre_post",
      "completed", "caseload", "group_freq", "online_freq", "rural_freq"
    )

    found_cols <- c(
      grep("(?i)provider", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)serv", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)proctor", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)mode", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)^(?=.*pre)(?=.*post)", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)^(?=.*complete)|(?=.*date)", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)^(?=.*case)|(?=.*caseload)|(?=.*workload)", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)^(?=.*group)|(?=.*grp)(?=.*freq)", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)^(?=.*online)(?=.*freq)", names(df_scores_combined), value = TRUE, perl = TRUE),
      grep("(?i)^(?=.*rural)(?=.*freq)", names(df_scores_combined), value = TRUE, perl = TRUE)
    )

    if (length(found_cols) == length(required_cols)) {
      cleaned_provider <- clean_provider(
        data = df_scores_combined,
        state_filter = states_of_interest,
        ID_col = id_col
      )
      rv$provider_data_cleaned <- cleaned_provider
    }

    incProgress(1/3, detail = "Finalizing...")
    rv$scores_data_cleaned <- cleaned_scores

    return(cleaned_scores)
  })
})

# This way, `clean_provider` only runs when all necessary columns are detected, and the cleaned provider data is stored in `rv$provider_data_cleaned`. Let me know if you’d like me to tweak anything or add more conditions! 🚀
