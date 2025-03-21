source("visuals_ui.R")
source("visuals_metadata.R")
source("visuals_scores.R")
source("models_sidebar.R")
source("models_main.R")
source("models_scores.R")
source("models_metadata.R")


# Define server logic
server <- function(input, output, session) {

  # see if this addresses server connection problem
  observe({
    invalidateLater(30000, session)  # Sends a heartbeat every 30 seconds
  })

  # Reactive values to store data
  rv <- reactiveValues(
    rsa_data_cleaned = NULL,
    scores_data_cleaned = NULL,
    merged_data = NULL,
    metadata = NULL,

    new_data = NULL,
    dataset_type = NULL,
    selected_data = NULL
  )


  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)

    withProgress(message = 'Cleaning RSA-911 Data...', value = 0, {
      file_paths <- input$rsa_data$datapath
      file_types <- tools::file_ext(input$rsa_data$name)

      df_list <- mapply(function(path, type) {
        if (type == "csv") {
          fread(path, stringsAsFactors = FALSE)
        } else {
          as.data.table(read_excel(path))
        }
      }, file_paths, file_types, SIMPLIFY = FALSE)


      incProgress(1/3, detail = "Combining data...")
      df_combined <- do.call(rbind, df_list)

      rm(df_list)  # Remove the intermediate list
      gc()         # Force garbage collection

      specials_to_clean <- if (nzchar(input$clean_specials)) strsplit(input$clean_specials, ",\\s*")[[1]] else NULL

      incProgress(1/3, detail = "Cleaning data...")
      df_cleaned <- clean_utah(df_combined,
                               aggregate = input$aggregate_utah,
                               unidentified_to_0 = input$unidentified_to_0,
                               clean_specials = specials_to_clean,
                               remove_desc = input$remove_desc,
                               remove_strictly_na = input$remove_strictly_na)

      incProgress(1/3, detail = "Finalizing...")
      rv$rsa_data_cleaned <- df_cleaned
      return(df_cleaned)
    })
  })

  output$rsa_data_exists <- reactive({
    !is.null(read_and_clean_rsa_data()) &&
      nrow(read_and_clean_rsa_data()) > 0
  })

  # Function to read and clean scores data files
  read_and_clean_scores_data <- reactive({
    req(input$scores_data)

    withProgress(message = 'Cleaning Scores Data...', value = 0, {
      file_paths <- input$scores_data$datapath
      file_types <- tools::file_ext(input$scores_data$name)

      df_scores_list <- mapply(function(path, type) {
        if (type == "csv") {
          fread(path, stringsAsFactors = FALSE)
        } else {
          # read_excel(path, row.names = NULL)
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

      cleaned_scores <- clean_scores(data = df_scores_combined,
                                     state_filter = states_of_interest,
                                     clean_ID = input$clean_ID,
                                     aggregate = input$aggregate_scores,
                                     ID_col = id_col)

      # Check for required provider-related columns
      required_cols <- c(
        "provider", "service", "proctor", "mode", "pre_post",
        "completed", "caseload", "group_freq", "online_freq", "rural_freq"
      )

      found_cols <- c(
        grep("(?i)provider", names(df_scores_combined), value = TRUE,
             perl = TRUE),
        grep("(?i)serv", names(df_scores_combined), value = TRUE, perl = TRUE),
        grep("(?i)proctor", names(df_scores_combined), value = TRUE,
             perl = TRUE),
        grep("(?i)mode", names(df_scores_combined), value = TRUE,
             perl = TRUE),
        grep("(?i)^(?=.*pre)(?=.*post)", names(df_scores_combined),
             value = TRUE, perl = TRUE),
        grep("(?i)^(?=.*complete)|(?=.*date)", names(df_scores_combined),
             value = TRUE, perl = TRUE),
        grep("(?i)^(?=.*case)|(?=.*caseload)|(?=.*workload)",
             names(df_scores_combined), value = TRUE, perl = TRUE),
        grep("(?i)^(?=.*group)|(?=.*grp)(?=.*freq)", names(df_scores_combined),
             value = TRUE, perl = TRUE),
        grep("(?i)^(?=.*online)(?=.*freq)", names(df_scores_combined),
             value = TRUE, perl = TRUE),
        grep("(?i)^(?=.*rural)(?=.*freq)", names(df_scores_combined),
             value = TRUE, perl = TRUE)
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

  output$scores_data_exists <- reactive({
    !is.null(read_and_clean_scores_data()) &&
      nrow(read_and_clean_scores_data()) > 0
  })

  merged_data <- reactive({
    req(read_and_clean_rsa_data(), read_and_clean_scores_data())

    withProgress(message = 'Merging Data...', value = 0, {
      rsa_data <- read_and_clean_rsa_data()
      scores_data <- read_and_clean_scores_data()

      incProgress(1/2, detail = "Merging data...")
      merged <- merge_scores(rsa_data, scores_data,
                             quarterly_ID = input$quarterly_ID,
                             scores_ID = input$scores_ID)

      incProgress(1/2, detail = "Finalizing...")
      rv$merged_data <- merged
      return(merged)
    })
  })


  output$merged_data_exists <- reactive({
    !is.null(merged_data()) && nrow(merged_data()) > 0
  })


  generate_metadata <- reactive({
    # Ensure the button was clicked
    req(input$generate_metadata)
    # Ensure a source is selected
    req(input$meta_source)

    withProgress(message = 'Generating Metadata...', value = 0, {
      data <- NULL
      includes_scores <- FALSE

      if (!is.null(merged_data()) && input$meta_source == "merged") {
        incProgress(1/2, detail = "Condensing merged data...")
        data <- merged_data()
        includes_scores <- TRUE
      } else {
        incProgress(1/2, detail = "Condensing RSA-911 data...")
        data <- read_and_clean_rsa_data()
        includes_scores <- FALSE
      }

      metadata <- create_metadata(data, includes_scores = includes_scores)

      incProgress(1/2, detail = "Finishing up!")
      rv$metadata <- metadata
      return(metadata)
    })
  })


  output$metadata_exists <- reactive({
    # Store the result of generate_metadata() in a local variable
    metadata <- generate_metadata()

    # Check if metadata exists and has rows
    !is.null(metadata) && nrow(metadata) > 0
  })


  outputOptions(output, "rsa_data_exists", suspendWhenHidden = FALSE)
  outputOptions(output, "scores_data_exists", suspendWhenHidden = FALSE)
  outputOptions(output, "merged_data_exists", suspendWhenHidden = FALSE)
  outputOptions(output, "metadata_exists", suspendWhenHidden = FALSE)


  # Render the RSA-911 data table
  output$table_rsa <- renderDT({
    datatable(read_and_clean_rsa_data(),
              options = list(
                scrollX = TRUE,
                title = "Quarterly Data"
              ))
  })

  # Render the scores data table
  output$table_scores <- renderDT({
    datatable(read_and_clean_scores_data(),
              options = list(
                scrollX = TRUE,
                title = "Scores Data"
              ))
  })

  # Render the merged data table
  output$table_merged <- renderDT({
    datatable(merged_data(),
              options = list(
                scrollX = TRUE,
                title = "Merged Quarterly and Scores Data"
              ))
  })

  # Render the metadata table
  output$table_metadata <- renderDT({
    datatable(generate_metadata(),
              options = list(
                scrollX = TRUE,
                title = "Generated Metadata"
              ))
  })

  # Render the summary for RSA-911 data
  output$summary_rsa <- renderPrint({
    df <- read_and_clean_rsa_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", format(n_rows, big.mark = ","), "\n")
    cat("Number of Columns:", format(n_cols, big.mark = ","), "\n")
    cat("Unique Participant IDs:", format(unique_ids, big.mark = ","), "\n")
  })

  # Render the summary for scores data
  output$summary_scores <- renderPrint({
    df <- read_and_clean_scores_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", format(n_rows, big.mark = ","), "\n")
    cat("Number of Columns:", format(n_cols, big.mark = ","), "\n")
    cat("Unique Participant IDs:", format(unique_ids, big.mark = ","), "\n")
  })

  # Render the summary for merged data
  output$summary_merged <- renderPrint({
    df <- merged_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", format(n_rows, big.mark = ","), "\n")
    cat("Number of Columns:", format(n_cols, big.mark = ","), "\n")
    cat("Unique Participant IDs:", format(unique_ids, big.mark = ","), "\n")
  })

  # Render the summary for metadata
  output$summary_metadata <- renderPrint({
    df <- generate_metadata()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", format(n_rows, big.mark = ","), "\n")
    cat("Number of Columns:", format(n_cols, big.mark = ","), "\n")
    cat("Unique Participant IDs:", format(unique_ids, big.mark = ","), "\n")
  })

  # Render selected data UI
  output$data_ui <- renderUI({
    if (input$data_choice == "Upload New Dataset" && !is.null(rv$new_data)) {
      h3("Uploaded Data")
    } else {
      h3(input$data_choice)
    }
  })

  # Download handler for RSA-911 data
  output$download_rsa <- downloadHandler(
    filename = function() {
      paste("cleaned_rsa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$rsa_data_cleaned, file, row.names = FALSE)
    }
  )

  # Download handler for scores data
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("cleaned_scores_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$scores_data_cleaned, file, row.names = FALSE)
    }
  )

  # Download handler for merged data
  output$download_merged <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$merged_data, file, row.names = FALSE)
    }
  )

  # Download handler for metadata
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$metadata, file, row.names = FALSE)
    }
  )

  # Reactive expression to handle selected data
  selected_data <- reactive({
    if (input$data_choice == "Use Cleaned RSA-911 Data" &&
        !is.null(rv$rsa_data_cleaned)) {
      return(rv$rsa_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Scores Data" &&
               !is.null(rv$scores_data_cleaned)) {
      return(rv$scores_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Merged Data" &&
               !is.null(rv$merged_data)) {
      return(rv$merged_data)
    } else if (input$data_choice == "Use Generated Metadata" &&
               !is.null(rv$metadata)) {
      return(rv$metadata)

    } else if (input$data_choice == "Upload New Dataset") {
      req(input$new_data, input$dataset_type)

      # Read the first file
      file_path <- input$new_data$datapath
      file_type <- tools::file_ext(input$new_data$name)

      df_new_data <- if (file_type == "csv") {
        read.csv(file_path, stringsAsFactors = FALSE, row.names = NULL)
      } else {
        read_excel(file_path)
      }

      # Store the new dataset based on the selected type
      rv$new_data <- df_new_data
      rv$dataset_type <- input$dataset_type

      # return(df_new_data)
      return(rv$new_data)

    } else {
      # Clear the new data when switching to cleaned data sources
      rv$new_data <- NULL
      rv$dataset_type <- NULL

      # Check if data is available based on the selected choice
      if (input$data_choice == "Use Cleaned RSA-911 Data" &&
          is.null(rv$rsa_data_cleaned)) {
        return(NULL)
      } else if (input$data_choice == "Use Cleaned Scores Data" &&
                 is.null(rv$scores_data_cleaned)) {
        return(NULL)
      } else if (input$data_choice == "Use Cleaned Merged Data" &&
                 is.null(rv$merged_data)) {
        return(NULL)
      } else if (input$data_choice == "Use Generated Metadata" &&
                 is.null(rv$metadata)) {
        return(NULL)
      }
      # return(switch(input$data_choice,
      #               "Use Cleaned RSA-911 Data" = rv$rsa_data_cleaned,
      #               "Use Cleaned Scores Data" = rv$scores_data_cleaned,
      #               "Use Cleaned Merged Data" = rv$merged_data,
      #               "Use Generated Metadata" = rv$metadata))
      return(switch(input$data_choice,
                    "Use Cleaned Scores Data" = rv$scores_data_cleaned,
                    "Use Generated Metadata" = rv$metadata))
    }
  })


  # Dynamically render UI for data messages and file input
  output$data_ui <- renderUI({
    if (input$data_choice == "Upload New Dataset") {
      tagList(
        # fileInput("new_data", "Upload New Dataset", accept = c(".csv")),
        radioButtons("dataset_type", "Select Dataset Type",
                     # choices = c("RSA-911" = "rsa",
                     #             "Scores" = "scores",
                     #             "Merged" = "merged",
                     #             "Metadata" = "metadata"),
                     choices = c("Scores" = "scores",
                                 "Metadata" = "metadata"),
                     inline = TRUE)
      )
    } else {
      NULL
    }
  })


  # Render the selected data table
  output$table_selected_data <- renderDT({
    data <- selected_data()
    if (is.null(data)) {
      return(datatable(data.frame(Message = "No data available. Please ensure data has been cleaned or uploaded.")))
    }
    datatable(data,
              options = list(
                scrollX = TRUE,
                title = "Selected Data"
              ))
  })

  # Render the summary for selected data
  output$summary_selected_data <- renderPrint({
    data <- selected_data()
    if (is.null(data)) {
      cat("No data available. Please ensure data has been cleaned or uploaded.\n")
      return()
    }
    n_rows <- nrow(data)
    n_cols <- ncol(data)
    cat("Number of Rows:", format(n_rows, big.mark = ","), "\n")
    cat("Number of Columns:", format(n_cols, big.mark = ","), "\n")
  })

  output$data_select_check <- renderText({
    # Ensure new data is uploaded
    req(input$new_data)

    # Determine file type
    file_type <- tools::file_ext(input$new_data$name[1])

    # Read the uploaded data based on file type
    data <- if (file_type == "csv") {
      read.csv(input$new_data$datapath[1], stringsAsFactors = FALSE)
    } else if (file_type == "xlsx") {
      read_excel(input$new_data$datapath[1])
    } else {
      return("Unsupported file type. Please upload a CSV or XLSX file.")
    }

    # Check the conditions
    choice <- input$data_choice
    dataset_type <- input$dataset_type

    if (choice == "Upload New Dataset") {

      if (dataset_type == "scores") {
        # Check for the presence of required variables and absence of excluded
        #   variables
        include_patterns <- "(?i)score"
        exclude_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"

        included_variables <- grep(include_patterns, names(data), value = TRUE,
                                   perl = TRUE)
        excluded_variables <- grep(exclude_patterns, names(data),
                                   value = TRUE, perl = TRUE)

        if (length(included_variables) < 1 || length(excluded_variables) > 0) {
          return("THIS DOES NOT APPEAR TO BE A SCORES DATASET. Please ensure it is classified correctly and contains score variables and no RSA-911 variables.")
        }
      } else if (dataset_type == "metadata") {
        # Check for the presence of required variables, both demo. and scores
        demo_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
        score_patterns <- "(?i)score"

        demo_variables <- grep(demo_patterns, names(data), value = TRUE,
                               perl = TRUE)

        score_variables <- grep(score_patterns, names(data), value = TRUE,
                                perl = TRUE)

        participant_variable <- grep("(?i)participant|(?i)_ID", names(data),
                                     value = TRUE, perl = TRUE)

        if (length(participant_variable) == 0) {
          return("Participant ID variable not found in the metadata dataset.")
        }

        participants <- data[[participant_variable]]

        if (length(demo_variables) < 1 || length(score_variables) < 1) {
          return("THIS DOES NOT APPEAR TO BE A METADATA DATASET. Please ensure it is classified correctly and contains BOTH RSA-911 variables and score variables.")
        } # make sure we have have metadata, one row per participant
        if (length(participants) != length(unique(participants))) {
          return("THIS DOES NOT APPEAR TO BE A METADATA DATASET. Please ensure it is classified correctly and contains only one row per participant.")
        }
      }

    }
  })



  validate_uploaded_dataset <- function(data, dataset_type) {
    if (dataset_type == "scores") {
      include_patterns <- "(?i)score"
      exclude_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"

      included_variables <- grep(include_patterns, names(data), value = TRUE, perl = TRUE)
      excluded_variables <- grep(exclude_patterns, names(data), value = TRUE, perl = TRUE)

      if (length(included_variables) < 1) {
        return(list(valid = FALSE, message = "Error: No score-related columns detected. Please upload a dataset that includes at least one 'score' variable."))
      }
      if (length(excluded_variables) > 0) {
        return(list(valid = FALSE, message = "Error: Your dataset contains invalid columns (e.g., gender, sex, plan, disability). Please remove these columns and try again."))
      }
    } else if (dataset_type == "metadata") {
      demo_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
      score_patterns <- "(?i)score"

      demo_variables <- grep(demo_patterns, names(data), value = TRUE, perl = TRUE)
      score_variables <- grep(score_patterns, names(data), value = TRUE, perl = TRUE)
      participant_variable <- grep("(?i)participant|(?i)_ID", names(data), value = TRUE, perl = TRUE)

      if (length(participant_variable) == 0) {
        return(list(valid = FALSE, message = "Error: No participant ID column detected. Please include a 'participant' or '_ID' variable in your dataset."))
      }
      if (length(demo_variables) < 1) {
        return(list(valid = FALSE, message = "Error: No demographic-related columns detected. Please upload a dataset that includes demographic variables."))
      }
      if (length(score_variables) < 1) {
        return(list(valid = FALSE, message = "Error: No score-related columns detected. Your dataset must contain at least one 'score' variable."))
      }

      participants <- data[[participant_variable]]
      if (length(participants) != length(unique(participants))) {
        return(list(valid = FALSE, message = "Error: The dataset contains duplicate participant IDs. Ensure that each participant has a unique row."))
      }
    } else {
      return(list(valid = FALSE, message = "Error: No valid dataset type selected."))
    }

    # Return if dataset is valid
    return(list(valid = TRUE, message = NULL))
  }


  ## VISUALS
  output$visuals_ui <- renderUI({
    visuals_ui(input, selected_data)
  })


  # SCORES plots
  output$overview_plot1 <- overview_plot1()
  # Download handler for overview_plot1
  output$download_overview_plot1 <- download_overview_plot1()


  output$overview_plot2 <- overview_plot2()
  # Download handler for overview_plot2
  output$download_overview_plot2 <- download_overview_plot2()


  output$overview_plot3 <- overview_plot3()
  # Download handler for overview_plot3
  output$download_overview_plot3 <- download_overview_plot3()


  ## Across Services
  output$services_plot1 <- services_plot1()
  # For services_plot1
  output$download_services_plot1 <- download_services_plot1()


  output$services_plot2 <- services_plot2()
  # For services_plot2
  output$download_services_plot2 <- download_services_plot2()


  output$services_plot3 <- services_plot3()
  # For services_plot3
  output$download_services_plot3 <- download_services_plot3()



  ## Across Providers
  output$providers_plot1 <- providers_plot1()
  # For providers_plot1
  output$download_providers_plot1 <- download_providers_plot1()

  ## PROVIDER DATA plots
  output$provider_data_plot1 <- provider_data_plot1()
  # Download handler for overview_plot1
  output$download_provider_data_plot1 <- download_provider_data_plot1()


  output$provider_data_plot2 <- provider_data_plot2()
  # Download handler for overview_plot2
  output$download_provider_data_plot2 <- download_provider_data_plot2()


  output$provider_data_plot3 <- provider_data_plot3()
  # Download handler for overview_plot3
  output$download_provider_data_plot3 <- download_provider_data_plot3()


  output$provider_data_plot4 <- provider_data_plot4()
  # Download handler for overview_plot4
  output$download_provider_data_plot4 <- download_provider_data_plot4()


  output$provider_data_plot5 <- provider_data_plot5()
  # Download handler for overview_plot5
  output$download_provider_data_plot5 <- download_provider_data_plot5()


  ## METADATA text outputs
  output$social_variables_text <- social_variables_text(selected_data)

  output$gen_demo_label1 <- gen_demo_label1()


  output$gen_demo_label2 <- gen_demo_label2()

  output$gen_demo_label_race <- gen_demo_label_race()

  output$gen_demo_label_disability <- gen_demo_label_disability()


  output$post_secondary_text <- post_secondary_text(selected_data)

  output$meta_gen_demo_table <- meta_gen_demo_table(selected_data)

  output$post_secondary_table <- post_secondary_table(selected_data)

  ## METADATA plots
  # Time passed in program
  # output$meta_gen_demo_plot1 <- meta_gen_demo_plot1(selected_data)
  # output$meta_gen_demo_plot1 <- renderPlot({
  #   meta_gen_demo_plot1(selected_data)
  # })

  output$meta_gen_demo_plot1 <- renderPlot({
    req(selected_data())  # Ensure dataset is available
    meta_gen_demo_plot1(selected_data())  # Pass evaluated data, NOT the reactive function itself
  })


  # Download handler for Time in Programs Plot
  output$download_meta_gen_demo_plot1 <- download_meta_gen_demo_plot1(selected_data)


  # Enrollment length
  output$meta_gen_demo_plot2 <- meta_gen_demo_plot2(selected_data)
  output$meta_gen_demo_plot2 <- renderPlot({
    req(selected_data())  # Ensure dataset is available
    meta_gen_demo_plot2(selected_data())  # Pass evaluated data
  })
  # Download handler for Enrollment Length Plot
  output$download_meta_gen_demo_plot2 <- download_meta_gen_demo_plot2(selected_data)

  ## NO plot 3 anymore...

  output$meta_gen_demo_plot4 <- meta_gen_demo_plot4(selected_data)
  output$meta_gen_demo_plot4 <- renderPlot({
    req(selected_data())  # Ensure dataset is available
    meta_gen_demo_plot4(selected_data())  # Pass evaluated data, NOT the reactive function itself
  })
  # Download handler for Race Distribution Plot
  output$download_meta_gen_demo_plot4 <- download_meta_gen_demo_plot4(selected_data)

  # Severity
  output$meta_gen_demo_plot5 <- meta_gen_demo_plot5(selected_data)
  # Download handler for Severity Distribution Plot
  output$download_meta_gen_demo_plot5 <- download_meta_gen_demo_plot5(selected_data)

  # Primary impairment
  output$meta_gen_demo_plot6 <- meta_gen_demo_plot6(selected_data)
  # Download handler for Primary Impairment Distribution Plot
  output$download_meta_gen_demo_plot6 <- download_meta_gen_demo_plot6(selected_data)

  # Primary impairment
  output$meta_gen_demo_plot7 <- meta_gen_demo_plot7(selected_data)
  # Download handler for Secondary Impairment Distribution Plot
  output$download_meta_gen_demo_plot7 <- download_meta_gen_demo_plot7(selected_data)



  output$meta_diff_plot1 <- meta_diff_plot1(selected_data)
  output$download_meta_diff_plot1 <- download_meta_diff_plot1(selected_data)


  output$meta_diff_plot2 <- meta_diff_plot2(selected_data)
  # Download handler for meta_diff_plot2
  output$download_meta_diff_plot2 <- download_meta_diff_plot2(selected_data)

  output$meta_diff_plot3 <- meta_diff_plot3(selected_data)
  # Download handler for meta_diff_plot3
  output$download_meta_diff_plot3 <- download_meta_diff_plot3(selected_data)

  output$meta_diff_plot4 <- meta_diff_plot4(selected_data)
  # Download handler for meta_diff_plot4
  output$download_meta_diff_plot4 <- download_meta_diff_plot4(selected_data)

  output$meta_diff_plot5 <- meta_diff_plot5(selected_data)
  # Download handler for meta_diff_plot5
  output$download_meta_diff_plot5 <- download_meta_diff_plot5(selected_data)


  output$meta_diff_plot6 <- meta_diff_plot6(selected_data)
  # Download handler for meta_diff_plot6
  output$download_meta_diff_plot6 <- download_meta_diff_plot6(selected_data)

  output$meta_diff_plot7 <- meta_diff_plot7(selected_data)
  # Download handler for meta_diff_plot7
  output$download_meta_diff_plot7 <- download_meta_diff_plot7(selected_data)



  output$meta_wage_plot1 <- meta_wage_plot1(selected_data)
  # Download handler for meta_wage_plot1
  output$download_meta_wage_plot1 <- download_meta_wage_plot1(selected_data)

  output$meta_wage_plot2 <- meta_wage_plot2(selected_data)
  # Download handler for meta_wage_plot2
  output$download_meta_wage_plot2 <- download_meta_wage_plot2(selected_data)

  output$meta_wage_plot3 <- meta_wage_plot3(selected_data)
  # Download handler for meta_wage_plot3
  output$download_meta_wage_plot3 <- download_meta_wage_plot3(selected_data)

  output$meta_wage_plot4 <- meta_wage_plot4(selected_data)
  # Download handler for meta_wage_plot4
  output$download_meta_wage_plot4 <- download_meta_wage_plot4(selected_data)

  output$meta_wage_plot5 <- meta_wage_plot5(selected_data)
  output$download_meta_wage_plot5 <- download_meta_wage_plot5(selected_data)

  output$meta_wage_plot6 <- meta_wage_plot6(selected_data)
  output$download_meta_wage_plot6 <- download_meta_wage_plot6(selected_data)

  output$meta_wage_plot7 <- meta_wage_plot7(selected_data)
  output$download_meta_wage_plot7 <- download_meta_wage_plot7(selected_data)



  output$meta_employ_plot1 <- meta_employ_plot1(selected_data)
  output$download_meta_employ_plot1 <- download_meta_employ_plot1(selected_data)

  output$meta_employ_plot2 <- meta_employ_plot2(selected_data)
  output$download_meta_employ_plot2 <- download_meta_employ_plot2(selected_data)

  output$meta_employ_plot3 <- meta_employ_plot3(selected_data)
  output$download_meta_employ_plot3 <- download_meta_employ_plot3(selected_data)

  output$meta_employ_plot4 <- meta_employ_plot4(selected_data)
  output$download_meta_employ_plot4 <- download_meta_employ_plot4(selected_data)

  output$meta_employ_plot5 <- meta_employ_plot5(selected_data)
  output$download_meta_employ_plot5 <- download_meta_employ_plot5(selected_data)

  output$meta_employ_plot6 <- meta_employ_plot6(selected_data)
  output$download_meta_employ_plot6 <- download_meta_employ_plot6(selected_data)

  output$meta_employ_plot7 <- meta_employ_plot7(selected_data)
  output$download_meta_employ_plot7 <- download_meta_employ_plot7(selected_data)



  ###################
  ## MODELS SIDEBAR #
  ###################
  output$models_sidebar <- renderUI({
    render_models_sidebar(input, selected_data)
  })


  ################
  # SCORES MODEL #
  ################

  # Reactive value to track if the ANOVA test has been run
  # anova_run <- reactiveVal(FALSE)

  # model_scores <- reactive({
  #   req(selected_data())
  #   req(input$anova)
  #   data <- selected_data()
  #   anova_test <- input$anova
  #
  #   if (anova_test == "ANOVA across Services") {
  #     difference_cols <- grep("(?i)difference", names(data),
  #                             value = TRUE, perl = TRUE)
  #
  #     participant_col <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
  #                             names(data), value = TRUE, perl = TRUE)
  #
  #     # Exclude columns that contain "med" or "avail"
  #     exclude_patterns <- "(?i)med|avail"
  #     filtered_columns <- difference_cols[!grepl(exclude_patterns,
  #                                                difference_cols, perl = TRUE)]
  #
  #     differences <- data[, .SD, .SDcols = c(participant_col, filtered_columns)]
  #
  #     # Convert data to long format to input into ANOVA test function
  #     long_data <- melt(differences, id.vars = participant_col,
  #                       measure.vars = filtered_columns,
  #                       variable.name = "Services",
  #                       value.name = "Difference_Scores")
  #
  #     # Convert Services column to a factor
  #     long_data[, Services := factor(Services)]
  #
  #
  #     # Perform ANOVA
  #     # result <- aov(mean_value ~ service, data = means_df)
  #     result <- aov(Difference_Scores ~ Services, data = na.omit(long_data))
  #     # pairwise comparisons
  #     tukey_result <- TukeyHSD(result)
  #
  #
  #   } else if (anova_test == "ANOVA across Providers") {
  #
  #     result <- aov(Median_Difference_Score ~ Provider,
  #                   data = na.omit(data[, c("Median_Difference_Score",
  #                                           "Provider")]))
  #
  #     # pairwise comparisons
  #     tukey_result <- TukeyHSD(result)
  #
  #
  #   } else {
  #     return(NULL)  # Return NULL if no valid test is selected
  #   }
  #
  #   # Set anova_run to TRUE
  #   anova_run(TRUE)
  #
  #   list(anova = result, tukey = tukey_result)
  #
  # })


  anova_run <- reactiveVal(FALSE)

  model_scores <- reactive({
    req(selected_data(), input$anova)
    result <- compute_model_scores(selected_data, input$anova)

    if (!is.null(result)) {
      anova_run(TRUE)
    }

    result
  })


  output$model_scores_summary <- renderPrint({
    req(model_scores())

    # Extract and round the ANOVA table
    anova_table <- summary(model_scores()$anova)[[1]]  # Extract the main ANOVA table
    rounded_anova <- round(anova_table, 2)            # Round the numeric values

    print(rounded_anova)
  })


  output$model_scores_exists <- reactive({
    req(model_scores())  # Ensure model_scores() is not NULL
    !is.null(model_scores()$anova)
    # !is.null(model_scores())
  })



  # output$download_model_scores_summary <- downloadHandler(
  #   filename = function() {
  #     "anova_summary.txt"  # The name of the file to download
  #   },
  #   content = function(file) {
  #     # Extract and round the ANOVA table again for downloading
  #     anova_table <- summary(model_scores()$anova)[[1]]
  #     rounded_anova <- round(anova_table, 2)
  #
  #     # Write the summary to a text file
  #     write.table(rounded_anova, file, sep = "\t", col.names = NA, quote = FALSE)
  #   }
  # )
  #
  output$download_model_scores_summary <- downloadHandler(
    filename = function() { "anova_summary.txt" },
    content = function(file) {
      anova_table <- round(summary(model_scores()$anova)[[1]], 2)
      write.table(anova_table, file, sep = "\t", col.names = NA, quote = FALSE)
    }
  )



  output$tukey_scores_summary <- renderPrint({
    req(selected_data())
    req(input$anova)
    data <- selected_data()
    anova_test <- input$anova

    req(model_scores())
    tukey_result <- model_scores()$tukey

    if (anova_test == "ANOVA across Services") {
      # Get the Tukey HSD results for Services
      tukey_df <- as.data.frame(tukey_result$Services)
    } else if (anova_test == "ANOVA across Providers") {
      # Get the Tukey HSD results for Providers
      tukey_df <- as.data.frame(tukey_result$Provider)
    } else {
      # Print a message if the input is not recognized
      cat("Invalid selection for ANOVA test. Please choose 'Services' or 'Provider'.")
      return()
    }

    # Round numeric columns
    numeric_cols <- sapply(tukey_df, is.numeric)
    tukey_df[, numeric_cols] <- lapply(tukey_df[, numeric_cols], round, 2)

    # Filter only significant pairs (p-value < 0.05)
    significant_pairs <- tukey_df[tukey_df$`p adj` < 0.05, ]

    # If there are no significant pairs, print a message
    if (nrow(significant_pairs) == 0) {
      cat("No significant (alpha = 0.05) pairwise comparisons found. May be due to sampling sizes.")
    } else {
      print(significant_pairs)
    }
  })



  # Reactive function to create residual plots
  output$scores_residuals1 <- renderPlot({
    req(model_scores())

    # residuals
    model <- model_scores()$anova
    residuals <- resid(model)

    # histogram to look for normality
    hist(residuals, col = "steelblue")

  })

  output$scores_residuals2 <- renderPlot({
    req(model_scores())

    # residuals
    model <- model_scores()$anova
    residuals <- resid(model)

    # QQ plot to look for normality
    qqnorm(residuals)
    qqline(residuals, col = "steelblue")

  })

  output$scores_residuals3 <- renderPlot({
    req(model_scores())

    # Extract fitted values and residuals
    model <- model_scores()$anova
    residuals <- resid(model)
    fitted <- fitted(model)

    # Generate residuals vs. fitted values plot
    plot(fitted, residuals,
         main = "Residuals vs Fitted",
         xlab = "Fitted Values",
         ylab = "Residuals",
         pch = 19)

    # Add a horizontal line at y = 0 for reference
    abline(h = 0, col = "steelblue", lty = 2)
  })



  ##################
  # METADATA MODEL #
  ##################

  # model_run <- reactiveVal(FALSE)
  #
  # model_metadata <- reactive({
  #   req(selected_data())
  #   req(input$response)
  #   data <- selected_data()
  #   response <- input$response
  #
  #   if (response == "Predict Employment Outcome"){
  #     # employ_col <- grep("(?i)^(?=.*employment)(?!.*(?i)_desc)(?!.*(?i)_wage)(?!.*(?i)un)",
  #     #                    names(data), value = TRUE, perl = TRUE)
  #     # employ_col <- "E389_Q4_Employment_911"
  #     #
  #     #       exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
  #     #                             names(data), value = TRUE, perl = TRUE)
  #
  #     # if (length(exit_work_col) < 1){
  #     #   return("No employment variable available.")
  #     # } else{
  #     y <- "Final_Employment"
  #     # }
  #
  #
  #   } else if (response == "Predict Ending Wage") {
  #     wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
  #                      value = TRUE, perl = TRUE)
  #
  #     if (length(wage_col) < 1){
  #       return("No wage variable available.")
  #     } else{
  #       y <- wage_col
  #     }
  #   } else if (response == "Predict Median Difference Score"){
  #     median_diff_col <- "Median_Difference_Score"
  #
  #     if (length(median_diff_col) < 1){
  #       return("No median difference score variable available.")
  #     } else{
  #       y <- median_diff_col
  #     }
  #   }
  #
  #   predictors <- c()
  #
  #   if (input$gender) {
  #     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                     value = TRUE, perl = TRUE)
  #     if (length(sex_col) < 1){
  #       return("No gender/sex variable available.")
  #     } else{
  #       predictors <- c(predictors, sex_col)
  #     }
  #   }
  #
  #   if (input$race) {
  #     race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
  #                       names(data),
  #                       value = TRUE, perl = TRUE)
  #     if (length(race_cols) < 1){
  #       return("No race variable(s) available.")
  #     } else{
  #       predictors <- c(predictors, race_cols)
  #     }
  #   }
  #
  #   if (input$severity) {
  #     # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
  #     #                      names(data), value = TRUE, perl = TRUE)
  #     severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
  #                          names(data), value = TRUE, perl = TRUE)
  #     if (length(severity_col) < 1){
  #       return("No disability severity variable available.")
  #     } else{
  #       predictors <- c(predictors, severity_col)
  #     }
  #   }
  #
  #   if (input$enroll_length) {
  #     enroll_length_col <- grep("Enroll_Length",
  #                               names(data), value = TRUE, perl = TRUE)
  #     if (length(enroll_length_col) < 1){
  #       return("No enrollment length variable available.")
  #     } else{
  #       predictors <- c(predictors, enroll_length_col)
  #     }
  #   }
  #
  #   if (input$prim_impairment) {
  #     # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
  #     #                      names(data), value = TRUE, perl = TRUE)
  #     if (length("Primary_Impairment_Group") < 1){
  #       return("No primary impairment variable available.")
  #     } else{
  #       predictors <- c(predictors, "Primary_Impairment_Group")
  #     }
  #   }
  #
  #   if (input$second_impairment) {
  #     # second_dis_col <- grep("(?i)^(?=.*sec)(?=.*impairment)(?!.*(desc))",
  #     #                        names(data), value = TRUE, perl = TRUE)
  #     if (length("Secondary_Impairment_Group") < 1){
  #       return("No secondary impairment variable available.")
  #     } else{
  #       predictors <- c(predictors, "Secondary_Impairment_Group")
  #     }
  #   }
  #
  #   # Check if response and predictors are selected
  #   req(response, length(predictors) > 0)
  #
  #   formula <- as.formula(paste(y, "~",
  #                               paste(predictors, collapse = "+")))
  #
  #   # # Create the formula with interaction terms if specified
  #   # if (input$interactions) {
  #   #   interaction_terms <- paste(predictors, collapse = "*")
  #   #   formula <- as.formula(paste(y, "~", interaction_terms))
  #   # } else {
  #   #   formula <- as.formula(paste(y, "~", paste(predictors, collapse = "+")))
  #   # }
  #
  #   if (response == "Predict Ending Wage" ||
  #       response == "Predict Median Difference Score") {
  #     lm(formula = formula, data = data)
  #   } else if (response == "Predict Employment Outcome") {
  #     glm(formula, family = binomial, data = data)
  #   }
  #
  #   # Set model_run to TRUE when the model is executed
  #   # model_run(TRUE)
  #
  # })


  model_metadata <- reactive({
    generate_model_metadata(selected_data, input)
  })


  output$metadata_residuals1 <- metadata_residuals1()

  output$metadata_residuals2 <- metadata_residuals2()

  output$metadata_residuals3 <- metadata_residuals3()

  output$roc_explanation <- roc_explanation()

  output$binned_explanation <- binned_explanation()

  output$residuals_explanation <- residuals_explanation()

  output$histogram_explanation <- histogram_explanation()

  output$qqplot_explanation <- qqplot_explanation()


  # For ANOVA model
  output$residuals_explanation2 <- residuals_explanation2()

  output$histogram_explanation2 <- histogram_explanation2()

  output$qqplot_explanation2 <- qqplot_explanation2()



  output$model_metadata_summary <- model_metadata_summary()

  # output$model_metadata_exists <- reactive({
  #   !is.null(model_metadata())
  # })

  output$model_metadata_exists <- model_metadata_exists()


  ###################
  # MODELS MAIN TAB #
  ###################
  # Render the models main UI
  output$models_main <- renderUI({
    render_models_main(input, selected_data, anova_run)
  })


}
