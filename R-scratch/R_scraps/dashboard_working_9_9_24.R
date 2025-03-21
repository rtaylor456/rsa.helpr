library(shiny)
library(DT)
library(data.table)
library(readxl)
library(ggplot2)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("RSA-911 Data Exploration"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Upload & Cleaning',
                       sidebarPanel(
                         fluidRow(
                           column(12,
                                  h4("Please upload in .csv format.")),
                           column(12,
                                  h6("(.xlsx files are accepted, but will take longer.)"))
                         ),

                         tags$hr(style = "margin: 20px 0;"),

                         # RSA-911 Data Upload and Clean Options
                         fluidRow(
                           # column(12,
                           #        h4("(Please upload in CSV format)")),
                           column(12,
                                  h4("RSA-911 Data Upload and Clean Options")),
                           # column(12,
                           #        h4("(Please upload in CSV or xlsx format)")),
                           column(12,
                                  fileInput("rsa_data",
                                            "Choose RSA-911 File(s)",
                                            accept = c(".csv", ".xlsx"),
                                            multiple = TRUE)),
                           column(12,
                                  checkboxInput("aggregate_utah",
                                                "Aggregate Data",
                                                value = TRUE)),
                           column(12,
                                  checkboxInput("unidentified_to_0",
                                                "Convert Unidentified to 0",
                                                value = TRUE)),
                           column(12,
                                  checkboxInput("convert_sex", "Convert Sex",
                                                value = TRUE)),
                           # column(12,
                           #        checkboxInput("convert_employ",
                           #                      "Convert Employment",
                           #                      value = TRUE)),
                           column(12,
                                  checkboxInput("clean_specials",
                                                "Clean Specials",
                                                value = FALSE)),
                           column(12,
                                  checkboxInput("remove_desc",
                                                "Remove Description Columns",
                                                value = TRUE)),
                           column(12,
                                  checkboxInput("remove_strictly_na",
                                                "Remove Strictly NA Columns",
                                                value = TRUE)),
                           column(12,
                                  downloadButton("download_rsa",
                                                 "Download Cleaned RSA-911 Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Scores Data Upload and Clean Options
                         fluidRow(
                           column(12,
                                  h4("Scores Data Upload and Clean Options")),
                           column(12,
                                  fileInput("scores_data",
                                            "Choose Scores Data File(s)",
                                            accept = c(".csv", ".xlsx"),
                                            multiple = TRUE)),
                           column(12,
                                  checkboxInput("aggregate_scores",
                                                "Aggregate Data",
                                                value = TRUE)),
                           column(12,
                                  downloadButton("download_scores",
                                                 "Download Cleaned Scores Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Merge Options
                         fluidRow(
                           column(12, h4("RSA-911 and Scores Merge Options")),
                           column(6,
                                  textInput("quarterly_ID",
                                            "Quarterly Data ID Column",
                                            value = "Participant_ID")),
                           column(6,
                                  textInput("scores_ID",
                                            "Scores Data ID Column",
                                            value = "Participant_ID")),
                           column(12,
                                  downloadButton("download_merged",
                                                 "Download Cleaned Merged Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Metadata Generation
                         fluidRow(
                           column(12,
                                  h4("Metadata Generation")),
                           column(12,
                                  conditionalPanel(
                                    condition =
                                      "output.merged_data_exists == true",
                                    checkboxInput("use_merged",
                                                  "(Use Merged Data)",
                                                  value = TRUE)
                                  )
                           ),
                           column(12,
                                  actionButton("generate_metadata",
                                               "Generate Metadata")),
                           column(12,
                                  downloadButton("download_metadata",
                                                 "Download Cleaned Metadata"))
                         )
                       ),

                       mainPanel(
                         h3("Quarterly Data"),
                         DTOutput("table_rsa"),
                         verbatimTextOutput("summary_rsa"),
                         br(),

                         h3("Scores Data"),
                         DTOutput("table_scores"),
                         verbatimTextOutput("summary_scores"),
                         br(),

                         h3("Merged Quarterly and Scores Data"),
                         DTOutput("table_merged"),
                         verbatimTextOutput("summary_merged"),
                         br(),

                         h3("Generated Metadata"),
                         DTOutput("table_metadata"),
                         verbatimTextOutput("summary_metadata")
                       )
              ),
              tabPanel('Select Data (or load new)',
                       sidebarPanel(
                         selectInput("data_choice", "Select Data Source",
                                     choices = c(" ",
                                                 "Use Generated Metadata",
                                                 "Use Cleaned RSA-911 Data",
                                                 "Use Cleaned Scores Data",
                                                 "Use Cleaned Merged Data",
                                                 # "Use Generated Metadata",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition =
                             "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset",
                                     accept = c(".csv", ".xlsx")),
                           # uiOutput("validation_message"),
                           uiOutput("data_select_check")
                           # radioButtons("dataset_type", "Select Dataset Type",
                           #              choices = c("RSA-911" = "rsa",
                           #                          "Scores" = "scores",
                           #                          "Merged" = "merged",
                           #                          "Metadata" = "metadata"),
                           #              inline = TRUE)
                         )
                       ),
                       mainPanel(
                         uiOutput("data_ui"),
                         # Use uiOutput to dynamically render the content
                         DTOutput("table_selected_data"),
                         verbatimTextOutput("summary_selected_data"),
                         uiOutput("validation_message")
                         # uiOutput("data_select_check")
                       )
              ),
              # tabPanel('Visualizations'),
              tabPanel('Visualizations',
                       mainPanel(
                         uiOutput("visuals_ui")
                       )
              ),

              tabPanel('Modeling',
                       # mainPanel(
                       #   uiOutput("models_ui")
                       # )
                       sidebarPanel(
                         uiOutput("models_sidebar")
                       ),
                       mainPanel(
                         uiOutput("models_main")
                       )
              )
  )
)

# Define server logic
server <- function(input, output, session) {

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

      incProgress(1/3, detail = "Cleaning data...")
      df_cleaned <- clean_utah(df_combined,
                               aggregate = input$aggregate_utah,
                               unidentified_to_0 = input$unidentified_to_0,
                               convert_sex = input$convert_sex,
                               # convert_employ = input$convert_employ,
                               clean_specials = input$clean_specials,
                               remove_desc = input$remove_desc,
                               remove_strictly_na = input$remove_strictly_na)

      incProgress(1/3, detail = "Finalizing...")
      rv$rsa_data_cleaned <- df_cleaned
      return(df_cleaned)
    })
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
          read_excel(path, row.names = NULL)
        }
      }, file_paths, file_types, SIMPLIFY = FALSE)

      incProgress(1/3, detail = "Combining data...")
      df_scores_combined <- do.call(rbind, df_scores_list)

      incProgress(1/3, detail = "Cleaning data...")
      cleaned_scores <- clean_scores(df_scores_combined,
                                     aggregate = input$aggregate_scores)

      incProgress(1/3, detail = "Finalizing...")
      rv$scores_data_cleaned <- cleaned_scores
      return(cleaned_scores)
    })
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

  outputOptions(output, "merged_data_exists", suspendWhenHidden = FALSE)


  # Function to create metadata from merged data
  generate_metadata <- eventReactive(input$generate_metadata, {
    withProgress(message = 'Generating Metadata...', value = 0, {
      if (is.null(merged_data()) | nrow(merged_data()) < 1){
        incProgress(1/2, detail = "Condensing RSA-911 data, merged data not available...")
        metadata <- create_metadata(read_and_clean_rsa_data(),
                                    includes_scores = FALSE)
      } else if (input$use_merged){
        incProgress(1/2, detail = "Condensing merged data...")
        metadata <- create_metadata(merged_data(),
                                    includes_scores = TRUE)
      } else {
        incProgress(1/2, detail = "Condensing RSA-911 data...")
        metadata <- create_metadata(read_and_clean_rsa_data(),
                                    includes_scores = FALSE)
      }

      incProgress(1/2, detail = "Finishing up!...")
      rv$metadata <- metadata
      return(metadata)
    })
  })


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
      return(switch(input$data_choice,
                    "Use Cleaned RSA-911 Data" = rv$rsa_data_cleaned,
                    "Use Cleaned Scores Data" = rv$scores_data_cleaned,
                    "Use Cleaned Merged Data" = rv$merged_data,
                    "Use Generated Metadata" = rv$metadata))
    }
  })


  # Dynamically render UI for data messages and file input
  output$data_ui <- renderUI({
    if (input$data_choice == "Upload New Dataset") {
      tagList(
        # fileInput("new_data", "Upload New Dataset", accept = c(".csv")),
        radioButtons("dataset_type", "Select Dataset Type",
                     choices = c("RSA-911" = "rsa",
                                 "Scores" = "scores",
                                 "Merged" = "merged",
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

      if (dataset_type == "rsa") {
        # Check for the presence of required variables and absence of excluded variables
        include_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
        exclude_patterns <- "(?i)score"

        included_variables <- grep(include_patterns, names(data), value = TRUE,
                                   perl = TRUE)
        excluded_variables <- grep(exclude_patterns, names(data), value = TRUE,
                                   perl = TRUE)

        if (length(included_variables) < 1 || length(excluded_variables) > 0) {
          return("THIS DOES NOT APPEAR TO BE AN RSA-911 DATASET. Please ensure it is classified correctly and contains RSA-911 variables and no score variables.")
        }
      }

      else if (dataset_type == "scores") {
        # Check for the presence of required variables and absence of excluded variables
        include_patterns <- "(?i)score"
        exclude_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"

        included_variables <- grep(include_patterns, names(data), value = TRUE,
                                   perl = TRUE)
        excluded_variables <- grep(exclude_patterns, included_variables,
                                   value = TRUE, perl = TRUE)

        if (length(included_variables) < 1 || length(excluded_variables) > 0) {
          return("THIS DOES NOT APPEAR TO BE A SCORES DATASET. Please ensure it is classified correctly and contains score variables and no RSA-911 variables.")
        }
      }

      else if (dataset_type == "merged") {
        # Check for the presence of required variables and absence of excluded variables
        demo_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
        score_patterns <- "(?i)score"

        demo_variables <- grep(demo_patterns, names(data), value = TRUE,
                               perl = TRUE)
        score_variables <- grep(score_patterns, names(data), value = TRUE,
                                perl = TRUE)

        if (length(demo_variables) < 1 || length(score_variables) < 1) {
          return("THIS DOES NOT APPEAR TO BE A MERGED DATASET. Please ensure it is classified correctly and contains BOTH RSA-911 variables and score variables.")
        }
      }

      else if (dataset_type == "metadata") {
        # Check for the presence of required variables and absence of excluded variables
        # demo_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
        # score_patterns <- "(?i)score"
        #
        # demo_variables <- grep(demo_patterns, names(data), value = TRUE, perl = TRUE)
        # score_variables <- grep(score_patterns, names(data), value = TRUE, perl = TRUE)
        #
        # Check for the presence of required variables and absence of excluded variables
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
        }
        if (length(participants) != length(unique(participants))) {
          return("THIS DOES NOT APPEAR TO BE A METADATA DATASET. Please ensure it is classified correctly and contains only one row per participant.")
        }
      }

    }
  })

  ## VISUALS
  output$visuals_ui <- renderUI({
    data <- selected_data()
    data_choice <- input$data_choice
    dataset_type <- input$dataset_type

    if ((data_choice == "Use Cleaned RSA-911 Data") ||
        (data_choice == "Upload New Dataset" && dataset_type == "rsa")) {
      tabsetPanel(
        tabPanel("Demographics",
                 plotOutput("demographics_plot1"),
                 plotOutput("demographics_plot2"),
                 plotOutput("demographics_plot3"),
                 plotOutput("demographics_plot4")),
        tabPanel("Enrollment Length",
                 plotOutput("enrollment_plot1"), plotOutput("enrollment_plot2"),
                 plotOutput("enrollment_plot3"))
      )
    } else if ((data_choice == "Use Cleaned Scores Data") ||
               (data_choice == "Upload New Dataset" &&
                dataset_type == "scores")) {
      tabsetPanel(
        tabPanel("Across Services",
                 plotOutput("services_plot1"), plotOutput("services_plot2"),
                 plotOutput("services_plot3")),
        tabPanel("Across Providers",
                 plotOutput("providers_plot1")
                 # plotOutput("providers_plot2")
        )
      )
    } else if ((data_choice == "Use Cleaned Merged Data") ||
               (data_choice == "Upload New Dataset" &&
                dataset_type == "merged")) {
      tabsetPanel(
        tabPanel("General Demographics",
                 plotOutput("gen_demo_plot1"), plotOutput("gen_demo_plot2"),
                 plotOutput("gen_demo_plot3"), plotOutput("gen_demo_plot4"),
                 plotOutput("gen_demo_plot5")),
        tabPanel("Demographics & Scores",
                 plotOutput("demo_scores_plot1"),
                 plotOutput("demo_scores_plot2"),
                 plotOutput("demo_scores_plot3"),
                 plotOutput("demo_scores_plot4"),
                 plotOutput("demo_scores_plot5"),
                 plotOutput("demo_scores_plot6"))
      )
    }  else if ((data_choice == "Use Generated Metadata") ||
                (data_choice == "Upload New Dataset" &&
                 dataset_type == "metadata")) {
      tabsetPanel(
        tabPanel("General Demographics",
                 plotOutput("meta_gen_demo_plot1"),
                 plotOutput("meta_gen_demo_plot2"),
                 plotOutput("meta_gen_demo_plot3"),
                 plotOutput("meta_gen_demo_plot4"),
                 plotOutput("meta_gen_demo_plot5")),
        tabPanel("Investigate Difference Scores",
                 plotOutput("meta_diff_plot1"),
                 plotOutput("meta_diff_plot2"),
                 plotOutput("meta_diff_plot3"),
                 plotOutput("meta_diff_plot4"),
                 plotOutput("meta_diff_plot5"),
                 plotOutput("meta_diff_plot6")),
        tabPanel("Investigate Wage",
                 plotOutput("meta_wage_plot1"),
                 plotOutput("meta_wage_plot2"),
                 plotOutput("meta_wage_plot3"),
                 plotOutput("meta_wage_plot4"),
                 plotOutput("meta_wage_plot5"),
                 plotOutput("meta_wage_plot6")),
        tabPanel("Investigate Employment",
                 plotOutput("meta_employ_plot1"),
                 plotOutput("meta_employ_plot2"),
                 plotOutput("meta_employ_plot3"),
                 plotOutput("meta_employ_plot4"),
                 plotOutput("meta_employ_plot5"),
                 plotOutput("meta_employ_plot6"))
      )
    }
  })


  ## RSA-911 plots
  output$demographics_plot1 <- renderPlot({ plot(rnorm(100)) })
  output$demographics_plot2 <- renderPlot({ plot(rnorm(100)) })
  output$demographics_plot3 <- renderPlot({ plot(rnorm(100)) })
  output$demographics_plot4 <- renderPlot({ plot(rnorm(100)) })

  output$enrollment_plot1 <- renderPlot({ plot(rnorm(100)) })
  output$enrollment_plot2 <- renderPlot({ plot(rnorm(100)) })
  output$enrollment_plot3 <- renderPlot({ plot(rnorm(100)) })

  ## SCORES plots
  output$services_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    difference_cols <- grep("(?i)difference", names(data),
                            value = TRUE, perl = TRUE)
    # differences <- data[, .SD, .SDcols = difference_cols]

    # Exclude columns that contain "med" or "avail"
    exclude_patterns <- "(?i)med|avail"
    filtered_columns <- difference_cols[!grepl(exclude_patterns,
                                               difference_cols,
                                               perl = TRUE)]

    differences <- data[, .SD, .SDcols = filtered_columns]

    # Find the overall median for all differences scores
    differences_scores_vector <- as.vector(unlist(differences))
    differences_median <- median(differences_scores_vector, na.rm = TRUE)

    par(las = 2)
    boxplot(differences,
            names = sub("Difference_", "", names(differences)),
            main = "Distributions of Difference Scores Across Services",
            ylab = "Difference Scores",
            xlab = "Service Test Category",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = differences_median, lty = 1, lwd = 3, col = "steelblue")
    par(las = 1)

  })

  output$services_plot2 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    pre_cols <- grep("(?i)pre", names(data),
                     value = TRUE, perl = TRUE)
    pre_scores <- data[, .SD, .SDcols = pre_cols]

    # Find the overall median for all differences scores
    pre_scores_vector <- as.vector(unlist(pre_scores))
    pre_scores_median <- median(pre_scores_vector, na.rm = TRUE)

    par(las = 2)
    boxplot(pre_scores,
            names = sub("Pre_Score_", "", names(pre_scores)),
            main = "Distributions of Pre Scores Across Services",
            ylab = "Pre Scores",
            xlab = "Service Test Category",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = pre_scores_median, lty = 2, lwd = 3, col = "steelblue")
    par(las = 1)
  })

  output$services_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    post_cols <- grep("(?i)post", names(data),
                      value = TRUE, perl = TRUE)
    post_scores <- data[, .SD, .SDcols = post_cols]

    # Find the overall median for all differences scores
    post_scores_vector <- as.vector(unlist(post_scores))
    post_scores_median <- median(post_scores_vector, na.rm = TRUE)

    ########## need this for second line that shows pre_score_median too
    pre_cols <- grep("(?i)pre", names(data),
                     value = TRUE, perl = TRUE)
    pre_scores <- data[, .SD, .SDcols = pre_cols]

    # Find the overall median for all differences scores
    pre_scores_vector <- as.vector(unlist(pre_scores))
    pre_scores_median <- median(pre_scores_vector, na.rm = TRUE)
    ##########

    par(las = 2)
    boxplot(post_scores,
            names = sub("Post_Score_", "", names(post_scores)),
            main = "Distributions of Post Scores Across Services",
            ylab = "Post Scores",
            xlab = "Service Test Category",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = pre_scores_median, lty = 2, lwd = 3, col = "steelblue")
    abline(h = post_scores_median, lty = 3, lwd = 3, col = "steelblue")
    par(las = 1)
  })

  # output$providers_plot1 <- renderPlot({ plot(rnorm(100)) })

  output$providers_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # provider_col <- grep("(?i)provider", names(data),
    #                   value = TRUE, perl = TRUE)
    #
    # providers <- data[, .SD, .SDcols = provider_col]

    # want to create a table that prints the distribution of providers too

    # we know this exact variable name because it's created during our
    #   cleaning
    median_diff_col <- "Median_Difference_Score"

    median_diff_scores <- data[, .SD, .SDcols = median_diff_col]

    # Find the overall median for all median differences scores
    median_diff_scores_vector <- as.vector(unlist(median_diff_scores))
    overall_median <- median(median_diff_scores_vector, na.rm = TRUE)

    par(las = 2)
    boxplot(Median_Difference_Score ~ Provider,
            data = data,
            main = "Median Difference Scores Across Providers",
            ylab = "Median Difference Score",
            cex.axis = 0.7,
            col = "lightsteelblue")
    abline(h = overall_median, lty = 1, lwd = 3, col = "steelblue")
    par(las = 1)
  })


  ## MERGED plots
  output$gen_demo_plot1 <- renderPlot({ plot(rnorm(100)) })
  output$gen_demo_plot2 <- renderPlot({ plot(rnorm(100)) })
  output$gen_demo_plot3 <- renderPlot({ plot(rnorm(100)) })
  output$gen_demo_plot4 <- renderPlot({ plot(rnorm(100)) })
  output$gen_demo_plot5 <- renderPlot({ plot(rnorm(100)) })

  output$demo_scores_plot1 <- renderPlot({ plot(rnorm(100)) })
  output$demo_scores_plot2 <- renderPlot({ plot(rnorm(100)) })
  output$demo_scores_plot3 <- renderPlot({ plot(rnorm(100)) })
  output$demo_scores_plot4 <- renderPlot({ plot(rnorm(100)) })
  output$demo_scores_plot5 <- renderPlot({ plot(rnorm(100)) })
  output$demo_scores_plot6 <- renderPlot({ plot(rnorm(100)) })

  ## METADATA plots
  output$meta_gen_demo_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    if (is.null(data)) {
      return("No data available.")
    }

    hist(data$Enroll_Length,
         col = "steelblue",
         main = "Distribution of Enrollment Lengths",
         xlab = "Enrollment Length (Quarters)")

  })


  # output$meta_gen_demo_plot1 <- renderPlot({
  #   req(selected_data())
  #   data <- selected_data()
  #
  # if (is.null(data)) {
  #   return()
  # }
  #
  #   if ("Participant_ID" %in% names(data)) {
  #     # Ensure the enrollment length column exists
  #     if ("Enroll_Length" %in% names(data)) {
  #       ggplot(data, aes(x = Enroll_Length)) +
  #         geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  #         labs(title = "Histogram of Enrollment Length",
  #              x = "Enrollment Length", y = "Frequency")
  #     } else {
  #       ggplot() +
  #         labs(title = "No Enrollment Length Data", x = "", y = "") +
  #         geom_text(aes(x = 1, y = 1,
  #                       label = "Enrollment Length column not found"),
  #                   size = 5, color = "red")
  #     }
  #   } else {
  #     ggplot() +
  #       labs(title = "Invalid Data Type", x = "", y = "") +
  #       geom_text(aes(x = 1, y = 1, label = "Data is not RSA-911"),
  #                 size = 5, color = "red")
  #   }
  #   })

  output$meta_gen_demo_plot2 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    if (is.null(data)) {
      return()
    }

  })


  output$meta_gen_demo_plot3 <- renderPlot({ plot(rnorm(100)) })
  output$meta_gen_demo_plot4 <- renderPlot({ plot(rnorm(100)) })
  output$meta_gen_demo_plot5 <- renderPlot({ plot(rnorm(100)) })


  output$meta_diff_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         data$Median_Difference_Score,
         main = "Difference Scores Across Time in Program",
         ylab = "Median Difference Scores",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)
  })

  output$meta_diff_plot2 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    plot(as.numeric(data$Enroll_Length), data$Median_Difference_Score,
         col = "steelblue",
         main = "Difference Scores Across Quarters Enrolled",
         ylab = "Median Difference Score",
         xlab = "Total Quarters Enrolled",
         pch = 8)

  })

  output$meta_diff_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # the name for the gender/sex column could be varied, so we need to
    #   account for this possibility
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    boxplot(Median_Difference_Score ~ data[[sex_col]], data = data,
            main = "Difference Scores by Gender",
            names = c("Males", "Females", "Did not identify"),
            xlab = "Gender",
            ylab = "Median Difference Scores",
            col = "steelblue")

  })

  output$meta_diff_plot5 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # the name for the gender/sex column could be varied, so we need to
    #   account for this possibility
    # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
    #                      names(data), value = TRUE, perl = TRUE)
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    boxplot(Median_Difference_Score ~ data[[severity_col]], data = data,
            main = "Difference Scores by Disability Severity",
            names = c("Non significant", "Significant",
                      "Most significant"),
            xlab = "Disability Severity",
            ylab = "Median Difference Scores",
            col = "steelblue")

  })

  output$meta_diff_plot6 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
    #                      names(data), value = TRUE, perl = TRUE)

    boxplot(Median_Difference_Score ~
              Primary_Impairment_Group,
            data = data,
            main = "Difference Scores by Primary Disability Type",
            xlab = "Primary Disability",
            ylab = "Median Difference Scores",
            col = "steelblue")

  })

  output$meta_diff_plot4 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    data_subset <- data[, .SD, .SDcols = c("Median_Difference_Score",
                                           race_cols)]


    # Create a logical vector to filter rows
    # data_subset <- data_subset[ ,
    #                             keep_rows := apply(.SD, 1,
    #                                                function(row) any(row == 1)),
    #                             .SDcols = race_cols]
    # filtered_data <- data_subset[keep_rows == TRUE]
    #
    # # Remove the temporary 'keep_rows' column
    # filtered_data[, keep_rows := NULL]

    # Create a long-format data.table
    long_data <- melt(data_subset,
                      id.vars = "Median_Difference_Score",
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")
    # Filter rows where Has_Race is 1
    filtered_data <- long_data[Has_Race == 1]

    # Adjust the outer margins, so that the bottom doesn't get cut off
    par(oma = c(0, 0, 0, 0) + 0.6)
    boxplot(Median_Difference_Score ~ Race, data = filtered_data,
            # names = gsub("^E[0-9]+_|_911$", "", race_cols),
            col = "steelblue",
            xaxt = "n",
            yaxt = "n",
            xlab = "",
            ylab = "Median Difference Score",
            main = "Difference Scores Across Race"
    )
    # axis(side = 1, labels = FALSE) # this adds in x-axis tick marks
    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    text(x = 1:length(race_cols),
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),
         xpd = NA,
         ## Rotate the labels by 45 degrees.
         srt = 45,
         cex = .8,
         adj = 1)


    # Plot the boxplots
    # ggplot(filtered_data, aes(x = Race, y = Median_Difference_Score)) +
    #   geom_boxplot() +
    #   theme_minimal() +
    #   labs(title = "Boxplots of Median Difference Score Across Races",
    #        x = "Race",
    #        y = "Median Difference Score") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))


  })


  output$meta_wage_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         wages_vector,
         main = "Exit Wages Across Days in Program",
         ylab = "Exit Wages ($ per hour)",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 3)
  })

  output$meta_wage_plot2 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    plot(as.numeric(data$Enroll_Length), wages_vector,
         col = "steelblue",
         main = "Exit Wages Across Quarters Enrolled",
         ylab = "Exit Wages ($ per Hour)",
         xlab = "Total Quarters Enrolled",
         pch = 8)

  })


  output$meta_wage_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # the name for the gender/sex column could be varied, so we need to
    #   account for this possibility
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    boxplot(wages_vector ~ data[[sex_col]],
            main = "Exit Wages by Gender",
            names = c("Males", "Females", "Did not identify"),
            xlab = "Gender",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")
  })


  output$meta_wage_plot5 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # the name for the gender/sex column could be varied, so we need to
    #   account for this possibility
    # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
    #                      names(data), value = TRUE, perl = TRUE)
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    boxplot(wages_vector ~ data[[severity_col]],
            main = "Exit Wages by Disability Severity",
            names = c("Non significant", "Significant",
                      "Most significant"),
            xlab = "Disability Severity",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

  })


  output$meta_wage_plot6 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
    #                      names(data), value = TRUE, perl = TRUE)

    boxplot(wages_vector ~ Primary_Impairment_Group,
            data = data,
            main = "Exit Wages by Primary Impairment Type",
            xlab = "Primary Impairment",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

  })


  output$meta_wage_plot4 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    data_subset <- data[, .SD, .SDcols = c(wage_col,
                                           race_cols)]

    # Create a long-format data.table
    long_data <- melt(data_subset,
                      id.vars = wage_col,
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")
    # Filter rows where Has_Race is 1
    filtered_data <- long_data[Has_Race == 1]


    # Ensure wage_col has only one column name for the boxplot
    if (length(wage_col) != 1) {
      stop("wage_col should contain exactly one column name.")
    }

    # Extract the wage column name
    wage_col_name <- wage_col[1]

    # Create a formula for the boxplot
    boxplot_formula <- as.formula(paste(wage_col_name, "~ Race"))

    # Plot using the dynamic formula
    par(oma = c(0, 0, 0, 0) + 0.6)
    boxplot(boxplot_formula, data = filtered_data,
            col = "steelblue",
            xaxt = "n",
            yaxt = "n",
            xlab = "",
            ylab = "Exit Wages ($ per Hour)",
            main = "Exit Wages Across Race"
    )
    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    text(x = 1:length(race_cols),
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),
         xpd = NA,
         ## Rotate the labels by 45 degrees.
         srt = 45,
         cex = .8,
         adj = 1)

  })


  output$meta_employ_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Median_Time_Passed_Days,
         as.character(data$Final_Employment),
         main = "Exit Employment Across Time in Program",
         ylab = "Exit Employment",
         xlab = "Median Days Spent in Programs (per individual)",
         col = "steelblue",
         pch = 8)

  })

  output$meta_employ_plot2 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    plot(data$Enroll_Length,
         as.character(data$Final_Employment),
         main = "Exit Employment Across Enrollment Length",
         ylab = "Exit Employment",
         xlab = "Enrollment Length (total quarters)",
         col = "steelblue",
         pch = 8)

  })

  output$meta_employ_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # the name for the gender/sex column could be varied, so we need to
    #   account for this possibility
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)


    # Create a contingency table of Final_Employment by Gender
    employment_gender_table <- table(data$Final_Employment,
                                     data[[sex_col]])

    rownames(employment_gender_table) <- c("Non-competitive Employment",
                                           "Competitive Employment")

    colnames(employment_gender_table) <- c("Female",
                                           "Male",
                                           "Did not identify")


    # Create a bar plot with bars broken up by gender
    barplot(employment_gender_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topleft", bty = "n",
                               title = "Employment Type"),
            xlab = "Gender", ylab = "Count",
            main = "Exit Employment by Gender")

  })

  output$meta_employ_plot4 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                      names(data),
                      value = TRUE, perl = TRUE)

    data_subset <- data[, .SD, .SDcols = c("Final_Employment",
                                           race_cols)]

    # Create a long-format data.table
    long_data <- melt(data_subset,
                      id.vars = "Final_Employment",
                      measure.vars = race_cols,
                      variable.name = "Race",
                      value.name = "Has_Race")
    # Filter rows where Has_Race is 1
    filtered_data <- long_data[Has_Race == 1]


    # Create a contingency table of Final_Employment by Gender
    employment_race_table <- table(filtered_data$Final_Employment,
                                   filtered_data$Race)

    # Create the bar plot based on the contingency table
    par(oma = c(0, 0, 0, 0) + 0.6)
    # barplot(employment_race_table, beside = TRUE,
    #         col = c("lightsteelblue", "steelblue"),
    #         legend.text = c("Non-competitive", "Competitive"),
    #         args.legend = list(x = "topleft", bty = "n",
    #                            title = "Employment Type"),
    #         ylab = "Count",
    #         xaxt = "n",
    #         yaxt = "n",
    #         xlab = "",
    #         main = "Final Employment by Race", las = 2)

    bar_midpoints <- barplot(employment_race_table, beside = TRUE,
                             col = c("lightsteelblue", "steelblue"),
                             legend.text = c("Non-competitive", "Competitive"),
                             args.legend = list(x = "topleft", bty = "n",
                                                title = "Employment Type"),
                             ylab = "Count",
                             xaxt = "n",   # Disable default x-axis labels
                             yaxt = "n",   # Disable default y-axis labels
                             xlab = "",
                             main = "Final Employment by Race", las = 2)


    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    # Add custom x-axis labels at the midpoints of the bars
    text(x = colMeans(bar_midpoints),  # Calculate the midpoints for grouped bars
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),  # Clean the race names
         xpd = NA,  # Allow plotting outside plot region
         ## Rotate the labels by 45 degrees.
         srt = 45,
         cex = .8,
         adj = 1)

  })



  output$meta_employ_plot5 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    # Create a contingency table of Final_Employment by Gender
    employment_severity_table <- table(data$Final_Employment,
                                       data[[severity_col]])

    rownames(employment_severity_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")

    colnames(employment_severity_table) <- c("Non-significant",
                                             "Significant",
                                             "Most significant")


    # Create a bar plot with bars broken up by gender
    barplot(employment_severity_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topleft", bty = "n",
                               title = "Employment Type"),
            xlab = "Disability Severity", ylab = "Count",
            main = "Exit Employment by Disability Severity")

  })


  output$meta_employ_plot6 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
    #                      names(data), value = TRUE, perl = TRUE)

    # Create a contingency table of Final_Employment by Gender
    # employment_prim_dis_table <- table(data$Final_Employment,
    #                                    data[[prim_dis_col]])

    employment_prim_dis_table <- table(data$Final_Employment,
                                       data$Primary_Impairment_Group)

    rownames(employment_prim_dis_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")


    # Create a bar plot with bars broken up by gender
    barplot(employment_prim_dis_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topleft", bty = "n",
                               title = "Employment Type"),
            xlab = "Primary Impairment", ylab = "Count",
            main = "Exit Employment by Primary Impairment",
    )

    # # Add custom x-axis labels at the midpoints of the bars
    # text(x = colMeans(bar_midpoints),  # Calculate the midpoints for grouped bars
    #      y = par("usr")[3] - 0.45,
    #      labels = levels(metadata$Primary_Impairment_Group),  # Clean the race names
    #      xpd = NA,  # Allow plotting outside plot region
    #      ## Rotate the labels by 45 degrees.
    #      srt = 45,
    #      cex = .8,
    #      adj = 1)

  })




  ##################
  # MODELS SIDEBAR #
  ##################

  output$models_sidebar <- renderUI({
    # output$models_ui <- renderUI({
    data <- selected_data()
    data_choice <- input$data_choice
    dataset_type <- input$dataset_type

    if ((data_choice == "Use Cleaned RSA-911 Data") ||
        (data_choice == "Upload New Dataset" && dataset_type == "rsa")) {
      # sidebarPanel(
      fluidRow(
        column(12,
               h4("RSA-911 Data Modeling Options")),

        selectInput("response", "Select Response Variable",
                    choices = c(" ",
                                "Predict Employment Outcome",
                                "Predict Ending Wage")
        ),
        column(12,
               tags$label("Select Predictor Variables:")),

        column(12,
               checkboxInput("gender",
                             "Gender",
                             value = FALSE)),
        column(12,
               checkboxInput("race",
                             "Race",
                             value = FALSE)),
        column(12,
               checkboxInput("severity",
                             "Severity",
                             value = FALSE)),
        column(12,
               checkboxInput("prim_impairment",
                             "Primary Impairment",
                             value = FALSE)),
        column(12,
               checkboxInput("second_impairment",
                             "Secondary Impairment",
                             value = FALSE))
        # column(12,
        #        checkboxInput("interactions",
        #                      "Interaction Effects",
        #                      value = FALSE))

      )
      # )

    } else if ((data_choice == "Use Cleaned Scores Data") ||
               (data_choice == "Upload New Dataset" &&
                dataset_type == "scores")) {

      fluidRow(
        column(12,
               h4("Scores Data Modeling Options")),
        selectInput("anova", "Select ANOVA Test",
                    choices = c(" ",
                                "ANOVA across Services",
                                "ANOVA across Providers")
        )
      )
    } else if ((data_choice == "Use Cleaned Merged Data") ||
               (data_choice == "Upload New Dataset" &&
                dataset_type == "merged")) {
      sidebarPanel(
        fluidRow(
          column(12,
                 h4("Merged Data Modeling Options"))
        )
      )
    }  else if ((data_choice == "Use Generated Metadata") ||
                (data_choice == "Upload New Dataset" &&
                 dataset_type == "metadata")) {
      fluidRow(
        column(12,
               h4("Metadata Modeling Options")),

        selectInput("response", "Select Response Variable",
                    choices = c(" ",
                                "Predict Median Difference Score",
                                "Predict Ending Wage",
                                "Predict Employment Outcome")
        ),
        column(12,
               tags$label("Select Predictor Variables:")),

        column(12,
               checkboxInput("gender",
                             "Gender",
                             value = FALSE)),
        column(12,
               checkboxInput("race",
                             "Race",
                             value = FALSE)),
        column(12,
               checkboxInput("severity",
                             "Severity",
                             value = FALSE)),
        column(12,
               checkboxInput("enroll_length",
                             "Enrollment Length",
                             value = FALSE)),
        column(12,
               checkboxInput("prim_impairment",
                             "Primary Impairment",
                             value = FALSE)),
        column(12,
               checkboxInput("second_impairment",
                             "Secondary Impairment",
                             value = FALSE))
        # column(12,
        #        checkboxInput("interactions",
        #                      "Interaction Effects",
        #                      value = FALSE))

      )
    }
  })


  #############
  # RSA MODEL #
  #############

  model_rsa <- reactive({
    req(selected_data())
    data <- selected_data()

    response <- input$response
    if (response == "Predict Employment Outcome"){
      # employ_col <- grep("(?i)^(?=.*employment)(?!.*(?i)_desc)(?!.*(?i)_wage)(?!.*(?i)un)",
      #                    names(data), value = TRUE, perl = TRUE)
      # employ_col <- "E389_Q4_Employment_911"
      exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                            names(data), value = TRUE, perl = TRUE)

      # employ_col <- grep()
      if (length(exit_work_col) < 1){
        return("No employment variable available.")
      } else{
        y <- exit_work_col
      }


    } else if (response == "Predict Ending Wage") {
      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)

      if (length(wage_col) < 1){
        return("No employment variable available.")
      } else{
        y <- wage_col
      }
    }

    predictors <- c()

    if (input$gender) {
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)
      if (length(sex_col) < 1){
        return("No gender/sex variable available.")
      } else{
        predictors <- c(predictors, sex_col)
      }
    }

    if (input$race) {
      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)
      if (length(race_cols) < 1){
        return("No race variable(s) available.")
      } else{
        predictors <- c(predictors, race_cols)
      }
    }

    if (input$severity) {
      # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
      #                      names(data), value = TRUE, perl = TRUE)
      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)
      if (length(severity_col) < 1){
        return("No disability severity variable available.")
      } else{
        predictors <- c(predictors, severity_col)
      }
    }

    # if (input$enroll_length) {
    #   enroll_length_col <- grep("Enroll_Length",
    #                        names(data), value = TRUE, perl = TRUE)
    #   if (length(enroll_length_col) < 1){
    #     return("No enrollment length variable available.")
    #   } else{
    #     predictors <- c(predictors, enroll_length_col)
    #   }
    # }

    if (input$prim_impairment) {
      # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
      #                      names(data), value = TRUE, perl = TRUE)
      if (length("Primary_Impairment_Group") < 1){
        return("No primary impairment variable available.")
      } else{
        predictors <- c(predictors, "Primary_Impairment_Group")
      }
    }

    if (input$second_impairment) {
      # second_dis_col <- grep("(?i)^(?=.*sec)(?=.*impairment)(?!.*(desc))",
      #                        names(data), value = TRUE, perl = TRUE)
      if (length("Secondary_Impairment_Group") < 1){
        return("No secondary impairment variable available.")
      } else{
        predictors <- c(predictors, "Secondary_Impairment_Group")
      }
    }

    # Check if response and predictors are selected
    req(response, length(predictors) > 0)

    formula <- as.formula(paste(y, "~",
                                paste(predictors, collapse = "+")))

    # # Create the formula with interaction terms if specified
    # if (input$interactions) {
    #   interaction_terms <- paste(predictors, collapse = "*")
    #   formula <- as.formula(paste(y, "~", interaction_terms))
    # } else {
    #   formula <- as.formula(paste(y, "~", paste(predictors, collapse = "+")))
    # }

    if (response == "Predict Ending Wage") {
      lm(formula = formula, data = data)
    } else if (response == "Predict Employment Outcome") {
      glm(formula, family = binomial, data = data)
    }

  })

  # Render model summaries or ANOVA results
  output$model_rsa_summary <- renderPrint({
    req(model_rsa())
    summary(model_rsa())
  })



  ################
  # SCORES MODEL #
  ################

  # Reactive value to track if the ANOVA test has been run
  anova_run <- reactiveVal(FALSE)

  model_scores <- reactive({
    req(selected_data())
    req(input$anova)
    data <- selected_data()
    anova_test <- input$anova

    if (anova_test == "ANOVA across Services") {
      difference_cols <- grep("(?i)difference", names(data),
                              value = TRUE, perl = TRUE)

      # Exclude columns that contain "med" or "avail"
      exclude_patterns <- "(?i)med|avail"
      filtered_columns <- difference_cols[!grepl(exclude_patterns,
                                                 difference_cols, perl = TRUE)]

      differences <- data[, .SD, .SDcols = filtered_columns]

      # Convert data to long format for anova test to work
      long_data <- data.frame(
        Difference_Scores = c(data$Difference_CPSO, data$Difference_CSS,
                              data$Difference_EMP, data$Difference_FL,
                              data$Difference_ILOM, data$Difference_ISA,
                              data$Difference_JOBEX, data$Difference_JS,
                              data$Difference_QWEX, data$Difference_WBLE),
        Services = factor(rep(filtered_columns, each = nrow(data)))
      )


      # Perform ANOVA
      # result <- aov(mean_value ~ service, data = means_df)
      result <- aov(Difference_Scores ~ Services, data = na.omit(long_data))
      # pairwise comparisons
      tukey_result <- TukeyHSD(result)


    } else if (anova_test == "ANOVA across Providers") {

      result <- aov(Median_Difference_Score ~ Provider,
                    data = na.omit(data[, c("Median_Difference_Score",
                                            "Provider")]))

      # pairwise comparisons
      tukey_result <- TukeyHSD(result)


    } else {
      return(NULL)  # Return NULL if no valid test is selected
    }

    # Set anova_run to TRUE
    anova_run(TRUE)

    list(anova = result, tukey = tukey_result)

  })

  # observeEvent(input$anova, {
  #   anova_run(TRUE)
  # })

  output$model_scores_summary <- renderPrint({
    req(model_scores())
    summary(model_scores()$anova)
  })

  # Render Tukey HSD pairwise comparison results (significant pairs only)
  output$tukey_scores_summary <- renderPrint({
    req(model_scores())
    tukey_result <- model_scores()$tukey

    # Get the Tukey HSD results as a data frame
    tukey_df <- as.data.frame(tukey_result$Services)
    # Or use $Provider for "ANOVA across Providers"

    # Filter only significant pairs (p-value < 0.05)
    significant_pairs <- tukey_df[tukey_df$`p adj` < 0.05, ]

    # If there are no significant pairs, print a message
    if (nrow(significant_pairs) == 0) {
      cat("No significant pairwise comparisons found. May be due to sampling sizes.")
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

  model_metadata <- reactive({
    req(selected_data())
    data <- selected_data()

    response <- input$response
    if (response == "Predict Employment Outcome"){
      # employ_col <- grep("(?i)^(?=.*employment)(?!.*(?i)_desc)(?!.*(?i)_wage)(?!.*(?i)un)",
      #                    names(data), value = TRUE, perl = TRUE)
      # employ_col <- "E389_Q4_Employment_911"

      exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                            names(data), value = TRUE, perl = TRUE)

      if (length(exit_work_col) < 1){
        return("No employment variable available.")
      } else{
        y <- exit_work_col
      }


    } else if (response == "Predict Ending Wage") {
      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)

      if (length(wage_col) < 1){
        return("No wage variable available.")
      } else{
        y <- wage_col
      }
    } else if (response == "Predict Median Difference Score"){
      median_diff_col <- "Median_Difference_Score"

      if (length(median_diff_col) < 1){
        return("No median difference score variable available.")
      } else{
        y <- median_diff_col
      }
    }

    predictors <- c()

    if (input$gender) {
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)
      if (length(sex_col) < 1){
        return("No gender/sex variable available.")
      } else{
        predictors <- c(predictors, sex_col)
      }
    }

    if (input$race) {
      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)", names(data),
                        value = TRUE, perl = TRUE)
      if (length(race_cols) < 1){
        return("No race variable(s) available.")
      } else{
        predictors <- c(predictors, race_cols)
      }
    }

    if (input$severity) {
      # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
      #                      names(data), value = TRUE, perl = TRUE)
      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)
      if (length(severity_col) < 1){
        return("No disability severity variable available.")
      } else{
        predictors <- c(predictors, severity_col)
      }
    }

    if (input$enroll_length) {
      enroll_length_col <- grep("Enroll_Length",
                                names(data), value = TRUE, perl = TRUE)
      if (length(enroll_length_col) < 1){
        return("No enrollment length variable available.")
      } else{
        predictors <- c(predictors, enroll_length_col)
      }
    }

    if (input$prim_impairment) {
      # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
      #                      names(data), value = TRUE, perl = TRUE)
      if (length("Primary_Impairment_Group") < 1){
        return("No primary impairment variable available.")
      } else{
        predictors <- c(predictors, "Primary_Impairment_Group")
      }
    }

    if (input$second_impairment) {
      # second_dis_col <- grep("(?i)^(?=.*sec)(?=.*impairment)(?!.*(desc))",
      #                        names(data), value = TRUE, perl = TRUE)
      if (length("Secondary_Impairment_Group") < 1){
        return("No secondary impairment variable available.")
      } else{
        predictors <- c(predictors, "Secondary_Impairment_Group")
      }
    }

    # Check if response and predictors are selected
    req(response, length(predictors) > 0)

    formula <- as.formula(paste(y, "~",
                                paste(predictors, collapse = "+")))

    # # Create the formula with interaction terms if specified
    # if (input$interactions) {
    #   interaction_terms <- paste(predictors, collapse = "*")
    #   formula <- as.formula(paste(y, "~", interaction_terms))
    # } else {
    #   formula <- as.formula(paste(y, "~", paste(predictors, collapse = "+")))
    # }

    if (response == "Predict Ending Wage" ||
        response == "Predict Median Difference Score") {
      lm(formula = formula, data = data)
    } else if (response == "Predict Employment Outcome") {
      glm(formula, family = binomial, data = data)
    }

  })

  # Reactive function to create residual plots
  output$metadata_residuals1 <- renderPlot({
    req(model_metadata())

    # residuals
    model <- model_metadata()
    residuals <- resid(model)

    # histogram to look for normality
    hist(residuals, col = "steelblue")

  })

  output$metadata_residuals2 <- renderPlot({
    req(model_metadata())

    # residuals
    model <- model_metadata()
    residuals <- resid(model)

    # histogram to look for normality
    qqnorm(residuals)
    qqline(residuals, col = "steelblue")

  })

  output$metadata_residuals3 <- renderPlot({
    req(model_metadata())

    # Extract fitted values and residuals
    model <- model_metadata()
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

  output$model_metadata_summary <- renderPrint({
    req(model_metadata())
    summary(model_metadata())
  })



  ###################
  # MODELS MAIN TAB #
  ###################

  # Render the model summary based on dataset type
  output$models_main <- renderUI({
    data_choice <- input$data_choice

    if (data_choice == "Use Cleaned RSA-911 Data" ||
        (data_choice == "Upload New Dataset" && input$dataset_type == "rsa")) {
      fluidRow(
        column(12, verbatimTextOutput("model_rsa_summary"))
      )
    } else if (data_choice == "Use Cleaned Scores Data" ||
               (data_choice == "Upload New Dataset" &&
                input$dataset_type == "scores")) {
      # fluidRow(
      #   # column(12, verbatimTextOutput("model_scores_summary")),
      #   # column(12, verbatimTextOutput("tukey_scores_summary")),
      #   # column(12, verbatimTextOutput("model_scores_summary")),
      #
      #   # Conditionally show the caption and Tukey results
      #   if (anova_run()) {
      #     tagList(
      #       # anova output
      #       tags$p("ANOVA results",
      #              style = "font-size: 14px; font-weight: bold;"),
      #       column(12, verbatimTextOutput("model_scores_summary")),
      #       # pairwise results
      #       tags$p("Significant pairwise comparisons:",
      #              style = "font-size: 14px; font-weight: bold;"),
      #       column(12, verbatimTextOutput("tukey_scores_summary"))
      #
      #     )
      #   },
      #   column(12, plotOutput("scores_residuals1")),
      #   column(12, plotOutput("scores_residuals2")),
      #   column(12, plotOutput("scores_residuals3"))
      # )
      #
      fluidRow(
        # Conditionally show the caption and ANOVA results
        if (anova_run()) {
          tagList(
            fluidRow(
              column(12, tags$p("ANOVA results",
                                style = "font-size: 14px; font-weight: bold;")),
              column(12, verbatimTextOutput("model_scores_summary"))
            ),
            fluidRow(
              column(12, tags$p("Significant pairwise comparisons:",
                                style = "font-size: 14px; font-weight: bold;")),
              column(12, verbatimTextOutput("tukey_scores_summary"))
            )
          )
        },
        fluidRow(
          column(12, plotOutput("scores_residuals1")),
          column(12, plotOutput("scores_residuals2")),
          column(12, plotOutput("scores_residuals3"))
        )
      )

    } else if ((data_choice == "Use Generated Metadata") ||
               (data_choice == "Upload New Dataset" &&
                dataset_type == "metadata")) {
      fluidRow(
        column(12, verbatimTextOutput("model_metadata_summary")),
        column(12, plotOutput("metadata_residuals1")),
        column(12, plotOutput("metadata_residuals2")),
        column(12, plotOutput("metadata_residuals3"))
      )
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)

