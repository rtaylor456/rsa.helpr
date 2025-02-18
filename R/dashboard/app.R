library(shiny)
library(data.table)
library(readxl)

if (!requireNamespace("rsa.helpr", quietly = TRUE)) {
  # devtools::install_github("rtaylor456/rsa.helpr")
  remotes::install_github("rtaylor456/rsa.helpr")
}
library(rsa.helpr)

# options(shiny.maxRequestSize = 1000 * 1024^2)  # 500MB
options(shiny.maxRequestSize = 1.5 * 1024^3)  # 1.5GB

# Define UI
ui <- fluidPage(

  # to change location of progress bars:
  tags$head(tags$style(".shiny-notification {position: fixed; top: 50% ;left: 50%")),

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
                           # column(12,
                           #        checkboxInput("convert_sex", "Convert Sex",
                           #                      value = TRUE)),
                           # column(12,
                           #        checkboxInput("convert_employ",
                           #                      "Convert Employment",
                           #                      value = TRUE)),
                           # column(12,
                           #        checkboxInput("clean_specials",
                           #                      "Clean Specials",
                           #                      value = FALSE)),


                           column(12,
                                  checkboxInput("remove_desc",
                                                "Remove Description Columns",
                                                value = TRUE)),
                           column(12,
                                  checkboxInput("remove_strictly_na",
                                                "Remove Strictly NA Columns",
                                                value = TRUE)),

                           column(12,
                                  textInput("clean_specials",
                                            "Clean Specials (Enter variable names separated by commas)",
                                            placeholder = "e.g., var1, var2, var3")),

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
                                  checkboxInput("clean_ID",
                                                "Remove rows with missing IDs",
                                                value = TRUE)),


                           column(12,
                                  textInput("state_filter",
                                            "State(s) of interest (If multiple states, enter names separated by commas)",
                                            placeholder = "e.g., Utah. e.g., Utah, Colorado, Idaho")),

                           column(12,
                                  textInput("ID_col",
                                            "If the variable naming structure for participant ID, please enter here",
                                            placeholder = "e.g., X")),


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
                                    # checkboxInput("use_merged",
                                    #               "(Use Merged Data)",
                                    #               value = TRUE)
                                    radioButtons("meta_source",
                                                 label = NULL,
                                                 choices = list("Using Merged Data" = "merged",
                                                                "Using Cleaned RSA-911 Data" = "rsa"),
                                                 selected = "merged")
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

                       # mainPanel(
                       #   h3("Quarterly Data"),
                       #   DTOutput("table_rsa"),
                       #   verbatimTextOutput("summary_rsa"),
                       #   br(),
                       #
                       #   h3("Scores Data"),
                       #   DTOutput("table_scores"),
                       #   verbatimTextOutput("summary_scores"),
                       #   br(),
                       #
                       #   h3("Merged Quarterly and Scores Data"),
                       #   DTOutput("table_merged"),
                       #   verbatimTextOutput("summary_merged"),
                       #   br(),
                       #
                       #   h3("Generated Metadata"),
                       #   DTOutput("table_metadata"),
                       #   verbatimTextOutput("summary_metadata")
                       # )

                       mainPanel(
                         # Quarterly Data
                         conditionalPanel(
                           condition = "output.rsa_data_exists == true",
                           h3("Quarterly Data"),
                           DTOutput("table_rsa"),
                           verbatimTextOutput("summary_rsa"),
                           br()
                         ),

                         # Scores Data
                         conditionalPanel(
                           condition = "output.scores_data_exists == true",
                           h3("Scores Data"),
                           DTOutput("table_scores"),
                           verbatimTextOutput("summary_scores"),
                           br()
                         ),

                         # Merged Quarterly and Scores Data
                         conditionalPanel(
                           condition = "output.merged_data_exists == true",
                           h3("Merged Quarterly and Scores Data"),
                           DTOutput("table_merged"),
                           verbatimTextOutput("summary_merged"),
                           br()
                         ),

                         # Generated Metadata
                         conditionalPanel(
                           condition = "output.metadata_exists == true",
                           h3("Generated Metadata"),
                           DTOutput("table_metadata"),
                           verbatimTextOutput("summary_metadata")
                         )
                       )
              ),
              tabPanel('Select Data (or load new)',
                       sidebarPanel(
                         selectInput("data_choice", "Select Data Source",
                                     choices = c(" ",
                                                 "Use Generated Metadata",
                                                 "Use Cleaned Scores Data",
                                                 # "Use Cleaned RSA-911 Data",
                                                 # "Use Cleaned Merged Data",
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
      # cleaned_scores <- clean_scores(df_scores_combined,
      #                                aggregate = input$aggregate_scores)

      cleaned_scores <- clean_scores(data = df_scores_combined,
                                     state_filter = states_of_interest,
                                     clean_ID = input$clean_ID,
                                     aggregate = input$aggregate_scores,
                                     ID_col = id_col)


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
    req(input$generate_metadata)  # Ensure the button was clicked
    req(input$meta_source)         # Ensure a source is selected

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
        tabPanel("Overview",
                 plotOutput("overview_plot1"),
                 downloadButton("download_overview_plot1", "Download Plot"),

                 plotOutput("overview_plot2"),
                 downloadButton("download_overview_plot2", "Download Plot"),

                 plotOutput("overview_plot3"),
                 downloadButton("download_overview_plot3", "Download Plot")),

        tabPanel("Across Services",
                 plotOutput("services_plot1"),
                 downloadButton("download_services_plot1", "Download Plot"),

                 plotOutput("services_plot2"),
                 downloadButton("download_services_plot2", "Download Plot"),

                 plotOutput("services_plot3"),
                 downloadButton("download_services_plot3", "Download Plot")),

        tabPanel("Across Providers",
                 plotOutput("providers_plot1"),
                 downloadButton("download_providers_plot1", "Download Plot")
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
                 downloadButton("download_meta_gen_demo_plot1", "Download Plot"),

                 plotOutput("meta_gen_demo_plot2"),
                 downloadButton("download_meta_gen_demo_plot2", "Download Plot"),

                 plotOutput("meta_gen_demo_plot3"),
                 downloadButton("download_meta_gen_demo_plot3", "Download Plot"),

                 plotOutput("meta_gen_demo_plot4"),
                 downloadButton("download_meta_gen_demo_plot4", "Download Plot"),

                 plotOutput("meta_gen_demo_plot5"),
                 downloadButton("download_meta_gen_demo_plot5", "Download Plot"),

                 plotOutput("meta_gen_demo_plot6"),
                 downloadButton("download_meta_gen_demo_plot6", "Download Plot"),

                 plotOutput("meta_gen_demo_plot7"),
                 downloadButton("download_meta_gen_demo_plot7", "Download Plot"),
                 ),

        tabPanel("Investigate Difference Scores",
                 plotOutput("meta_diff_plot1"),
                 downloadButton("download_meta_diff_plot1", "Download Plot"),

                 plotOutput("meta_diff_plot2"),
                 downloadButton("download_meta_diff_plot2", "Download Plot"),

                 plotOutput("meta_diff_plot3"),
                 downloadButton("download_meta_diff_plot3", "Download Plot"),

                 plotOutput("meta_diff_plot4"),
                 downloadButton("download_meta_diff_plot4", "Download Plot"),

                 plotOutput("meta_diff_plot5"),
                 downloadButton("download_meta_diff_plot5", "Download Plot"),

                 plotOutput("meta_diff_plot6"),
                 downloadButton("download_meta_diff_plot6", "Download Plot"),

                 plotOutput("meta_diff_plot7"),
                 downloadButton("download_meta_diff_plot7", "Download Plot")
                 ),
        tabPanel("Investigate Wage",
                 plotOutput("meta_wage_plot1"),
                 downloadButton("download_meta_wage_plot1", "Download Plot"),

                 plotOutput("meta_wage_plot2"),
                 downloadButton("download_meta_wage_plot2", "Download Plot"),

                 plotOutput("meta_wage_plot3"),
                 downloadButton("download_meta_wage_plot3", "Download Plot"),

                 plotOutput("meta_wage_plot4"),
                 downloadButton("download_meta_wage_plot4", "Download Plot"),

                 plotOutput("meta_wage_plot5"),
                 downloadButton("download_meta_wage_plot5", "Download Plot"),

                 plotOutput("meta_wage_plot6"),
                 downloadButton("download_meta_wage_plot6", "Download Plot"),

                 plotOutput("meta_wage_plot7"),
                 downloadButton("download_meta_wage_plot7", "Download Plot")
                 ),
        tabPanel("Investigate Employment",
                 plotOutput("meta_employ_plot1"),
                 downloadButton("download_meta_employ_plot1", "Download Plot"),

                 plotOutput("meta_employ_plot2"),
                 downloadButton("download_meta_employ_plot2", "Download Plot"),

                 plotOutput("meta_employ_plot3"),
                 downloadButton("download_meta_employ_plot3", "Download Plot"),

                 plotOutput("meta_employ_plot4"),
                 downloadButton("download_meta_employ_plot4", "Download Plot"),

                 plotOutput("meta_employ_plot5"),
                 downloadButton("download_meta_employ_plot5", "Download Plot"),

                 plotOutput("meta_employ_plot6"),
                 downloadButton("download_meta_employ_plot6", "Download Plot"),

                 plotOutput("meta_employ_plot7"),
                 downloadButton("download_meta_employ_plot7", "Download Plot")
                 )
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
  output$overview_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Median_Difference_Score,
         col = "lightsteelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")

  })

  # Download handler for overview_plot1
  output$download_overview_plot1 <- downloadHandler(
    filename = function() { "median_difference_scores.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Median_Difference_Score,
           col = "lightsteelblue",
           main = "Distribution of Median Difference Scores",
           xlab = "Median Difference Scores")
      dev.off()
    }
  )

  output$overview_plot2 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Median_Time_Passed_Days,
         col = "lightsteelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

  })

  # Download handler for overview_plot2
  output$download_overview_plot2 <- downloadHandler(
    filename = function() { "median_time_passed.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Median_Time_Passed_Days,
           col = "lightsteelblue",
           main = "Distribution of Time in Programs",
           xlab = "Median Time in Program (per individual)")
      dev.off()
    }
  )

  output$overview_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Differences_Available,
         col = "lightsteelblue",
         main = "Distribution of Counts of Difference Scores",
         xlab = "Number of Program Difference Scores per Individual")

  })

  # Download handler for overview_plot3
  output$download_overview_plot3 <- downloadHandler(
    filename = function() { "differences_available.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Differences_Available,
           col = "lightsteelblue",
           main = "Distribution of Counts of Difference Scores",
           xlab = "Number of Program Difference Scores per Individual")
      dev.off()
    }
  )


  ## Across Services
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

  # For services_plot1
  output$download_services_plot1 <- downloadHandler(
    filename = function() {
      paste("services_plot1_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      difference_cols <- grep("(?i)difference", names(selected_data()),
                              value = TRUE, perl = TRUE)
      exclude_patterns <- "(?i)med|avail"
      filtered_columns <- difference_cols[!grepl(exclude_patterns,
                                                 difference_cols, perl = TRUE)]
      differences <- selected_data()[, .SD, .SDcols = filtered_columns]
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
      dev.off()
    }
  )


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


  # For services_plot2
  output$download_services_plot2 <- downloadHandler(
    filename = function() {
      paste("services_plot2_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      pre_cols <- grep("(?i)pre", names(selected_data()), value = TRUE,
                       perl = TRUE)
      pre_scores <- selected_data()[, .SD, .SDcols = pre_cols]
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
      dev.off()
    }
  )


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

  # For services_plot3
  output$download_services_plot3 <- downloadHandler(
    filename = function() {
      paste("services_plot3_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      post_cols <- grep("(?i)post", names(selected_data()), value = TRUE,
                        perl = TRUE)
      post_scores <- selected_data()[, .SD, .SDcols = post_cols]
      post_scores_vector <- as.vector(unlist(post_scores))
      post_scores_median <- median(post_scores_vector, na.rm = TRUE)

      pre_cols <- grep("(?i)pre", names(selected_data()), value = TRUE,
                       perl = TRUE)
      pre_scores <- selected_data()[, .SD, .SDcols = pre_cols]
      pre_scores_vector <- as.vector(unlist(pre_scores))
      pre_scores_median <- median(pre_scores_vector, na.rm = TRUE)

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
      dev.off()
    }
  )



  ## Across Providers
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


  # For providers_plot1
  output$download_providers_plot1 <- downloadHandler(
    filename = function() {
      paste("providers_plot1_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      data <- selected_data()
      median_diff_col <- "Median_Difference_Score"
      median_diff_scores <- data[, .SD, .SDcols = median_diff_col]
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
      dev.off()
    }
  )


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
  # Wage
  output$meta_gen_demo_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Median_Time_Passed_Days,
         col = "steelblue",
         main = "Distribution of Time in Programs",
         xlab = "Median Time in Program (per individual)")

    })

  # Download handler for Time in Programs Plot
  output$download_meta_gen_demo_plot1 <- downloadHandler(
    filename = function() { "time_in_programs_plot.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Median_Time_Passed_Days,
           col = "steelblue",
           main = "Distribution of Time in Programs",
           xlab = "Median Time in Program (per individual)")
      dev.off()
    }
  )

  # Enrollment length
  output$meta_gen_demo_plot2 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Enroll_Length,
         col = "steelblue",
         main = "Distribution of Enrollment Lengths",
         xlab = "Enrollment Length (Quarters)")

  })

  # Download handler for Enrollment Length Plot
  output$download_meta_gen_demo_plot2 <- downloadHandler(
    filename = function() { "enrollment_length_plot.png" },
    content = function(file) {
      png(file)
      hist(selected_data()$Enroll_Length,
           col = "steelblue",
           main = "Distribution of Enrollment Lengths",
           xlab = "Enrollment Length (Quarters)")
      dev.off()
    }
  )



  # Gender
  # output$meta_gen_demo_plot3 <- renderPlot({
  #   req(selected_data())
  #   data <- selected_data()
  #
  #   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                   value = TRUE, perl = TRUE)
  #
  #   barplot(table(data[[sex_col]]),
  #           main = "Distribution of Genders",
  #           xlab = "Gender",
  #           names = c("Females", "Males", "Other", "Did not identify"),
  #           col = c("lightsteelblue", "steelblue", "darkblue", "gray"))
  #
  #   })

  output$meta_gen_demo_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # Identify the column for gender/sex dynamically
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Extract the gender column
    gender_data <- data[[sex_col]]

    # Replace numeric values with labels
    labeled_gender_data <- factor(gender_data, levels = names(gender_labels),
                                  labels = gender_labels)

    # Create the barplot
    barplot(table(labeled_gender_data),
            main = "Distribution of Genders",
            xlab = "Gender",
            col = c("steelblue", "lightsteelblue", "darkblue", "gray"))
  })


  # Download handler for Gender Distribution Plot

  # output$download_meta_gen_demo_plot3 <- downloadHandler(
  #   filename = function() { "gender_distribution_plot.png" },
  #   content = function(file) {
  #     png(file)
  #     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(selected_data()),
  #                     value = TRUE, perl = TRUE)
  #
  #     barplot(table(selected_data()[[sex_col]]),
  #             main = "Distribution of Genders",
  #             xlab = "Gender",
  #             names = c("Females", "Males", "Did not identify"),
  #             col = c("lightsteelblue", "steelblue", "darkblue"))
  #     dev.off()
  #   }
  # )

  # Download handler for Gender Distribution Plot
  output$download_meta_gen_demo_plot3 <- downloadHandler(
    filename = function() { "gender_distribution_plot.png" },
    content = function(file) {
      png(file)

      # Access the selected data
      data <- selected_data()

      # Identify the column for gender/sex dynamically
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

      # Define the mapping of numeric values to labels
      gender_labels <- c(
        "1" = "Male",
        "2" = "Female",
        "3" = "Other",
        "9" = "Did not identify"
      )

      # Extract the gender column
      gender_data <- data[[sex_col]]

      # Replace numeric values with labels
      labeled_gender_data <- factor(gender_data, levels = names(gender_labels),
                                    labels = gender_labels)

      # Create the barplot
      barplot(table(labeled_gender_data),
              main = "Distribution of Genders",
              xlab = "Gender",
              col = c("steelblue", "lightsteelblue", "darkblue", "gray"))

      dev.off()
    }
  )




  # Race
  output$meta_gen_demo_plot4 <- renderPlot({
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
    race_table <- table(filtered_data$Race)

    # Create the bar plot based on the contingency table
    par(oma = c(0, 0, 0, 0) + 0.6)
    barplot_heights <- barplot(race_table, beside = TRUE,
                               ylab = "Count",
                               xaxt = "n",   # Disable default x-axis labels
                               yaxt = "n",   # Disable default y-axis labels
                               xlab = "",
                               main = "Distribution of Race", las = 2,
                               col = "steelblue")

    # Add the y-axis
    axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

    # Add diagonal labels
    text(x = barplot_heights, # Center labels based on barplot positions
         y = par("usr")[3] - 0.45,
         labels = gsub("^E[0-9]+_|_911$", "", race_cols),
         xpd = NA,
         srt = 45,  # Rotate the labels by 45 degrees
         cex = .8,
         adj = c(1, 1))  # Adjust text alignment to center under bars

    })

  # Download handler for Race Distribution Plot
  output$download_meta_gen_demo_plot4 <- downloadHandler(
    filename = function() { "race_distribution_plot.png" },
    content = function(file) {
      png(file)
      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(selected_data()),
                        value = TRUE, perl = TRUE)

      data_subset <- selected_data()[, .SD, .SDcols = c("Final_Employment", race_cols)]
      long_data <- melt(data_subset, id.vars = "Final_Employment", measure.vars = race_cols,
                        variable.name = "Race", value.name = "Has_Race")
      filtered_data <- long_data[Has_Race == 1]
      race_table <- table(filtered_data$Race)

      barplot_heights <- barplot(race_table, beside = TRUE,
                                 ylab = "Count",
                                 xaxt = "n",
                                 yaxt = "n",
                                 xlab = "",
                                 main = "Distribution of Race", las = 2,
                                 col = "steelblue")
      axis(side = 2, las = 2, mgp = c(3, 0.75, 0))
      text(x = barplot_heights, y = par("usr")[3] - 0.45,
           labels = gsub("^E[0-9]+_|_911$", "", race_cols),
           xpd = NA,
           srt = 45,
           cex = .8,
           adj = c(1, 1))
      dev.off()
    }
  )


  # Severity
  output$meta_gen_demo_plot5 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    barplot(table(data[[severity_col]]),
            main = "Distribution of Disability Severity",
            xlab = "Severity",
            names = c("Non-significant", "Significant", "Most significant"),
            col = c("lightsteelblue", "steelblue", "darkblue"))

    })

  # Download handler for Severity Distribution Plot
  output$download_meta_gen_demo_plot5 <- downloadHandler(
    filename = function() { "severity_distribution_plot.png" },
    content = function(file) {
      png(file)
      req(selected_data())
      data <- selected_data()

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      barplot(table(data[[severity_col]]),
              main = "Distribution of Disability Severity",
              xlab = "Severity",
              names = c("Non-significant", "Significant", "Most significant"),
              col = c("lightsteelblue", "steelblue", "darkblue"))
      dev.off()
    }
  )


  # Primary impairment
  output$meta_gen_demo_plot6 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    barplot(table(data$Primary_Impairment_Group),
            main = "Distribution of Primary Impairments",
            xlab = "Primary Impairment",
            col = "steelblue")

  })

  # Download handler for Primary Impairment Distribution Plot
  output$download_meta_gen_demo_plot6 <- downloadHandler(
    filename = function() { "primary_impairment_distribution_plot.png" },
    content = function(file) {
      png(file)
      req(selected_data())
      data <- selected_data()

      barplot(table(data$Primary_Impairment_Group),
              main = "Distribution of Primary Impairments",
              xlab = "Primary Impairment",
              col = "steelblue")
      dev.off()
    }
  )


  # Primary impairment
  output$meta_gen_demo_plot7 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    barplot(table(data$Secondary_Impairment_Group),
            main = "Distribution of Secondary Impairments",
            xlab = "Secondary Impairment",
            col = "steelblue")

  })

  # Download handler for Secondary Impairment Distribution Plot
  output$download_meta_gen_demo_plot7 <- downloadHandler(
    filename = function() { "secondary_impairment_distribution_plot.png" },
    content = function(file) {
      png(file)
      req(selected_data())
      data <- selected_data()

      barplot(table(data$Secondary_Impairment_Group),
              main = "Distribution of Secondary Impairments",
              xlab = "Secondary Impairment",
              col = "steelblue")
      dev.off()
    }
  )




  output$meta_diff_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Median_Difference_Score,
         col = "steelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")
  })

  output$meta_diff_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    hist(data$Median_Difference_Score,
         col = "steelblue",
         main = "Distribution of Median Difference Scores",
         xlab = "Median Difference Scores")
  })

  output$meta_diff_plot2 <- renderPlot({
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

  output$meta_diff_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # plot(as.numeric(data$Enroll_Length), data$Median_Difference_Score,
    #      col = "steelblue",
    #      main = "Difference Scores Across Quarters Enrolled",
    #      ylab = "Median Difference Score",
    #      xlab = "Total Quarters Enrolled",
    #      pch = 8)

    boxplot(Median_Difference_Score ~ data[["Enroll_Length_Grp"]], data = data,
            main = "Difference Scores Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Median Difference Scores",
            col = "steelblue")

  })


  output$meta_diff_plot2 <- renderPlot({
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

  # Download handler for meta_diff_plot2
  output$download_meta_diff_plot2 <- downloadHandler(
    filename = function() { "difference_scores_across_time_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()
      plot(data$Median_Time_Passed_Days,
           data$Median_Difference_Score,
           main = "Difference Scores Across Time in Program",
           ylab = "Median Difference Scores",
           xlab = "Median Days Spent in Programs (per individual)",
           col = "steelblue",
           pch = 3)
      dev.off()
    }
  )

  output$meta_diff_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # plot(as.numeric(data$Enroll_Length), data$Median_Difference_Score,
    #      col = "steelblue",
    #      main = "Difference Scores Across Quarters Enrolled",
    #      ylab = "Median Difference Score",
    #      xlab = "Total Quarters Enrolled",
    #      pch = 8)

    boxplot(Median_Difference_Score ~ data[["Enroll_Length_Grp"]], data = data,
            main = "Difference Scores Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Median Difference Scores",
            col = "steelblue")

    })

  # Download handler for meta_diff_plot3
  output$download_meta_diff_plot3 <- downloadHandler(
    filename = function() { "difference_scores_across_quarters_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()
      boxplot(Median_Difference_Score ~ data[["Enroll_Length_Grp"]], data = data,
              main = "Difference Scores Across Quarters Enrolled",
              xlab = "Enrollment Length (total quarters)",
              ylab = "Median Difference Scores",
              col = "steelblue")
      dev.off()
    }
  )


  # output$meta_diff_plot4 <- renderPlot({
  #   req(selected_data())
  #   data <- selected_data()
  #
  #   # the name for the gender/sex column could be varied, so we need to
  #   # account for this possibility
  #   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                   value = TRUE, perl = TRUE)
  #
  #
  #   if (length(unique(data$Median_Difference_Score[
  #     data[[sex_col]] == 0])) >= 2 &
  #     length(unique(data$Median_Difference_Score[
  #       data[[sex_col]] == 1])) >= 2 &
  #     length(unique(data$Median_Difference_Score[
  #       data[[sex_col]] == 9])) >= 2
  #   ) {
  #     # Create density for each group
  #     females_density <- density(data$Median_Difference_Score[
  #       data[[sex_col]] == 0], na.rm = TRUE)
  #     males_density <- density(data$Median_Difference_Score[
  #       data[[sex_col]] == 1], na.rm = TRUE)
  #     did_not_identify_density <- density(data$Median_Difference_Score[
  #       data[[sex_col]] == 9], na.rm = TRUE)
  #
  #     # Determine the maximum y value for setting y limits
  #     max_density_value <- max(c(max(females_density$y, na.rm = TRUE),
  #                                max(males_density$y, na.rm = TRUE),
  #                                max(did_not_identify_density$y, na.rm = TRUE)),
  #                              na.rm = TRUE)
  #
  #     # Initialize the plot with dynamic y-limits
  #     plot(females_density, col = "steelblue4", lwd = 2,
  #          main = "Density Plot of Median Difference Scores by Gender",
  #          xlab = "Median Difference Scores", ylab = "Density",
  #          xlim = range(c(females_density$x, males_density$x,
  #                         did_not_identify_density$x)),
  #          # Add a bit of padding for y-limits
  #          ylim = c(0, max_density_value * 1.1))
  #
  #     # Fill under the females density curve
  #     polygon(c(females_density$x, rev(females_density$x)),
  #             c(rep(0, length(females_density$x)), rev(females_density$y)),
  #             # Light blue fill
  #             col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
  #
  #     # Add the density lines for other groups
  #     lines(males_density, col = "darkblue", lwd = 2)
  #     lines(did_not_identify_density, col = "darkgray", lwd = 2)
  #
  #     # Fill under the males density curve
  #     polygon(c(males_density$x, rev(males_density$x)),
  #             c(rep(0, length(males_density$x)), rev(males_density$y)),
  #             # Light dark blue fill
  #             col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
  #
  #     # Fill under the did not identify density curve
  #     polygon(c(did_not_identify_density$x, rev(did_not_identify_density$x)),
  #             c(rep(0, length(did_not_identify_density$x)),
  #               rev(did_not_identify_density$y)),
  #             # Light gray fill
  #             col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
  #
  #     # Add a legend
  #     legend("topright", legend = c("Females", "Males", "Did not identify"),
  #            col = c("steelblue4", "darkblue", "gray"), lwd = 2)
  #   } else{
  #     boxplot(Median_Difference_Score ~ data[[sex_col]], data = data,
  #             main = "Difference Scores by Gender",
  #             names = c("Females", "Males", "Did not identify"),
  #             xlab = "Gender",
  #             ylab = "Median Difference Scores",
  #             col = "steelblue")
  #   }
  # })

  output$meta_diff_plot4 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # Identify the gender/sex column dynamically
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Replace numeric values with labels
    labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                  labels = gender_labels)

    # Ensure Median_Difference_Score has enough unique values for each gender
    if (all(sapply(levels(labeled_gender_data), function(g) {
      sum(data[[sex_col]] == g & !is.na(data$Median_Difference_Score)) >= 2
    }))) {
      # Compute density for each gender
      density_data <- lapply(levels(labeled_gender_data), function(g) {
        density(data$Median_Difference_Score[labeled_gender_data == g], na.rm = TRUE)
      })

      # Determine maximum y-limit for all densities
      max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

      # Plot the first density curve to set up the plot
      plot(density_data[[1]], col = "steelblue4", lwd = 2,
           main = "Density Plot of Median Difference Scores by Gender",
           xlab = "Median Difference Scores", ylab = "Density",
           xlim = range(sapply(density_data, function(d) range(d$x))),
           ylim = c(0, max_density_value * 1.1))

      # Fill under the first density curve
      polygon(c(density_data[[1]]$x, rev(density_data[[1]]$x)),
              c(rep(0, length(density_data[[1]]$x)), rev(density_data[[1]]$y)),
              col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)

      # Add the remaining density curves and their fills
      colors <- c("darkblue", "darkgray", "lightsteelblue")
      for (i in 2:length(density_data)) {
        lines(density_data[[i]], col = colors[i - 1], lwd = 2)
        polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
      }

      # Add a legend
      legend("topright", legend = levels(labeled_gender_data),
             col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
    } else {
      # Fallback: Create a boxplot if densities cannot be plotted
      boxplot(Median_Difference_Score ~ labeled_gender_data, data = data,
              main = "Difference Scores by Gender",
              xlab = "Gender",
              ylab = "Median Difference Scores",
              col = "steelblue")
    }
  })


  # Download handler for meta_diff_plot4
  # output$download_meta_diff_plot4 <- downloadHandler(
  #   filename = function() { "difference_scores_by_gender_plot.png" },
  #   content = function(file) {
  #     png(file)
  #     data <- selected_data()
  #
  #     # Generate the plot for meta_diff_plot4
  #     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                     value = TRUE, perl = TRUE)
  #
  #     if (length(unique(data$Median_Difference_Score[
  #       data[[sex_col]] == 0])) >= 2 &
  #       length(unique(data$Median_Difference_Score[
  #         data[[sex_col]] == 1])) >= 2 &
  #       length(unique(data$Median_Difference_Score[
  #         data[[sex_col]] == 9])) >= 2
  #     ) {
  #       # Create density plot
  #       females_density <- density(data$Median_Difference_Score[
  #         data[[sex_col]] == 0], na.rm = TRUE)
  #       males_density <- density(data$Median_Difference_Score[
  #         data[[sex_col]] == 1], na.rm = TRUE)
  #       did_not_identify_density <- density(data$Median_Difference_Score[
  #         data[[sex_col]] == 9], na.rm = TRUE)
  #
  #       max_density_value <- max(c(max(females_density$y, na.rm = TRUE),
  #                                  max(males_density$y, na.rm = TRUE),
  #                                  max(did_not_identify_density$y, na.rm = TRUE)),
  #                                na.rm = TRUE)
  #
  #       plot(females_density, col = "steelblue4", lwd = 2,
  #            main = "Density Plot of Median Difference Scores by Gender",
  #            xlab = "Median Difference Scores", ylab = "Density",
  #            xlim = range(c(females_density$x, males_density$x,
  #                           did_not_identify_density$x)),
  #            ylim = c(0, max_density_value * 1.1))
  #       polygon(c(females_density$x, rev(females_density$x)),
  #               c(rep(0, length(females_density$x)), rev(females_density$y)),
  #               col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
  #       lines(males_density, col = "darkblue", lwd = 2)
  #       lines(did_not_identify_density, col = "darkgray", lwd = 2)
  #       polygon(c(males_density$x, rev(males_density$x)),
  #               c(rep(0, length(males_density$x)), rev(males_density$y)),
  #               col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
  #       polygon(c(did_not_identify_density$x, rev(did_not_identify_density$x)),
  #               c(rep(0, length(did_not_identify_density$x)),
  #                 rev(did_not_identify_density$y)),
  #               col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
  #       legend("topright", legend = c("Females", "Males", "Did not identify"),
  #              col = c("steelblue4", "darkblue", "gray"), lwd = 2)
  #     } else {
  #       boxplot(Median_Difference_Score ~ data[[sex_col]], data = data,
  #               main = "Difference Scores by Gender",
  #               names = c("Females", "Males", "Did not identify"),
  #               xlab = "Gender",
  #               ylab = "Median Difference Scores",
  #               col = "steelblue")
  #     }
  #     dev.off()
  #   }
  # )

  # Download handler for meta_diff_plot4
  output$download_meta_diff_plot4 <- downloadHandler(
    filename = function() { "difference_scores_by_gender_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      # Identify the gender/sex column dynamically
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

      # Define the mapping of numeric values to labels
      gender_labels <- c(
        "1" = "Male",
        "2" = "Female",
        "3" = "Other",
        "9" = "Did not identify"
      )

      # Replace numeric values with labels
      labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                    labels = gender_labels)

      # Ensure Median_Difference_Score has enough unique values for each gender
      if (all(sapply(levels(labeled_gender_data), function(g) {
        sum(data[[sex_col]] == g & !is.na(data$Median_Difference_Score)) >= 2
      }))) {
        # Compute density for each gender
        density_data <- lapply(levels(labeled_gender_data), function(g) {
          density(data$Median_Difference_Score[labeled_gender_data == g], na.rm = TRUE)
        })

        # Determine maximum y-limit for all densities
        max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

        # Plot the first density curve to set up the plot
        plot(density_data[[1]], col = "steelblue4", lwd = 2,
             main = "Density Plot of Median Difference Scores by Gender",
             xlab = "Median Difference Scores", ylab = "Density",
             xlim = range(sapply(density_data, function(d) range(d$x))),
             ylim = c(0, max_density_value * 1.1))

        # Fill under the first density curve
        polygon(c(density_data[[1]]$x, rev(density_data[[1]]$x)),
                c(rep(0, length(density_data[[1]]$x)), rev(density_data[[1]]$y)),
                col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)

        # Add the remaining density curves and their fills
        colors <- c("darkblue", "darkgray", "lightsteelblue")
        for (i in 2:length(density_data)) {
          lines(density_data[[i]], col = colors[i - 1], lwd = 2)
          polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                  c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                  col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
        }

        # Add a legend
        legend("topright", legend = levels(labeled_gender_data), col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
      } else {
        # Fallback: Create a boxplot if densities cannot be plotted
        boxplot(Median_Difference_Score ~ labeled_gender_data, data = data,
                main = "Difference Scores by Gender",
                xlab = "Gender",
                ylab = "Median Difference Scores",
                col = "steelblue")
      }

      dev.off()
    }
  )



  output$meta_diff_plot6 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # Identify the severity column dynamically
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    if (length(unique(data$Median_Difference_Score[
      data[[severity_col]] == 0])) >= 2 &
        length(unique(data$Median_Difference_Score[
          data[[severity_col]] == 1])) >= 2 &
        length(unique(data$Median_Difference_Score[
          data[[severity_col]] == 2])) >= 2
    ) {
      # Create density for each group
      non_significant_density <- density(data$Median_Difference_Score[
        data[[severity_col]] == 0], na.rm = TRUE)
      significant_density <- density(data$Median_Difference_Score[
        data[[severity_col]] == 1], na.rm = TRUE)
      most_significant_density <- density(data$Median_Difference_Score[
        data[[severity_col]] == 2], na.rm = TRUE)

      # Determine the maximum y value for setting y limits
      max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                 max(significant_density$y, na.rm = TRUE),
                                 max(most_significant_density$y, na.rm = TRUE)),
                               na.rm = TRUE)

      # Initialize the plot with dynamic y-limits
      plot(non_significant_density, col = "steelblue4", lwd = 2,
           main = "Density Plot of Median Difference Scores by Disability Severity",
           xlab = "Median Difference Scores", ylab = "Density",
           xlim = range(c(non_significant_density$x, significant_density$x,
                          most_significant_density$x)),
           ylim = c(0, max_density_value * 1.1))
      # Add a bit of padding for y-limits

      # Fill under the non-significant density curve
      polygon(c(non_significant_density$x, rev(non_significant_density$x)),
              c(rep(0, length(non_significant_density$x)),
                rev(non_significant_density$y)),
              col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
      # Light blue fill

      # Add the density lines for other groups
      lines(significant_density, col = "darkblue", lwd = 2)
      lines(most_significant_density, col = "darkgray", lwd = 2)

      # Fill under the significant density curve
      polygon(c(significant_density$x, rev(significant_density$x)),
              c(rep(0, length(significant_density$x)),
                rev(significant_density$y)),
              col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
      # Light dark blue fill

      # Fill under the most significant density curve
      polygon(c(most_significant_density$x, rev(most_significant_density$x)),
              c(rep(0, length(most_significant_density$x)),
                rev(most_significant_density$y)),
              col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
      # Light gray fill

      # Add a legend
      legend("topright", legend = c("Non Significant", "Significant",
                                    "Most Significant"),
             col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
    } else{
      boxplot(Median_Difference_Score ~ data[[severity_col]], data = data,
              main = "Difference Scores by Disability Severity",
              names = c("Non significant", "Significant",
                        "Most significant"),
              xlab = "Disability Severity",
              ylab = "Median Difference Scores",
              col = "steelblue")
    }


  })


  # Download handler for meta_diff_plot6
  output$download_meta_diff_plot6 <- downloadHandler(
    filename = function() { "difference_scores_by_severity_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      if (length(unique(data$Median_Difference_Score[
        data[[severity_col]] == 0])) >= 2 &
        length(unique(data$Median_Difference_Score[
          data[[severity_col]] == 1])) >= 2 &
        length(unique(data$Median_Difference_Score[
          data[[severity_col]] == 2])) >= 2
      ) {
        non_significant_density <- density(data$Median_Difference_Score[
          data[[severity_col]] == 0], na.rm = TRUE)
        significant_density <- density(data$Median_Difference_Score[
          data[[severity_col]] == 1], na.rm = TRUE)
        most_significant_density <- density(data$Median_Difference_Score[
          data[[severity_col]] == 2], na.rm = TRUE)

        max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                   max(significant_density$y, na.rm = TRUE),
                                   max(most_significant_density$y, na.rm = TRUE)),
                                 na.rm = TRUE)

        plot(non_significant_density, col = "steelblue4", lwd = 2,
             main = "Density Plot of Median Difference Scores by Disability Severity",
             xlab = "Median Difference Scores", ylab = "Density",
             xlim = range(c(non_significant_density$x, significant_density$x,
                            most_significant_density$x)),
             ylim = c(0, max_density_value * 1.1))
        polygon(c(non_significant_density$x, rev(non_significant_density$x)),
                c(rep(0, length(non_significant_density$x)),
                  rev(non_significant_density$y)),
                col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
        lines(significant_density, col = "darkblue", lwd = 2)
        lines(most_significant_density, col = "darkgray", lwd = 2)
        polygon(c(significant_density$x, rev(significant_density$x)),
                c(rep(0, length(significant_density$x)),
                  rev(significant_density$y)),
                col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
        polygon(c(most_significant_density$x, rev(most_significant_density$x)),
                c(rep(0, length(most_significant_density$x)),
                  rev(most_significant_density$y)),
                col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
        legend("topright", legend = c("Non Significant", "Significant",
                                      "Most Significant"),
               col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
      } else {
        boxplot(Median_Difference_Score ~ data[[severity_col]], data = data,
                main = "Difference Scores by Disability Severity",
                names = c("Non significant", "Significant",
                          "Most significant"),
                xlab = "Disability Severity",
                ylab = "Median Difference Scores",
                col = "steelblue")
      }
      dev.off()
    }
  )



  output$meta_diff_plot7 <- renderPlot({
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

  # Download handler for meta_diff_plot7
  output$download_meta_diff_plot7 <- downloadHandler(
    filename = function() { "difference_scores_by_disability_type_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      boxplot(Median_Difference_Score ~ Primary_Impairment_Group,
              data = data,
              main = "Difference Scores by Primary Disability Type",
              xlab = "Primary Disability",
              ylab = "Median Difference Scores",
              col = "steelblue")
      dev.off()
    }
  )

  output$meta_diff_plot5 <- renderPlot({
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

  })

  # Download handler for meta_diff_plot5
  output$download_meta_diff_plot5 <- downloadHandler(
    filename = function() { "difference_scores_by_race_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)

      data_subset <- data[, .SD, .SDcols = c("Median_Difference_Score", race_cols)]

      long_data <- melt(data_subset,
                        id.vars = "Median_Difference_Score",
                        measure.vars = race_cols,
                        variable.name = "Race",
                        value.name = "Has_Race")
      filtered_data <- long_data[Has_Race == 1]

      par(oma = c(0, 0, 0, 0) + 0.6)
      boxplot(Median_Difference_Score ~ Race, data = filtered_data,
              col = "steelblue",
              xaxt = "n", yaxt = "n",
              xlab = "", ylab = "Median Difference Score",
              main = "Difference Scores Across Race")
      axis(side = 2, las = 2, mgp = c(3, 0.75, 0))
      text(x = 1:length(race_cols),
           y = par("usr")[3] - 0.45,
           labels = gsub("^E[0-9]+_|_911$", "", race_cols),
           xpd = NA, srt = 45, cex = .8, adj = 1)
      dev.off()
    }
  )


  output$meta_wage_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    if (is.null(data)) {
      return("No data available.")
    }

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    hist(wages_vector,
         col = "steelblue",
         main = "Distribution of Exit Wages",
         xlab = "Exit Wage ($ per hour)")

  })

  # Download handler for meta_wage_plot1
  output$download_meta_wage_plot1 <- downloadHandler(
    filename = function() { "exit_wages_distribution_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      if (is.null(data)) {
        return("No data available.")
      }

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      hist(wages_vector,
           col = "steelblue",
           main = "Distribution of Exit Wages",
           xlab = "Exit Wage ($ per hour)")
      dev.off()
    }
  )


  output$meta_wage_plot2 <- renderPlot({
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

  # Download handler for meta_wage_plot2
  output$download_meta_wage_plot2 <- downloadHandler(
    filename = function() { "exit_wages_across_days_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      plot(data$Median_Time_Passed_Days,
           wages_vector,
           main = "Exit Wages Across Days in Program",
           ylab = "Exit Wages ($ per hour)",
           xlab = "Median Days Spent in Programs (per individual)",
           col = "steelblue",
           pch = 3)
      dev.off()
    }
  )

  output$meta_wage_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # plot(as.numeric(data$Enroll_Length), wages_vector,
    #      col = "steelblue",
    #      main = "Exit Wages Across Quarters Enrolled",
    #      ylab = "Exit Wages ($ per Hour)",
    #      xlab = "Total Quarters Enrolled",
    #      pch = 8)

    boxplot(wages_vector ~ data[["Enroll_Length_Grp"]],
            main = "Exit Wages Across Quarters Enrolled",
            xlab = "Enrollment Length (total quarters)",
            ylab = "Exit Wages ($ per Hour)",
            col = "steelblue")

    })

  # Download handler for meta_wage_plot3
  output$download_meta_wage_plot3 <- downloadHandler(
    filename = function() { "exit_wages_across_quarters_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      boxplot(wages_vector ~ data[["Enroll_Length_Grp"]],
              main = "Exit Wages Across Quarters Enrolled",
              xlab = "Enrollment Length (total quarters)",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
      dev.off()
    }
  )


  # output$meta_wage_plot4 <- renderPlot({
  #   req(selected_data())
  #   data <- selected_data()
  #
  #   # Identify the column with wage data (exit wage column)
  #   wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
  #                    value = TRUE, perl = TRUE)
  #
  #   # Extract wage data
  #   wages <- data[, .SD, .SDcols = wage_col]
  #   wages_vector <- as.vector(unlist(wages)) # Convert to vector if needed
  #
  #   # Identify the column for gender/sex
  #   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                   value = TRUE, perl = TRUE)
  #
  #   if (length(sex_col) == 0) {
  #     stop("No gender/sex column found in the dataset.")
  #   }
  #
  #
  #   if (length(unique(wage_col[data[[sex_col]] == 0])) >= 2 &
  #       length(unique(wage_col[data[[sex_col]] == 1])) >= 2 &
  #       length(unique(wage_col[data[[sex_col]] == 9])) >= 2
  #   ) {
  #
  #     # Calculate densities for each gender group
  #     females_density <- density(wages_vector[data[[sex_col]] == 0],
  #                                na.rm = TRUE)
  #     males_density <- density(wages_vector[data[[sex_col]] == 1],
  #                              na.rm = TRUE)
  #     did_not_identify_density <- density(wages_vector[data[[sex_col]] == 9],
  #                                         na.rm = TRUE)
  #
  #     # Determine the maximum y value for setting y limits
  #     max_density_value <- max(c(max(females_density$y, na.rm = TRUE),
  #                                max(males_density$y, na.rm = TRUE),
  #                                max(did_not_identify_density$y, na.rm = TRUE)),
  #                              na.rm = TRUE)
  #
  #     # Initialize the plot with dynamic y-limits
  #     plot(females_density, col = "steelblue4", lwd = 2,
  #          main = "Density Plot of Exit Wages by Gender",
  #          xlab = "Exit Wages ($ per Hour)", ylab = "Density",
  #          xlim = range(c(females_density$x, males_density$x,
  #                         did_not_identify_density$x)),
  #          ylim = c(0, max_density_value * 1.1))
  #
  #     # Fill under the density curves for each group
  #     polygon(c(females_density$x, rev(females_density$x)),
  #             c(rep(0, length(females_density$x)), rev(females_density$y)),
  #             col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
  #
  #     lines(males_density, col = "darkblue", lwd = 2)
  #     polygon(c(males_density$x, rev(males_density$x)),
  #             c(rep(0, length(males_density$x)), rev(males_density$y)),
  #             col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
  #
  #     lines(did_not_identify_density, col = "darkgray", lwd = 2)
  #     polygon(c(did_not_identify_density$x, rev(did_not_identify_density$x)),
  #             c(rep(0, length(did_not_identify_density$x)),
  #               rev(did_not_identify_density$y)),
  #             col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
  #
  #     # Add a legend to identify the gender groups
  #     legend("topright", legend = c("Females", "Males", "Did not identify"),
  #            col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
  #
  #   } else {
  #     boxplot(wages_vector ~ data[[sex_col]],
  #             main = "Exit Wages by Gender",
  #             names = c("Females", "Males", "Did not identify"),
  #             xlab = "Gender",
  #             ylab = "Exit Wages ($ per Hour)",
  #             col = "steelblue")
  #   }
  #
  # })

  # output$meta_wage_plot4 <- renderPlot({
  #   req(selected_data())
  #   data <- selected_data()
  #
  #   # Identify the column with wage data (exit wage column)
  #   wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data), value = TRUE, perl = TRUE)
  #   wages <- data[, .SD, .SDcols = wage_col]
  #   wages_vector <- as.vector(unlist(wages)) # Convert to vector if needed
  #
  #   # Identify the column for gender/sex
  #   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data), value = TRUE, perl = TRUE)
  #
  #   if (length(sex_col) == 0) {
  #     stop("No gender/sex column found in the dataset.")
  #   }
  #
  #   if (all(sapply(c(0, 1, 3, 9), function(g) length(unique(wages_vector[data[[sex_col]] == g])) >= 2))) {
  #     # Calculate densities for each group
  #     females_density <- density(wages_vector[data[[sex_col]] == 0], na.rm = TRUE)
  #     males_density <- density(wages_vector[data[[sex_col]] == 1], na.rm = TRUE)
  #     other_density <- density(wages_vector[data[[sex_col]] == 3], na.rm = TRUE)
  #     did_not_identify_density <- density(wages_vector[data[[sex_col]] == 9], na.rm = TRUE)
  #
  #     # Determine the maximum y value for setting y limits
  #     max_density_value <- max(c(max(females_density$y, na.rm = TRUE),
  #                                max(males_density$y, na.rm = TRUE),
  #                                max(other_density$y, na.rm = TRUE),
  #                                max(did_not_identify_density$y, na.rm = TRUE)), na.rm = TRUE)
  #
  #     # Initialize the plot with dynamic y-limits
  #     plot(females_density, col = "steelblue4", lwd = 2,
  #          main = "Density Plot of Exit Wages by Gender",
  #          xlab = "Exit Wages ($ per Hour)", ylab = "Density",
  #          xlim = range(c(females_density$x, males_density$x, other_density$x, did_not_identify_density$x)),
  #          ylim = c(0, max_density_value * 1.1))
  #
  #     # Fill under the density curves for each group
  #     polygon(c(females_density$x, rev(females_density$x)),
  #             c(rep(0, length(females_density$x)), rev(females_density$y)),
  #             col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
  #
  #     lines(males_density, col = "darkblue", lwd = 2)
  #     polygon(c(males_density$x, rev(males_density$x)),
  #             c(rep(0, length(males_density$x)), rev(males_density$y)),
  #             col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
  #
  #     lines(other_density, col = "darkgreen", lwd = 2)
  #     polygon(c(other_density$x, rev(other_density$x)),
  #             c(rep(0, length(other_density$x)), rev(other_density$y)),
  #             col = rgb(0.2, 0.8, 0.2, alpha = 0.3), border = NA)
  #
  #     lines(did_not_identify_density, col = "darkgray", lwd = 2)
  #     polygon(c(did_not_identify_density$x, rev(did_not_identify_density$x)),
  #             c(rep(0, length(did_not_identify_density$x)), rev(did_not_identify_density$y)),
  #             col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
  #
  #     # Add a legend to identify the gender groups
  #     legend("topright", legend = c("Females", "Males", "Other", "Did not identify"),
  #            col = c("steelblue4", "darkblue", "darkgreen", "darkgray"), lwd = 2)
  #   } else {
  #     boxplot(wages_vector ~ data[[sex_col]],
  #             main = "Exit Wages by Gender",
  #             names = c("Females", "Males", "Other", "Did not identify"),
  #             xlab = "Gender",
  #             ylab = "Exit Wages ($ per Hour)",
  #             col = c("steelblue", "darkblue", "darkgreen", "darkgray"))
  #   }
  # })

  output$meta_wage_plot4 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # Identify the column with wage data (exit wage column)
    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages)) # Convert to vector if needed

    # Identify the column for gender/sex
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    if (length(sex_col) == 0) {
      stop("No gender/sex column found in the dataset.")
    }

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Replace numeric values with labels
    labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                  labels = gender_labels)

    # Check if each gender group has enough unique values for density computation
    if (all(sapply(levels(labeled_gender_data), function(g) {
      sum(labeled_gender_data == g & !is.na(wages_vector)) >= 2
    }))) {
      # Compute density for each gender group
      density_data <- lapply(levels(labeled_gender_data), function(g) {
        density(wages_vector[labeled_gender_data == g], na.rm = TRUE)
      })

      # Determine maximum y-limit for all densities
      max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

      # Plot the first density curve to set up the plot
      plot(density_data[[1]], col = "steelblue4", lwd = 2,
           main = "Density Plot of Exit Wages by Gender",
           xlab = "Exit Wages ($ per Hour)", ylab = "Density",
           xlim = range(sapply(density_data, function(d) range(d$x))),
           ylim = c(0, max_density_value * 1.1))

      # Add density curves and fill areas for each group
      colors <- c("steelblue4", "darkblue", "darkgreen", "darkgray")
      for (i in seq_along(density_data)) {
        lines(density_data[[i]], col = colors[i], lwd = 2)
        polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                col = adjustcolor(colors[i], alpha.f = 0.3), border = NA)
      }

      # Add legend
      legend("topright", legend = levels(labeled_gender_data),
             col = colors, lwd = 2)
    } else {
      # Fallback: Create a boxplot if densities cannot be computed
      boxplot(wages_vector ~ labeled_gender_data, data = data,
              main = "Exit Wages by Gender",
              xlab = "Gender",
              ylab = "Exit Wages ($ per Hour)",
              col = c("steelblue", "darkblue", "darkgreen", "darkgray"))
    }
  })



  # output$download_meta_wage_plot4 <- downloadHandler(
  #   filename = function() { "exit_wages_by_gender_plot.png" },
  #   content = function(file) {
  #     png(file)
  #     data <- selected_data()
  #
  #     wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data), value = TRUE, perl = TRUE)
  #     wages <- data[, .SD, .SDcols = wage_col]
  #     wages_vector <- as.vector(unlist(wages))
  #
  #     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data), value = TRUE, perl = TRUE)
  #
  #     if (length(sex_col) == 0) {
  #       stop("No gender/sex column found in the dataset.")
  #     }
  #
  #     if (all(sapply(c(0, 1, 3, 9), function(g) length(unique(wages_vector[data[[sex_col]] == g])) >= 2))) {
  #       females_density <- density(wages_vector[data[[sex_col]] == 0], na.rm = TRUE)
  #       males_density <- density(wages_vector[data[[sex_col]] == 1], na.rm = TRUE)
  #       other_density <- density(wages_vector[data[[sex_col]] == 3], na.rm = TRUE)
  #       did_not_identify_density <- density(wages_vector[data[[sex_col]] == 9], na.rm = TRUE)
  #
  #       max_density_value <- max(c(max(females_density$y, na.rm = TRUE),
  #                                  max(males_density$y, na.rm = TRUE),
  #                                  max(other_density$y, na.rm = TRUE),
  #                                  max(did_not_identify_density$y, na.rm = TRUE)), na.rm = TRUE)
  #
  #       plot(females_density, col = "steelblue4", lwd = 2,
  #            main = "Density Plot of Exit Wages by Gender",
  #            xlab = "Exit Wages ($ per Hour)", ylab = "Density",
  #            xlim = range(c(females_density$x, males_density$x, other_density$x, did_not_identify_density$x)),
  #            ylim = c(0, max_density_value * 1.1))
  #
  #       polygon(c(females_density$x, rev(females_density$x)),
  #               c(rep(0, length(females_density$x)), rev(females_density$y)),
  #               col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
  #
  #       lines(males_density, col = "darkblue", lwd = 2)
  #       polygon(c(males_density$x, rev(males_density$x)),
  #               c(rep(0, length(males_density$x)), rev(males_density$y)),
  #               col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
  #
  #       lines(other_density, col = "darkgreen", lwd = 2)
  #       polygon(c(other_density$x, rev(other_density$x)),
  #               c(rep(0, length(other_density$x)), rev(other_density$y)),
  #               col = rgb(0.2, 0.8, 0.2, alpha = 0.3), border = NA)
  #
  #       lines(did_not_identify_density, col = "darkgray", lwd = 2)
  #       polygon(c(did_not_identify_density$x, rev(did_not_identify_density$x)),
  #               c(rep(0, length(did_not_identify_density$x)), rev(did_not_identify_density$y)),
  #               col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
  #
  #       legend("topright", legend = c("Females", "Males", "Other", "Did not identify"),
  #              col = c("steelblue4", "darkblue", "darkgreen", "darkgray"), lwd = 2)
  #     } else {
  #       boxplot(wages_vector ~ data[[sex_col]],
  #               main = "Exit Wages by Gender",
  #               names = c("Females", "Males", "Other", "Did not identify"),
  #               xlab = "Gender",
  #               ylab = "Exit Wages ($ per Hour)",
  #               col = c("steelblue", "darkblue", "darkgreen", "darkgray"))
  #     }
  #     dev.off()
  #   }
  # )


  # Download handler for meta_wage_plot4
  # output$download_meta_wage_plot4 <- downloadHandler(
  #   filename = function() { "exit_wages_by_gender_plot.png" },
  #   content = function(file) {
  #     png(file)
  #     data <- selected_data()
  #
  #     wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
  #                      value = TRUE, perl = TRUE)
  #     wages <- data[, .SD, .SDcols = wage_col]
  #     wages_vector <- as.vector(unlist(wages))
  #
  #     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                     value = TRUE, perl = TRUE)
  #
  #     if (length(sex_col) == 0) {
  #       stop("No gender/sex column found in the dataset.")
  #     }
  #
  #     if (length(unique(wage_col[data[[sex_col]] == 0])) >= 2 &
  #         length(unique(wage_col[data[[sex_col]] == 1])) >= 2 &
  #         length(unique(wage_col[data[[sex_col]] == 9])) >= 2
  #     ) {
  #       females_density <- density(wages_vector[data[[sex_col]] == 0], na.rm = TRUE)
  #       males_density <- density(wages_vector[data[[sex_col]] == 1], na.rm = TRUE)
  #       did_not_identify_density <- density(wages_vector[data[[sex_col]] == 9], na.rm = TRUE)
  #
  #       max_density_value <- max(c(max(females_density$y, na.rm = TRUE),
  #                                  max(males_density$y, na.rm = TRUE),
  #                                  max(did_not_identify_density$y, na.rm = TRUE)),
  #                                na.rm = TRUE)
  #
  #       plot(females_density, col = "steelblue4", lwd = 2,
  #            main = "Density Plot of Exit Wages by Gender",
  #            xlab = "Exit Wages ($ per Hour)", ylab = "Density",
  #            xlim = range(c(females_density$x, males_density$x,
  #                           did_not_identify_density$x)),
  #            ylim = c(0, max_density_value * 1.1))
  #
  #       polygon(c(females_density$x, rev(females_density$x)),
  #               c(rep(0, length(females_density$x)), rev(females_density$y)),
  #               col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
  #
  #       lines(males_density, col = "darkblue", lwd = 2)
  #       polygon(c(males_density$x, rev(males_density$x)),
  #               c(rep(0, length(males_density$x)), rev(males_density$y)),
  #               col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
  #
  #       lines(did_not_identify_density, col = "darkgray", lwd = 2)
  #       polygon(c(did_not_identify_density$x, rev(did_not_identify_density$x)),
  #               c(rep(0, length(did_not_identify_density$x)),
  #                 rev(did_not_identify_density$y)),
  #               col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
  #
  #       legend("topright", legend = c("Females", "Males", "Did not identify"),
  #              col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
  #     } else {
  #       boxplot(wages_vector ~ data[[sex_col]],
  #               main = "Exit Wages by Gender",
  #               names = c("Females", "Males", "Did not identify"),
  #               xlab = "Gender",
  #               ylab = "Exit Wages ($ per Hour)",
  #               col = "steelblue")
  #     }
  #     dev.off()
  #   }
  # )

  # Download handler for meta_wage_plot4
  output$download_meta_wage_plot4 <- downloadHandler(
    filename = function() { "exit_wages_by_gender_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      # Identify the column with wage data (exit wage column)
      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      # Identify the column for gender/sex
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

      if (length(sex_col) == 0) {
        stop("No gender/sex column found in the dataset.")
      }

      # Define the mapping of numeric values to labels
      gender_labels <- c(
        "1" = "Male",
        "2" = "Female",
        "3" = "Other",
        "9" = "Did not identify"
      )

      # Replace numeric values with labels
      labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                    labels = gender_labels)

      # Check if each gender group has enough unique values for density computation
      if (all(sapply(levels(labeled_gender_data), function(g) {
        sum(labeled_gender_data == g & !is.na(wages_vector)) >= 2
      }))) {
        # Compute density for each gender group
        density_data <- lapply(levels(labeled_gender_data), function(g) {
          density(wages_vector[labeled_gender_data == g], na.rm = TRUE)
        })

        # Determine maximum y-limit for all densities
        max_density_value <- max(sapply(density_data, function(d) max(d$y, na.rm = TRUE)), na.rm = TRUE)

        # Plot the first density curve to set up the plot
        plot(density_data[[1]], col = "steelblue4", lwd = 2,
             main = "Density Plot of Exit Wages by Gender",
             xlab = "Exit Wages ($ per Hour)", ylab = "Density",
             xlim = range(sapply(density_data, function(d) range(d$x))),
             ylim = c(0, max_density_value * 1.1))

        # Add density curves and fill areas for each group
        colors <- c("steelblue4", "darkblue", "darkgreen", "darkgray")
        for (i in seq_along(density_data)) {
          lines(density_data[[i]], col = colors[i], lwd = 2)
          polygon(c(density_data[[i]]$x, rev(density_data[[i]]$x)),
                  c(rep(0, length(density_data[[i]]$x)), rev(density_data[[i]]$y)),
                  col = adjustcolor(colors[i], alpha.f = 0.3), border = NA)
        }

        # Add legend
        legend("topright", legend = levels(labeled_gender_data),
               col = colors, lwd = 2)
      } else {
        # Fallback: Create a boxplot if densities cannot be computed
        boxplot(wages_vector ~ labeled_gender_data, data = data,
                main = "Exit Wages by Gender",
                xlab = "Gender",
                ylab = "Exit Wages ($ per Hour)",
                col = c("steelblue", "darkblue", "darkgreen", "darkgray"))
      }

      dev.off()
    }
  )




  output$meta_wage_plot6 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                     value = TRUE, perl = TRUE)
    wages <- data[, .SD, .SDcols = wage_col]
    wages_vector <- as.vector(unlist(wages))

    # Identify the severity column dynamically
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                         names(data), value = TRUE, perl = TRUE)

    if (length(unique(wage_col[data[[severity_col]] == 0])) >= 2 &
        length(unique(wage_col[data[[severity_col]] == 1])) >= 2 &
        length(unique(wage_col[data[[severity_col]] == 2])) >= 2
        ) {
      # Create density for each group
      non_significant_density <- density(wage_col[
        data[[severity_col]] == 0], na.rm = TRUE)
      significant_density <- density(wage_col[
        data[[severity_col]] == 1], na.rm = TRUE)
      most_significant_density <- density(wage_col[
        data[[severity_col]] == 2], na.rm = TRUE)

      # Determine the maximum y value for setting y limits
      max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                 max(significant_density$y, na.rm = TRUE),
                                 max(most_significant_density$y, na.rm = TRUE)),
                               na.rm = TRUE)

      # Initialize the plot with dynamic y-limits
      plot(non_significant_density, col = "steelblue4", lwd = 2,
           main = "Density Plot of Ending Wages by Disability Severity",
           xlab = "Median Difference Scores", ylab = "Density",
           xlim = range(c(non_significant_density$x, significant_density$x,
                          most_significant_density$x)),
           ylim = c(0, max_density_value * 1.1))
      # Add a bit of padding for y-limits

      # Fill under the non-significant density curve
      polygon(c(non_significant_density$x, rev(non_significant_density$x)),
              c(rep(0, length(non_significant_density$x)),
                rev(non_significant_density$y)),
              col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)
      # Light blue fill

      # Add the density lines for other groups
      lines(significant_density, col = "darkblue", lwd = 2)
      lines(most_significant_density, col = "darkgray", lwd = 2)

      # Fill under the significant density curve
      polygon(c(significant_density$x, rev(significant_density$x)),
              c(rep(0, length(significant_density$x)),
                rev(significant_density$y)),
              col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)
      # Light dark blue fill

      # Fill under the most significant density curve
      polygon(c(most_significant_density$x, rev(most_significant_density$x)),
              c(rep(0, length(most_significant_density$x)),
                rev(most_significant_density$y)),
              col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)
      # Light gray fill

      # Add a legend
      legend("topright", legend = c("Non Significant", "Significant",
                                    "Most Significant"),
             col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
    } else {
      boxplot(wages_vector ~ data[[severity_col]],
              main = "Exit Wages by Disability Severity",
              names = c("Non significant", "Significant",
                        "Most significant"),
              xlab = "Disability Severity",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
    }
  })

  output$download_meta_wage_plot6 <- downloadHandler(
    filename = function() { "exit_wages_by_severity_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      if (length(unique(wage_col[data[[severity_col]] == 0])) >= 2 &
          length(unique(wage_col[data[[severity_col]] == 1])) >= 2 &
          length(unique(wage_col[data[[severity_col]] == 2])) >= 2
      ) {
        non_significant_density <- density(wage_col[data[[severity_col]] == 0], na.rm = TRUE)
        significant_density <- density(wage_col[data[[severity_col]] == 1], na.rm = TRUE)
        most_significant_density <- density(wage_col[data[[severity_col]] == 2], na.rm = TRUE)

        max_density_value <- max(c(max(non_significant_density$y, na.rm = TRUE),
                                   max(significant_density$y, na.rm = TRUE),
                                   max(most_significant_density$y, na.rm = TRUE)),
                                 na.rm = TRUE)

        plot(non_significant_density, col = "steelblue4", lwd = 2,
             main = "Density Plot of Exit Wages by Disability Severity",
             xlab = "Median Difference Scores", ylab = "Density",
             xlim = range(c(non_significant_density$x, significant_density$x, most_significant_density$x)),
             ylim = c(0, max_density_value * 1.1))

        polygon(c(non_significant_density$x, rev(non_significant_density$x)),
                c(rep(0, length(non_significant_density$x)),
                  rev(non_significant_density$y)),
                col = rgb(0.2, 0.6, 1, alpha = 0.3), border = NA)

        lines(significant_density, col = "darkblue", lwd = 2)
        lines(most_significant_density, col = "darkgray", lwd = 2)

        polygon(c(significant_density$x, rev(significant_density$x)),
                c(rep(0, length(significant_density$x)),
                  rev(significant_density$y)),
                col = rgb(0, 0, 0.5, alpha = 0.3), border = NA)

        polygon(c(most_significant_density$x, rev(most_significant_density$x)),
                c(rep(0, length(most_significant_density$x)),
                  rev(most_significant_density$y)),
                col = rgb(0.5, 0.5, 0.5, alpha = 0.3), border = NA)

        legend("topright", legend = c("Non Significant", "Significant", "Most Significant"),
               col = c("steelblue4", "darkblue", "darkgray"), lwd = 2)
      } else {
        boxplot(wages_vector ~ data[[severity_col]],
                main = "Exit Wages by Disability Severity",
                names = c("Non significant", "Significant", "Most significant"),
                xlab = "Disability Severity",
                ylab = "Exit Wages ($ per Hour)",
                col = "steelblue")
      }
      dev.off()
    }
  )


  output$meta_wage_plot7 <- renderPlot({
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

  output$download_meta_wage_plot7 <- downloadHandler(
    filename = function() { "exit_wages_by_impairment_type_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      boxplot(wages_vector ~ Primary_Impairment_Group,
              data = data,
              main = "Exit Wages by Primary Impairment Type",
              xlab = "Primary Impairment",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
      dev.off()
    }
  )



  output$meta_wage_plot5 <- renderPlot({
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

  output$download_meta_wage_plot5 <- downloadHandler(
    filename = function() { "exit_wages_by_race_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
                       value = TRUE, perl = TRUE)
      wages <- data[, .SD, .SDcols = wage_col]
      wages_vector <- as.vector(unlist(wages))

      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)

      data_subset <- data[, .SD, .SDcols = race_cols]

      boxplot(wages_vector ~ data[[race_cols[1]]],
              main = "Exit Wages by Race/Ethnicity",
              xlab = "Race/Ethnicity",
              ylab = "Exit Wages ($ per Hour)",
              col = "steelblue")
      dev.off()
    }
  )



  output$meta_employ_plot1 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    barplot(table(data$Final_Employment),
            main = "Distribution of Exit Employment",
            names = c("Non-competitive", "Competitive"),
            xlab = "Exit Employment Status",
            col = c("lightsteelblue", "steelblue"))

  })

  output$download_meta_employ_plot1 <- downloadHandler(
    filename = function() { "exit_employment_distribution_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      barplot(table(data$Final_Employment),
              main = "Distribution of Exit Employment",
              names = c("Non-competitive", "Competitive"),
              xlab = "Exit Employment Status",
              col = c("lightsteelblue", "steelblue"))
      dev.off()
    }
  )


  output$meta_employ_plot2 <- renderPlot({
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

  output$download_meta_employ_plot2 <- downloadHandler(
    filename = function() { "exit_employment_across_time_in_program_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                            names(data), value = TRUE, perl = TRUE)

      plot(data$Median_Time_Passed_Days,
           as.character(data$Final_Employment),
           main = "Exit Employment Across Time in Program",
           ylab = "Exit Employment",
           xlab = "Median Days Spent in Programs (per individual)",
           col = "steelblue",
           pch = 8)
      dev.off()
    }
  )


  output$meta_employ_plot3 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
                          names(data), value = TRUE, perl = TRUE)

    # these variables have been created in the data cleaning process,
    #   so we can use the exact names
    # plot(data$Enroll_Length,
    #      as.character(data$Final_Employment),
    #      main = "Exit Employment Across Enrollment Length",
    #      ylab = "Exit Employment",
    #      xlab = "Enrollment Length (total quarters)",
    #      col = "steelblue",
    #      pch = 8)


    # Create a contingency table of Final_Employment by Gender
    employment_enroll_table <- table(data$Final_Employment,
                                     data[["Enroll_Length_Grp"]])

    rownames(employment_enroll_table) <- c("Non-competitive Employment",
                                           "Competitive Employment")

    colnames(employment_enroll_table) <- c("<5", "5-10", "11+")


    # Create a bar plot with bars broken up by gender
    barplot(employment_enroll_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topright", bty = "n",
                               title = "Employment Type"),
            xlab = "Enrollment Length (total quarters)", ylab = "Count",
            main = "Exit Employment Across Quarters Enrolled")

  })

  output$download_meta_employ_plot3 <- downloadHandler(
    filename = function() { "exit_employment_across_enrollment_length_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      employment_enroll_table <- table(data$Final_Employment,
                                       data[["Enroll_Length_Grp"]])

      rownames(employment_enroll_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")

      colnames(employment_enroll_table) <- c("<5", "5-10", "11+")

      barplot(employment_enroll_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topright", bty = "n",
                                 title = "Employment Type"),
              xlab = "Enrollment Length (total quarters)", ylab = "Count",
              main = "Exit Employment Across Quarters Enrolled")
      dev.off()
    }
  )


  # output$meta_employ_plot4 <- renderPlot({
  #   req(selected_data())
  #   data <- selected_data()
  #
  #   # the name for the gender/sex column could be varied, so we need to
  #   #   account for this possibility
  #   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                   value = TRUE, perl = TRUE)
  #
  #
  #   # Create a contingency table of Final_Employment by Gender
  #   employment_gender_table <- table(data$Final_Employment,
  #                                    data[[sex_col]])
  #
  #   rownames(employment_gender_table) <- c("Non-competitive Employment",
  #                                          "Competitive Employment")
  #
  #   colnames(employment_gender_table) <- c("Male",
  #                                          "Female",
  #                                          "Other",
  #                                          "Did not identify")
  #
  #
  #   # Create a bar plot with bars broken up by gender
  #   barplot(employment_gender_table, beside = TRUE,
  #           col = c("lightsteelblue", "steelblue"),
  #           legend.text = c("Non-competitive", "Competitive"),
  #           args.legend = list(x = "topleft", bty = "n",
  #                              title = "Employment Type"),
  #           xlab = "Gender", ylab = "Count",
  #           main = "Exit Employment by Gender")
  #
  # })

  output$meta_employ_plot4 <- renderPlot({
    req(selected_data())
    data <- selected_data()

    # Identify the gender/sex column dynamically
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                    value = TRUE, perl = TRUE)

    if (length(sex_col) == 0) {
      stop("No gender/sex column found in the dataset.")
    }

    # Define the mapping of numeric values to labels
    gender_labels <- c(
      "1" = "Male",
      "2" = "Female",
      "3" = "Other",
      "9" = "Did not identify"
    )

    # Replace numeric values with labels
    labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                  labels = gender_labels)

    # Create a contingency table of Final_Employment by Gender
    employment_gender_table <- table(data$Final_Employment, labeled_gender_data)

    # Assign row names for employment types
    rownames(employment_gender_table) <- c("Non-competitive Employment",
                                           "Competitive Employment")

    # Create a bar plot with bars broken up by gender
    barplot(employment_gender_table, beside = TRUE,
            col = c("lightsteelblue", "steelblue"),
            legend.text = c("Non-competitive", "Competitive"),
            args.legend = list(x = "topright", bty = "n",
                               title = "Employment Type"),
            xlab = "Gender", ylab = "Count",
            main = "Exit Employment by Gender")
  })


  # output$download_meta_employ_plot4 <- downloadHandler(
  #   filename = function() { "exit_employment_by_gender_plot.png" },
  #   content = function(file) {
  #     png(file)
  #     data <- selected_data()
  #
  #     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
  #                     value = TRUE, perl = TRUE)
  #
  #     employment_gender_table <- table(data$Final_Employment,
  #                                      data[[sex_col]])
  #
  #     rownames(employment_gender_table) <- c("Non-competitive Employment",
  #                                            "Competitive Employment")
  #
  #     colnames(employment_gender_table) <- c("Male",
  #                                            "Female",
  #                                            "Other",
  #                                            "Did not identify")
  #
  #     barplot(employment_gender_table, beside = TRUE,
  #             col = c("lightsteelblue", "steelblue"),
  #             legend.text = c("Non-competitive", "Competitive"),
  #             args.legend = list(x = "topleft", bty = "n",
  #                                title = "Employment Type"),
  #             xlab = "Gender", ylab = "Count",
  #             main = "Exit Employment by Gender")
  #     dev.off()
  #   }
  # )

  output$download_meta_employ_plot4 <- downloadHandler(
    filename = function() { "exit_employment_by_gender_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      # Identify the gender/sex column dynamically
      sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
                      value = TRUE, perl = TRUE)

      if (length(sex_col) == 0) {
        stop("No gender/sex column found in the dataset.")
      }

      # Define the mapping of numeric values to labels
      gender_labels <- c(
        "1" = "Male",
        "2" = "Female",
        "3" = "Other",
        "9" = "Did not identify"
      )

      # Replace numeric values with labels
      labeled_gender_data <- factor(data[[sex_col]], levels = names(gender_labels),
                                    labels = gender_labels)

      # Create a contingency table of Final_Employment by Gender
      employment_gender_table <- table(data$Final_Employment, labeled_gender_data)

      # Assign row names for employment types
      rownames(employment_gender_table) <- c("Non-competitive Employment",
                                             "Competitive Employment")

      # Create a bar plot with bars broken up by gender
      barplot(employment_gender_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topright", bty = "n",
                                 title = "Employment Type"),
              xlab = "Gender", ylab = "Count",
              main = "Exit Employment by Gender")

      dev.off()
    }
  )

  output$meta_employ_plot5 <- renderPlot({
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

  output$download_meta_employ_plot5 <- downloadHandler(
    filename = function() { "exit_employment_by_race_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                        names(data),
                        value = TRUE, perl = TRUE)

      data_subset <- data[, .SD, .SDcols = c("Final_Employment",
                                             race_cols)]

      long_data <- melt(data_subset,
                        id.vars = "Final_Employment",
                        measure.vars = race_cols,
                        variable.name = "Race",
                        value.name = "Has_Race")
      filtered_data <- long_data[Has_Race == 1]

      employment_race_table <- table(filtered_data$Final_Employment,
                                     filtered_data$Race)

      bar_midpoints <- barplot(employment_race_table, beside = TRUE,
                               col = c("lightsteelblue", "steelblue"),
                               legend.text = c("Non-competitive", "Competitive"),
                               args.legend = list(x = "topleft", bty = "n",
                                                  title = "Employment Type"),
                               ylab = "Count",
                               xaxt = "n", yaxt = "n", xlab = "",
                               main = "Final Employment by Race", las = 2)

      axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

      text(x = colMeans(bar_midpoints),
           y = par("usr")[3] - 0.45,
           labels = gsub("^E[0-9]+_|_911$", "", race_cols),
           xpd = NA,
           srt = 45,
           cex = .8,
           adj = 1)
      dev.off()
    }
  )



  output$meta_employ_plot6 <- renderPlot({
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

  output$download_meta_employ_plot6 <- downloadHandler(
    filename = function() { "exit_employment_by_severity_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
                           names(data), value = TRUE, perl = TRUE)

      employment_severity_table <- table(data$Final_Employment,
                                         data[[severity_col]])

      rownames(employment_severity_table) <- c("Non-competitive Employment",
                                               "Competitive Employment")

      colnames(employment_severity_table) <- c("Non-significant",
                                               "Significant",
                                               "Most significant")

      barplot(employment_severity_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topleft", bty = "n",
                                 title = "Employment Type"),
              xlab = "Disability Severity", ylab = "Count",
              main = "Exit Employment by Disability Severity")
      dev.off()
    }
  )



  output$meta_employ_plot7 <- renderPlot({
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

  })

  output$download_meta_employ_plot7 <- downloadHandler(
    filename = function() { "exit_employment_by_primary_impairment_plot.png" },
    content = function(file) {
      png(file)
      data <- selected_data()

      employment_prim_dis_table <- table(data$Final_Employment,
                                         data$Primary_Impairment_Group)

      rownames(employment_prim_dis_table) <- c("Non-competitive Employment",
                                               "Competitive Employment")

      barplot(employment_prim_dis_table, beside = TRUE,
              col = c("lightsteelblue", "steelblue"),
              legend.text = c("Non-competitive", "Competitive"),
              args.legend = list(x = "topleft", bty = "n",
                                 title = "Employment Type"),
              xlab = "Primary Impairment", ylab = "Count",
              main = "Exit Employment by Primary Impairment")
      dev.off()
    }
  )





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


  # Reactive function to create residual plots
  output$rsa_residuals1 <- renderPlot({
    req(model_rsa())

    # residuals
    model <- model_rsa()
    residuals <- resid(model)

    # histogram to look for normality
    hist(residuals, col = "steelblue")

  })

  output$rsa_residuals2 <- renderPlot({
    req(model_rsa())

    # residuals
    model <- model_rsa()
    residuals <- resid(model)

    # histogram to look for normality
    qqnorm(residuals)
    qqline(residuals, col = "steelblue")

  })

  output$rsa_residuals3 <- renderPlot({
    req(model_rsa())

    # Extract fitted values and residuals
    model <- model_rsa()
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

  # output$model_scores_summary <- renderPrint({
  #   req(model_scores())
  #   summary(model_scores()$anova)
  # })

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



  output$download_model_scores_summary <- downloadHandler(
    filename = function() {
      "anova_summary.txt"  # The name of the file to download
    },
    content = function(file) {
      # Extract and round the ANOVA table again for downloading
      anova_table <- summary(model_scores()$anova)[[1]]
      rounded_anova <- round(anova_table, 2)

      # Write the summary to a text file
      write.table(rounded_anova, file, sep = "\t", col.names = NA, quote = FALSE)
    }
  )


  # # Render Tukey HSD pairwise comparison results (significant pairs only)
  # output$tukey_scores_summary <- renderPrint({
  #   req(model_scores())
  #   tukey_result <- model_scores()$tukey
  #
  #   # Get the Tukey HSD results as a data frame
  #   tukey_df <- as.data.frame(tukey_result$Services)
  #   # Or use $Provider for "ANOVA across Providers"
  #
  #   # Filter only significant pairs (p-value < 0.05)
  #   significant_pairs <- tukey_df[tukey_df$`p adj` < 0.05, ]
  #
  #   # If there are no significant pairs, print a message
  #   if (nrow(significant_pairs) == 0) {
  #     cat("No significant (alpha = 0.05) pairwise comparisons found. May be due to sampling sizes.")
  #   } else {
  #     print(significant_pairs)
  #   }
  # })

  # output$tukey_scores_summary <- renderPrint({
  #   req(selected_data())
  #   req(input$anova)
  #   data <- selected_data()
  #   anova_test <- input$anova
  #
  #   req(model_scores())
  #   tukey_result <- model_scores()$tukey
  #
  #
  #   # Get the Tukey HSD results as a data frame
  #   tukey_df <- as.data.frame(tukey_result$Services)  # Or $Provider for providers
  #
  #   # Round numeric columns
  #   numeric_cols <- sapply(tukey_df, is.numeric)
  #   tukey_df[, numeric_cols] <- lapply(tukey_df[, numeric_cols], round, 2)
  #
  #   # Filter only significant pairs (p-value < 0.05)
  #   significant_pairs <- tukey_df[tukey_df$`p adj` < 0.05, ]
  #
  #   # If there are no significant pairs, print a message
  #   if (nrow(significant_pairs) == 0) {
  #     cat("No significant pairwise comparisons found. May be due to sampling sizes.")
  #   } else {
  #     print(significant_pairs)
  #   }
  # })

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

  model_metadata <- reactive({
    req(selected_data())
    data <- selected_data()

    response <- input$response
    if (response == "Predict Employment Outcome"){
      # employ_col <- grep("(?i)^(?=.*employment)(?!.*(?i)_desc)(?!.*(?i)_wage)(?!.*(?i)un)",
      #                    names(data), value = TRUE, perl = TRUE)
      # employ_col <- "E389_Q4_Employment_911"
#
#       exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
#                             names(data), value = TRUE, perl = TRUE)

      # if (length(exit_work_col) < 1){
      #   return("No employment variable available.")
      # } else{
        y <- "Final_Employment"
      # }


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

    # # Set model_run to TRUE when the model is executed
    # model_run(TRUE)

  })

  # Reactive function to create residual plots
  output$metadata_residuals1 <- renderPlot({
    response <- input$response
    req(model_metadata())

    # residuals
    model <- model_metadata()
    residuals <- resid(model)

    if (response == "Predict Ending Wage" ||
        response == "Predict Median Difference Score"){
      # histogram to look for normality
      hist(residuals, col = "steelblue")
    } else{
      y <- "Final_Employment"
      # Ensure the model is logistic (family = binomial)
      # Calculate predicted probabilities
      predicted_probs <- predict(model, type = "response")

      # Use pROC to generate an ROC curve
      library(pROC)
      roc_obj <- roc(model$y, predicted_probs)

      # Plot the ROC curve
      plot(roc_obj, col = "steelblue", lwd = 2, main = "ROC Curve")
      # Add diagonal line (no skill classifier
      # abline(a = 0, b = 1, col = "gray", lty = 2)
    }


  })

  output$metadata_residuals2 <- renderPlot({
    response <- input$response
    req(model_metadata())

    # residuals
    model <- model_metadata()
    residuals <- resid(model)

    if (response == "Predict Ending Wage" ||
        response == "Predict Median Difference Score") {

      # histogram to look for normality
      qqnorm(residuals)
      qqline(residuals, col = "steelblue")
    } else {
      arm::binnedplot(fitted(model), residuals(model, type = "response"),
                      nclass = NULL,
                      xlab = "Expected Values",
                      ylab = "Average residual",
                      main = "Binned residual plot",
                      cex.pts = 0.8,
                      col.pts = 1,
                      col.int = "gray")
    }

  })

  output$metadata_residuals3 <- renderPlot({
    response <- input$response
    req(model_metadata())

    # Extract fitted values and residuals
    model <- model_metadata()
    residuals <- resid(model)
    fitted <- fitted(model)

    if (response == "Predict Ending Wage" ||
        response == "Predict Median Difference Score") {
      # Generate residuals vs. fitted values plot
      plot(fitted, residuals,
           main = "Residuals vs Fitted",
           xlab = "Fitted Values",
           ylab = "Residuals",
           pch = 19)

      # Add a horizontal line at y = 0 for reference
      abline(h = 0, col = "steelblue", lty = 2)
    }
  })

  # # Explanation for ROC or Binned Residuals Plot
  # output$plot_explanation <- renderUI({
  #   req(model_metadata())
  #
  #   # Check the response type
  #   response <- input$response
  #
  #   if (response == "Predict Employment Outcome") {
  #     # Explanation for ROC plot
  #     HTML("<b>ROC Curve Explanation:</b> The ROC curve plots the true positive rate (sensitivity) against the false positive rate (1-specificity) at various threshold levels. The Area Under the Curve (AUC) represents the model's ability to discriminate between positive and negative outcomes. A higher AUC indicates better model performance.")
  #
  #   } else if (response == "Predict Ending Wage" || response == "Predict Median Difference Score") {
  #     # Explanation for Residuals vs. Fitted plot
  #     HTML("<b>Residuals vs Fitted Plot Explanation:</b> This plot helps assess the model fit by showing the residuals (differences between observed and predicted values) against the fitted values (predicted values). Ideally, the residuals should be randomly scattered around 0, indicating that the model's assumptions are valid.")
  #
  #   } else {
  #     # Explanation for Binned Residuals plot
  #     HTML("<b>Binned Residuals Plot Explanation:</b> This plot divides the data into bins based on fitted values, showing the average residual versus the average fitted value for each bin. It helps assess how well the model fits in different ranges of the predictor variable.")
  #   }
  # })

  # Explanation for ROC or Binned Residuals Plot
  # output$roc_explanation <- renderUI({
  #   HTML("<b>ROC Curve Explanation:</b> The ROC curve plots the true positive rate (sensitivity) against the false positive rate (1-specificity) at various threshold levels. The Area Under the Curve (AUC) represents the model's ability to discriminate between positive and negative outcomes. A higher AUC indicates better model performance.")
  # })
  # output$binned_explanation <- renderUI({
  #   HTML("<b>Binned Residuals Plot Explanation:</b> This plot divides the data into bins based on fitted values, showing the average residual versus the average fitted value for each bin. It helps assess how well the model fits in different ranges of the predictor variable.")
  # })
  # output$residuals_explanation <- renderUI({
  #   HTML("<b>Residuals vs Fitted Plot Explanation:</b> This plot helps assess the model fit by showing the residuals (differences between observed and predicted values) against the fitted values (predicted values). Ideally, the residuals should be randomly scattered around 0, indicating that the model's assumptions are valid.")
  # })


  output$roc_explanation <- renderUI({
    req(model_metadata())
    if (input$response == "Predict Employment Outcome") {
      tags$p(HTML("<b>ROC Curve Explanation:</b> The ROC curve plots the true positive rate (sensitivity) against the false positive rate (1-specificity) at various threshold levels. The Area Under the Curve (AUC) represents the model's ability to discriminate between positive and negative outcomes. A higher AUC indicates better model performance."))
    }
  })

  output$binned_explanation <- renderUI({
    req(model_metadata())
    if (input$response == "Predict Employment Outcome") {
      tags$p(HTML("<b>Binned Residuals Plot Explanation:</b> This plot divides the data into bins based on fitted values, showing the average residual versus the average fitted value for each bin. It helps assess how well the model fits in different ranges of the predictor variable. For a reasonable model, we hope to see the residuals scattered randomly around 0 with no discernible pattern. This indicates that the model's assumptions are valid and the errors are randomly distributed. If the residuals display a systematic trend (e.g., a curve or increasing/decreasing spread), it may suggest issues such as non-linearity, heteroscedasticity, or a missing predictor variable."))
    }
  })

  output$residuals_explanation <- renderUI({
    req(model_metadata())
    if (input$response == "Predict Ending Wage" || input$response == "Predict Median Difference Score") {
      tags$p(HTML("<b>Residuals vs Fitted Plot Explanation:</b> This plot helps assess the model fit by showing the residuals (differences between observed and predicted values) against the fitted values (predicted values). Ideally, the residuals should be randomly scattered around 0, indicating that the model's assumptions are valid."))
    }
  })

  output$histogram_explanation <- renderUI({
    req(model_metadata())
    if (input$response == "Predict Ending Wage" || input$response == "Predict Median Difference Score") {
      tags$p(HTML("<b>Histogram of Residuals Explanation:</b> This plot shows the distribution of the residuals from the model. It helps assess the normality of the residuals, which is an important assumption for linear regression. Ideally, the histogram should resemble a bell-shaped curve, indicating that the residuals are approximately normally distributed."))
    }
  })

  output$qqplot_explanation <- renderUI({
    req(model_metadata())  # Ensure model_scores() exists
    if (input$response == "Predict Ending Wage" || input$response == "Predict Median Difference Score") {
      tags$p(HTML("<b>QQ Plot Explanation:</b> The QQ plot is used to check if the residuals follow a normal distribution. Points should lie approximately on the diagonal line if the residuals are normally distributed. Deviations from the line suggest departures from normality."))
    }
  })


  # For ANOVA model
  output$residuals_explanation2 <- renderUI({
    req(model_scores())  # Ensure model_scores() exists
    tags$p(HTML("<b>Residuals vs Fitted Plot Explanation:</b> This plot helps assess the model fit by showing the residuals (differences between observed and predicted values) against the fitted values (predicted values). Ideally, the residuals should be randomly scattered around 0, indicating that the model's assumptions are valid."))
  })

  output$histogram_explanation2 <- renderUI({
    req(model_scores())  # Ensure model_scores() exists
    tags$p(HTML("<b>Histogram of Residuals Explanation:</b> This plot shows the distribution of the residuals from the model. It helps assess the normality of the residuals, which is an important assumption for linear regression. Ideally, the histogram should resemble a bell-shaped curve, indicating that the residuals are approximately normally distributed."))
  })

  output$qqplot_explanation2 <- renderUI({
    req(model_scores())  # Ensure model_scores() exists
    tags$p(HTML("<b>QQ Plot Explanation:</b> The QQ plot is used to check if the residuals follow a normal distribution. Points should lie approximately on the diagonal line if the residuals are normally distributed. Deviations from the line suggest departures from normality."))
  })

  # output$model_metadata_summary <- renderPrint({
  #   req(model_metadata())
  #   summary(model_metadata())
  # })


  # output$model_metadata_summary <- renderPrint({
  #   req(model_metadata())
  #
  #   # Fit the model (assuming `model_metadata()` is the result of your model fitting)
  #   model <- model_metadata()
  #
  #   # Extract the desired components
  #   coefficients <- round(coef(summary(model)), 2)
  #   sigma <- round(summary(model)$sigma, 2)
  #   r_squared <- round(summary(model)$r.squared, 2)
  #   adj_r_squared <- round(summary(model)$adj.r.squared, 2)
  #   f_stat <- round(summary(model)$fstatistic[1], 2) # F-statistic
  #
  #   # Print the extracted information
  #   cat("Coefficients:\n")
  #   print(coefficients)
  #   cat("\nResidual Standard Error (sigma):", sigma)
  #   cat("\nR-squared:", r_squared)
  #   cat("\nAdjusted R-squared:", adj_r_squared)
  #   cat("\nF-statistic:", f_stat, "\n")
  # })

  output$model_metadata_summary <- renderPrint({
    req(model_metadata())

    model <- model_metadata()

    # Check if the model is a logistic regression (glm with family binomial)
    if (inherits(model, "glm") && model$family$family == "binomial") {
      coefficients <- round(coef(summary(model)), 2)
      null_deviance <- round(model$null.deviance, 2)
      residual_deviance <- round(model$deviance, 2)
      aic <- round(AIC(model), 2)

      # Print the logistic regression summary
      cat("Logistic Regression Summary:\n")
      cat("Coefficients:\n")
      print(coefficients)
      cat("\nNull Deviance:", null_deviance)
      cat("\nResidual Deviance:", residual_deviance)
      cat("\nAIC:", aic, "\n")
    } else if (inherits(model, "lm")) {
      # Handle linear model
      coefficients <- round(coef(summary(model)), 2)
      sigma <- round(summary(model)$sigma, 2)
      r_squared <- round(summary(model)$r.squared, 2)
      adj_r_squared <- round(summary(model)$adj.r.squared, 2)
      f_stat <- round(summary(model)$fstatistic[1], 2) # F-statistic

      # Print the linear model summary
      cat("Linear Model Summary:\n")
      cat("Coefficients:\n")
      print(coefficients)
      cat("\nResidual Standard Error (sigma):", sigma)
      cat("\nR-squared:", r_squared)
      cat("\nAdjusted R-squared:", adj_r_squared)
      cat("\nF-statistic:", f_stat, "\n")
    } else {
      # Handle unsupported model types
      cat("Model type not supported for summary output.\n")
    }
  })


  # output$download_pdf <- downloadHandler(
  #   filename = function() {
  #     paste("model_results_", Sys.Date(), ".pdf", sep = "")
  #   },
  #   content = function(file) {
  #     # Create a temporary R Markdown file
  #     temp_rmd <- tempfile(fileext = ".Rmd")
  #
  #     # Create the content for the report (replace with actual dynamic content)
  #     report_content <- "
  #   ---
  #   title: 'Model Results Report'
  #   output: pdf_document
  #   ---
  #
  #   # Model Summary
  #   `r model_summary`
  #
  #   # Residuals Plots
  #   ![Residual Plot 1](residual_plot1.png)
  #   ![Residual Plot 2](residual_plot2.png)
  #
  #   # Other Plots
  #   ![Other Plot](other_plot.png)
  #   "
  #
  #     # Write the Rmd content to the temporary file
  #     writeLines(report_content, temp_rmd)
  #
  #     # Render the Rmd file to PDF
  #     rmarkdown::render(temp_rmd, output_file = file, clean = TRUE)
  #   }
  # )


  output$model_metadata_exists <- reactive({
    !is.null(model_metadata())
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
        column(12, verbatimTextOutput("model_rsa_summary")),
        column(12, plotOutput("rsa_residuals1")),
        column(12, plotOutput("rsa_residuals2")),
        column(12, plotOutput("rsa_residuals3"))
      )
    } else if (data_choice == "Use Cleaned Scores Data" ||
               (data_choice == "Upload New Dataset" &&
                input$dataset_type == "scores")) {
      fluidRow(
        # Conditionally show the caption and ANOVA results
        if (anova_run()) {
          tagList(
            fluidRow(
              column(12, tags$p("ANOVA results:",
                                style = "font-size: 14px; font-weight: bold;")),


              column(12, verbatimTextOutput("model_scores_summary")),
              column(12, downloadButton("download_model_scores_summary",
                                        "Download ANOVA Summary"))
            ),
            fluidRow(
              column(12, tags$p("Significant pairwise comparisons:",
                                style = "font-size: 14px; font-weight: bold;")),
              column(12, verbatimTextOutput("tukey_scores_summary")),
              column(12, downloadButton("download_tukey_scores_summary",
                                        "Download Pairwise Summary"))
            )
          )
        },
        fluidRow(

          # conditionalPanel(condition = "output.model_scores_exists==true" ,
          #                  uiOutput("histogram_explanation2"),
          #                  column(12, plotOutput("scores_residuals1")),
          #                  column(12, downloadButton("download_scores_residuals1",
          #                                            "Download")),
          #
          #                  uiOutput("qqplot_explanation2"),
          #                  column(12, plotOutput("scores_residuals2")),
          #                  column(12, downloadButton("download_scores_residuals2",
          #                                            "Download")),
          #
          #                  uiOutput("residuals_explanation2"),
          #                  column(12, plotOutput("scores_residuals3")),
          #                  column(12, downloadButton("download_scores_residuals3",
          #                                            "Download"))
          #                  )

          uiOutput("histogram_explanation2"),
          column(12, plotOutput("scores_residuals1")),
          # column(12, downloadButton("download_scores_residuals1",
          #                           "Download")),

          uiOutput("qqplot_explanation2"),
          column(12, plotOutput("scores_residuals2")),
          # column(12, downloadButton("download_scores_residuals2",
          #                           "Download")),

          uiOutput("residuals_explanation2"),
          column(12, plotOutput("scores_residuals3")),
          # column(12, downloadButton("download_scores_residuals3",
          #                           "Download"))
        )


      )

    } else if ((data_choice == "Use Generated Metadata") ||
               (data_choice == "Upload New Dataset" &&
                dataset_type == "metadata")) {
      # fluidRow(
      #   column(12, verbatimTextOutput("model_metadata_summary")),
      #   column(12, plotOutput("metadata_residuals1")),
      #   column(12, plotOutput("metadata_residuals2")),
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Ending Wage' || input.response === 'Predict Median Difference Score'",
      #     column(12, plotOutput("metadata_residuals3"))
      #   ))

      # # this one works so far
      # fluidRow(
      #   column(12, verbatimTextOutput("model_metadata_summary")),
      #   column(12, uiOutput("roc_explanation")),
      #   column(12, plotOutput("metadata_residuals1")),
      #   column(12, uiOutput("binned_explanation")),
      #   column(12, plotOutput("metadata_residuals2")),
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Ending Wage' || input.response === 'Predict Median Difference Score'",
      #     column(12, uiOutput("residuals_explanation")),
      #     column(12, plotOutput("metadata_residuals3"))
      #   )
      # )

      # fluidRow(
      #   column(12, verbatimTextOutput("model_metadata_summary")),
      #   column(12, uiOutput("roc_explanation")),
      #   column(12, plotOutput("metadata_residuals1")),
      #   column(12, uiOutput("binned_explanation")),
      #   column(12, plotOutput("metadata_residuals2")),
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Ending Wage' || input.response === 'Predict Median Difference Score'",
      #     column(12, uiOutput("histogram_explanation")),
      #     column(12, plotOutput("metadata_residuals1")),
      #     column(12, uiOutput("qqplot_explanation")),
      #     column(12, plotOutput("metadata_residuals2")),
      #     column(12, uiOutput("residuals_explanation")),
      #     column(12, plotOutput("metadata_residuals3")),
      #   ))

      # fluidRow(
      #   column(12, verbatimTextOutput("model_metadata_summary")),
      #
      #   # Display roc_explanation above plot1 if response is "Predict Employment Outcome"
      #   # Otherwise, display histogram_explanation above plot1
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Employment Outcome'",
      #     column(12, uiOutput("roc_explanation")),
      #   ),
      #   conditionalPanel(
      #     condition = "input.response !== 'Predict Employment Outcome'",
      #     column(12, uiOutput("histogram_explanation")),
      #   ),
      #
      #   column(12, plotOutput("metadata_residuals1")),
      #
      #   # Display binned_explanation above plot2 if response is "Predict Employment Outcome"
      #   # Otherwise, display qqplot_explanation above plot2
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Employment Outcome'",
      #     column(12, uiOutput("binned_explanation")),
      #   ),
      #   conditionalPanel(
      #     condition = "input.response !== 'Predict Employment Outcome'",
      #     column(12, uiOutput("qqplot_explanation")),
      #   ),
      #
      #   column(12, plotOutput("metadata_residuals2")),
      #
      #   # Display residuals_explanation above plot3 if response is "Predict Ending Wage" or "Predict Median Difference Score"
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Ending Wage' || input.response === 'Predict Median Difference Score'",
      #     column(12, uiOutput("residuals_explanation")),
      #     column(12, plotOutput("metadata_residuals3")),
      #   )
      # )

      # fluidRow(
      #   column(12, verbatimTextOutput("model_metadata_summary")),
      #
      #   # Plot 1 with explanation above it
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Employment Outcome'",
      #     column(12, uiOutput("roc_explanation"))
      #   ),
      #   conditionalPanel(
      #     condition = "input.response !== 'Predict Employment Outcome'",
      #     column(12, uiOutput("histogram_explanation"))
      #   ),
      #   column(12, plotOutput("metadata_residuals1")),
      #
      #   # Plot 2 with explanation above it
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Employment Outcome'",
      #     column(12, uiOutput("binned_explanation"))
      #   ),
      #   conditionalPanel(
      #     condition = "input.response !== 'Predict Employment Outcome'",
      #     column(12, uiOutput("qqplot_explanation"))
      #   ),
      #   column(12, plotOutput("metadata_residuals2")),
      #
      #   # Plot 3 with explanation above it, only for specific responses
      #   conditionalPanel(
      #     condition = "input.response === 'Predict Ending Wage' || input.response === 'Predict Median Difference Score'",
      #     column(12, uiOutput("residuals_explanation")),
      #     column(12, plotOutput("metadata_residuals3"))
      #   )
      # )
      #
      fluidRow(
        column(12, verbatimTextOutput("model_metadata_summary")),
        # conditionalPanel(
        #   condition = "output.model_run == true",
        #   column(12, downloadButton("download_model_metadata_summary", "Download Model Summary"))
        # ),

        column(12, downloadButton("download_model_metadata_summary",
                                  "Download Model Summary")),

        # Plot 1 with the appropriate explanation above it
        column(12,
               conditionalPanel(
                 condition = "input.response === 'Predict Employment Outcome'",
                 uiOutput("roc_explanation")
               ),
               conditionalPanel(
                 condition = "input.response !== 'Predict Employment Outcome'",
                 uiOutput("histogram_explanation")
               ),
               plotOutput("metadata_residuals1")
        ),
        # column(12, downloadButton("download_metadata_residuals1",
        #                           "Download")),

        # Plot 2 with the appropriate explanation above it
        column(12,
               conditionalPanel(
                 condition = "input.response === 'Predict Employment Outcome'",
                 uiOutput("binned_explanation")
               ),
               conditionalPanel(
                 condition = "input.response !== 'Predict Employment Outcome'",
                 uiOutput("qqplot_explanation")
               ),
               plotOutput("metadata_residuals2")
        ),
        # column(12, downloadButton("download_metadata_residuals2",
        #                           "Download")),

        # Plot 3 with its explanation above it, only for specific responses
        conditionalPanel(
          condition = "input.response === 'Predict Ending Wage' || input.response === 'Predict Median Difference Score'",
          column(12,
                 uiOutput("residuals_explanation"),
                 plotOutput("metadata_residuals3")
          ),
          # column(12, downloadButton("download_metadata_residuals3",
          #                           "Download"))
        )

      )




    }
  })





}

# Run the application
shinyApp(ui = ui, server = server)



