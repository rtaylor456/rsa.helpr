library(shiny)
library(DT)
library(data.table)
library(readxl)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("RSA-911 Data Exploration"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Upload & Cleaning',
                       sidebarPanel(
                         # RSA-911 Data Upload and Clean Options
                         fluidRow(
                           column(12,
                                  h4("RSA-911 Data Upload and Clean Options")),
                           column(12,
                                  h4("(Please upload in CSV or xlsx format)")),
                           column(12,
                                  fileInput("rsa_data",
                                            "Choose RSA-911 File(s)",
                                            accept = c(".csv", ".xlsx"),
                                            multiple = TRUE)),
                           column(12,
                                  checkboxInput("aggregate_utah",
                                                "Aggregate Data",
                                                value = FALSE)),
                           column(12,
                                  checkboxInput("unidentified_to_0",
                                                "Convert Unidentified to 0",
                                                value = TRUE)),
                           column(12,
                                  checkboxInput("convert_sex", "Convert Sex",
                                                value = TRUE)),
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
                           column(12, h4("Merge Options")),
                           column(6,
                                  textInput("quarterly_ID",
                                            "Quarterly Data ID Column",
                                            value = "Participant_ID")),
                           column(6,
                                  textInput("scores_ID",
                                            "Scores Data ID Column",
                                            value = "Participant.ID")),
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
                           condition = "input.data_choice == 'Upload New Dataset'",
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
                         uiOutput("data_ui"),  # Use uiOutput to dynamically render the content
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

              tabPanel('Modeling')
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

  # Helper function to read and combine multiple files
  read_and_combine_files <- function(file_paths, file_type) {
    if (file_type == "csv") {
      df_list <- lapply(file_paths, read.csv,
                        stringsAsFactors = FALSE, row.names = NULL)
    } else if (file_type == "xlsx") {
      df_list <- lapply(file_paths, read_excel)
    }
    df_combined <- do.call(rbind, df_list)
    return(df_combined)
  }


  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)

    file_paths <- input$rsa_data$datapath
    file_types <- tools::file_ext(input$rsa_data$name)

    df_list <- mapply(function(path, type) {
      if (type == "csv") {
        read.csv(path, stringsAsFactors = FALSE, row.names = NULL)
      } else {
        read_excel(path)
      }
    }, file_paths, file_types, SIMPLIFY = FALSE)

    # df_list <- lapply(input$rsa_data$datapath, read.csv)

    df_combined <- do.call(rbind, df_list)
    df_cleaned <- clean_utah(df_combined,
                             aggregate = input$aggregate_utah,
                             unidentified_to_0 = input$unidentified_to_0,
                             convert_sex = input$convert_sex,
                             clean_specials = input$clean_specials,
                             remove_desc = input$remove_desc,
                             remove_strictly_na = input$remove_strictly_na)
    rv$rsa_data_cleaned <- df_cleaned
    return(df_cleaned)
  })

  # Function to read and clean scores data files
  read_and_clean_scores_data <- reactive({
    req(input$scores_data)
    file_paths <- input$scores_data$datapath
    file_types <- tools::file_ext(input$scores_data$name)

    df_scores_list <- mapply(function(path, type) {
      if (type == "csv") {
        read.csv(path, stringsAsFactors = FALSE, row.names = NULL)
      } else {
        read_excel(path, row.names = NULL)
      }
    }, file_paths, file_types, SIMPLIFY = FALSE)


    # df_scores_list <- lapply(input$scores_data$datapath, read.csv)

    df_scores_combined <- do.call(rbind, df_scores_list)
    cleaned_scores <- clean_scores(df_scores_combined,
                                   aggregate = input$aggregate_scores)
    rv$scores_data_cleaned <- cleaned_scores
    return(cleaned_scores)
  })

  # Function to merge cleaned RSA-911 and scores data
  merged_data <- reactive({
    req(read_and_clean_rsa_data(), read_and_clean_scores_data())
    merged <- merge_scores(read_and_clean_rsa_data(),
                           read_and_clean_scores_data(),
                           quarterly_ID = input$quarterly_ID,
                           scores_ID = input$scores_ID)
    rv$merged_data <- merged
    return(merged)
  })

  # Function to create metadata from merged data
  generate_metadata <- eventReactive(input$generate_metadata, {
    req(merged_data())
    metadata <- create_metadata(merged_data())
    rv$metadata <- metadata
    return(metadata)
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
    unique_ids <- length(unique(df$Participant.ID))
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
    if (input$data_choice == "Use Cleaned RSA-911 Data" && !is.null(rv$rsa_data_cleaned)) {
      return(rv$rsa_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Scores Data" && !is.null(rv$scores_data_cleaned)) {
      return(rv$scores_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Merged Data" && !is.null(rv$merged_data)) {
      return(rv$merged_data)
    } else if (input$data_choice == "Use Generated Metadata" && !is.null(rv$metadata)) {
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
      if (input$data_choice == "Use Cleaned RSA-911 Data" && is.null(rv$rsa_data_cleaned)) {
        return(NULL)
      } else if (input$data_choice == "Use Cleaned Scores Data" && is.null(rv$scores_data_cleaned)) {
        return(NULL)
      } else if (input$data_choice == "Use Cleaned Merged Data" && is.null(rv$merged_data)) {
        return(NULL)
      } else if (input$data_choice == "Use Generated Metadata" && is.null(rv$metadata)) {
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

        included_variables <- grep(include_patterns, names(data), value = TRUE, perl = TRUE)
        excluded_variables <- grep(exclude_patterns, names(data), value = TRUE, perl = TRUE)

        if (length(included_variables) < 1 || length(excluded_variables) > 0) {
          return("THIS DOES NOT APPEAR TO BE AN RSA-911 DATASET. Please ensure it is classified correctly and contains RSA-911 variables and no score variables.")
        }
      }

      else if (dataset_type == "scores") {
        # Check for the presence of required variables and absence of excluded variables
        include_patterns <- "(?i)score"
        exclude_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"

        included_variables <- grep(include_patterns, names(data), value = TRUE, perl = TRUE)
        excluded_variables <- grep(exclude_patterns, included_variables, value = TRUE, perl = TRUE)

        if (length(included_variables) < 1 || length(excluded_variables) > 0) {
          return("THIS DOES NOT APPEAR TO BE A SCORES DATASET. Please ensure it is classified correctly and contains score variables and no RSA-911 variables.")
        }
      }

      else if (dataset_type == "merged") {
        # Check for the presence of required variables and absence of excluded variables
        demo_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
        score_patterns <- "(?i)score"

        demo_variables <- grep(demo_patterns, names(data), value = TRUE, perl = TRUE)
        score_variables <- grep(score_patterns, names(data), value = TRUE, perl = TRUE)

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

        demo_variables <- grep(demo_patterns, names(data), value = TRUE, perl = TRUE)
        score_variables <- grep(score_patterns, names(data), value = TRUE, perl = TRUE)

        participant_variable <- grep("(?i)participant|(?i)_ID", names(data), value = TRUE, perl = TRUE)

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

    # Print debugging information
    print(paste("Data Choice:", data_choice))
    print(paste("Dataset Type:", dataset_type))

    if ((data_choice == "Use Cleaned RSA-911 Data") || (data_choice == "Upload New Dataset" && dataset_type == "rsa")) {
      tabsetPanel(
        tabPanel("Demographics",
                 plotOutput("demographics_plot1"), plotOutput("demographics_plot2"),
                 plotOutput("demographics_plot3"), plotOutput("demographics_plot4")),
        tabPanel("Enrollment Length",
                 plotOutput("enrollment_plot1"), plotOutput("enrollment_plot2"),
                 plotOutput("enrollment_plot3"))
      )
    } else if ((data_choice == "Use Cleaned Scores Data") || (data_choice == "Upload New Dataset" && dataset_type == "scores")) {
      tabsetPanel(
        tabPanel("Across Services",
                 plotOutput("services_plot1"), plotOutput("services_plot2")),
        tabPanel("Across Providers",
                 plotOutput("providers_plot1"), plotOutput("providers_plot2"))
      )
    } else if ((data_choice == "Use Cleaned Merged Data") || (data_choice == "Upload New Dataset" && dataset_type == "merged")) {
      tabsetPanel(
        tabPanel("General Demographics",
                 plotOutput("gen_demo_plot1"), plotOutput("gen_demo_plot2"),
                 plotOutput("gen_demo_plot3"), plotOutput("gen_demo_plot4"), plotOutput("gen_demo_plot5")),
        tabPanel("Demographics & Scores",
                 plotOutput("demo_scores_plot1"), plotOutput("demo_scores_plot2"),
                 plotOutput("demo_scores_plot3"), plotOutput("demo_scores_plot4"),
                 plotOutput("demo_scores_plot5"), plotOutput("demo_scores_plot6"))
      )
    }  else if ((data_choice == "Use Generated Metadata") || (data_choice == "Upload New Dataset" && dataset_type == "metadata")) {
      tabsetPanel(
        tabPanel("General Demographics",
                 plotOutput("meta_gen_demo_plot1"), plotOutput("meta_gen_demo_plot2"),
                 plotOutput("meta_gen_demo_plot3"), plotOutput("meta_gen_demo_plot4"), plotOutput("meta_gen_demo_plot5")),
        tabPanel("Demographics & Scores",
                 plotOutput("meta_demo_scores_plot1"), plotOutput("meta_demo_scores_plot2"),
                 plotOutput("meta_demo_scores_plot3"), plotOutput("meta_demo_scores_plot4"),
                 plotOutput("meta_demo_scores_plot5"), plotOutput("meta_demo_scores_plot6"))
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
  output$services_plot1 <- renderPlot({ plot(rnorm(100)) })
  # output$services_plot1 <- renderPlot({
  # })
  output$services_plot2 <- renderPlot({ plot(rnorm(100)) })

  output$providers_plot1 <- renderPlot({ plot(rnorm(100)) })
  output$providers_plot2 <- renderPlot({ plot(rnorm(100)) })

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
  output$meta_gen_demo_plot1 <- renderPlot({ plot(rnorm(100)) })
  # output$meta_gen_demo_plot1 <- renderPlot({
  #   data <- input$new_data
  #   hist(data$Enroll_Length)
  #   })

  output$meta_gen_demo_plot2 <- renderPlot({ plot(rnorm(100)) })
  output$meta_gen_demo_plot3 <- renderPlot({ plot(rnorm(100)) })
  output$meta_gen_demo_plot4 <- renderPlot({ plot(rnorm(100)) })
  output$meta_gen_demo_plot5 <- renderPlot({ plot(rnorm(100)) })

  output$meta_demo_scores_plot1 <- renderPlot({ plot(rnorm(100)) })
  output$meta_demo_scores_plot2 <- renderPlot({ plot(rnorm(100)) })
  output$meta_demo_scores_plot3 <- renderPlot({ plot(rnorm(100)) })
  output$meta_demo_scores_plot4 <- renderPlot({ plot(rnorm(100)) })
  output$meta_demo_scores_plot5 <- renderPlot({ plot(rnorm(100)) })
  output$meta_demo_scores_plot6 <- renderPlot({ plot(rnorm(100)) })

}

# Run the application
shinyApp(ui = ui, server = server)
