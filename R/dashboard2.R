library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("RSA-911 Data Exploration"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Upload & Cleaning',
                       sidebarPanel(
                         # RSA-911 Data Upload and Clean Options
                         fluidRow(
                           column(12, h4("RSA-911 Data Upload and Clean Options")),
                           column(12, h4("(Please upload in CSV or xlsx format)")),
                           column(12, fileInput("rsa_data", "Choose RSA-911 File(s)", accept = c(".csv"), multiple = TRUE)),
                           column(12, checkboxInput("aggregate_utah", "Aggregate Data", value = FALSE)),
                           column(12, checkboxInput("unidentified_to_0", "Convert Unidentified to 0", value = TRUE)),
                           column(12, checkboxInput("convert_sex", "Convert Sex", value = TRUE)),
                           column(12, checkboxInput("clean_specials", "Clean Specials", value = FALSE)),
                           column(12, checkboxInput("remove_desc", "Remove Description Columns", value = TRUE)),
                           column(12, checkboxInput("remove_strictly_na", "Remove Strictly NA Columns", value = TRUE)),
                           column(12, downloadButton("download_rsa", "Download Cleaned RSA-911 Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Scores Data Upload and Clean Options
                         fluidRow(
                           column(12, h4("Scores Data Upload and Clean Options")),
                           column(12, fileInput("scores_data", "Choose Scores Data File(s)", accept = c(".csv"), multiple = TRUE)),
                           column(12, checkboxInput("aggregate_scores", "Aggregate Data", value = TRUE)),
                           column(12, downloadButton("download_scores", "Download Cleaned Scores Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Merge Options
                         fluidRow(
                           column(12, h4("Merge Options")),
                           column(6, textInput("quarterly_ID", "Quarterly Data ID Column", value = "Participant_ID")),
                           column(6, textInput("scores_ID", "Scores Data ID Column", value = "Participant.ID")),
                           column(12, downloadButton("download_merged", "Download Cleaned Merged Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Metadata Generation
                         fluidRow(
                           column(12, h4("Metadata Generation")),
                           column(12, actionButton("generate_metadata", "Generate Metadata")),
                           column(12, downloadButton("download_metadata", "Download Cleaned Metadata"))
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
                                     choices = c("Use Generated Metadata",
                                                 "Use Cleaned RSA-911 Data",
                                                 "Use Cleaned Scores Data",
                                                 "Use Cleaned Merged Data",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition = "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset", accept = c(".csv")),
                           radioButtons("dataset_type", "Select Dataset Type",
                                        choices = c("RSA-911" = "rsa",
                                                    "Scores" = "scores",
                                                    "Merged" = "merged",
                                                    "Metadata" = "metadata"),
                                        inline = TRUE)
                         ),
                         uiOutput("validation_message")  # Add a UI output for validation messages
                       ),
                       mainPanel(
                         uiOutput("data_ui"),  # Use uiOutput to dynamically render the content
                         DTOutput("table_selected_data"),
                         verbatimTextOutput("summary_selected_data")
                       )
              ),
              tabPanel('Visualizations',
                       sidebarPanel(
                         h4("Visualization Options"),
                         selectInput("visualization_choice", "Select Visualization",
                                     choices = c("None", "Enrollment Length Histogram"))
                       ),
                       mainPanel(
                         uiOutput("visualization_ui")
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
    validation_message = NULL  # Add a reactive value to store validation messages
  )

  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)
    df_list <- lapply(input$rsa_data$datapath, read.csv)
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
    df_scores_list <- lapply(input$scores_data$datapath, read.csv)
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
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the summary for scores data
  output$summary_scores <- renderPrint({
    df <- read_and_clean_scores_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant.ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the summary for merged data
  output$summary_merged <- renderPrint({
    df <- merged_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the summary for metadata
  output$summary_metadata <- renderPrint({
    df <- generate_metadata()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Function to validate uploaded dataset
  validate_dataset <- function(data, type) {
    if (type == "rsa") {
      if (any(grepl("score", names(data), ignore.case = TRUE))) {
        return("Error: RSA-911 data should not contain 'score' in any variable name.")
      }
      if (!any(grepl("application|gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: RSA-911 data should contain 'application', 'gender', 'plan', or 'disability' in variable names.")
      }
    } else if (type == "scores") {
      if (!any(grepl("score", names(data), ignore.case = TRUE))) {
        return("Error: Scores data should contain 'score' in variable names.")
      }
      if (any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: Scores data should not contain 'gender', 'plan', or 'disability' in variable names.")
      }
    } else if (type == "merged") {
      if (!any(grepl("score", names(data), ignore.case = TRUE)) ||
          !any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: Merged data should contain 'score' and 'gender', 'plan', or 'disability' in variable names.")
      }
    } else if (type == "metadata") {
      if (!any(grepl("score", names(data), ignore.case = TRUE)) ||
          !any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: Metadata should contain 'score' and 'gender', 'plan', or 'disability' in variable names.")
      }
      participant_ids <- unique(data$Participant_ID)
      if (length(participant_ids) != nrow(data)) {
        return("Error: Metadata should have one row per participant (unique Participant_ID).")
      }
    }
    return(NULL)
  }

  # Validate and process new dataset upload
  observeEvent(input$new_data, {
    req(input$new_data)
    data <- read.csv(input$new_data$datapath)
    validation_message <- validate_dataset(data, input$dataset_type)
    rv$validation_message <- validation_message
    if (is.null(validation_message)) {
      rv$new_data <- data
      rv$dataset_type <- input$dataset_type
    } else {
      rv$new_data <- NULL
      rv$dataset_type <- NULL
    }
  })

  # Display validation message if any
  output$validation_message <- renderUI({
    if (!is.null(rv$validation_message)) {
      div(style = "color: red;", rv$validation_message)
    }
  })

  # Render selected data UI
  output$data_ui <- renderUI({
    if (input$data_choice == "Upload New Dataset" && !is.null(rv$new_data)) {
      h3("Uploaded Data")
    } else {
      h3(input$data_choice)
    }
  })

  # Render selected data table
  output$table_selected_data <- renderDT({
    if (input$data_choice == "Upload New Dataset" && !is.null(rv$new_data)) {
      datatable(rv$new_data)
    } else if (input$data_choice == "Use Generated Metadata") {
      datatable(generate_metadata())
    } else if (input$data_choice == "Use Cleaned RSA-911 Data") {
      datatable(read_and_clean_rsa_data())
    } else if (input$data_choice == "Use Cleaned Scores Data") {
      datatable(read_and_clean_scores_data())
    } else if (input$data_choice == "Use Cleaned Merged Data") {
      datatable(merged_data())
    }
  })

  # Render selected data summary
  output$summary_selected_data <- renderPrint({
    if (input$data_choice == "Upload New Dataset" && !is.null(rv$new_data)) {
      df <- rv$new_data
    } else if (input$data_choice == "Use Generated Metadata") {
      df <- generate_metadata()
    } else if (input$data_choice == "Use Cleaned RSA-911 Data") {
      df <- read_and_clean_rsa_data()
    } else if (input$data_choice == "Use Cleaned Scores Data") {
      df <- read_and_clean_scores_data()
    } else if (input$data_choice == "Use Cleaned Merged Data") {
      df <- merged_data()
    }
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the visualization UI
  output$visualization_ui <- renderUI({
    if (input$visualization_choice == "Enrollment Length Histogram") {
      plotOutput("histogram_enrollment_length")
    }
  })

  # Render the enrollment length histogram
  output$histogram_enrollment_length <- renderPlot({
    req(input$visualization_choice == "Enrollment Length Histogram")
    metadata <- generate_metadata()
    ggplot(metadata, aes(x = enrollment_length)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "white") +
      labs(title = "Histogram of Enrollment Length",
           x = "Enrollment Length (Years)",
           y = "Frequency")
  })
}

shinyApp(ui, server)
################################################################################

library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("RSA-911 Data Exploration"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Upload & Cleaning',
                       sidebarPanel(
                         # RSA-911 Data Upload and Clean Options
                         fluidRow(
                           column(12, h4("RSA-911 Data Upload and Clean Options")),
                           column(12, h4("(Please upload in CSV or xlsx format)")),
                           column(12, fileInput("rsa_data", "Choose RSA-911 File(s)", accept = c(".csv"), multiple = TRUE)),
                           column(12, checkboxInput("aggregate_utah", "Aggregate Data", value = FALSE)),
                           column(12, checkboxInput("unidentified_to_0", "Convert Unidentified to 0", value = TRUE)),
                           column(12, checkboxInput("convert_sex", "Convert Sex", value = TRUE)),
                           column(12, checkboxInput("clean_specials", "Clean Specials", value = FALSE)),
                           column(12, checkboxInput("remove_desc", "Remove Description Columns", value = TRUE)),
                           column(12, checkboxInput("remove_strictly_na", "Remove Strictly NA Columns", value = TRUE)),
                           column(12, downloadButton("download_rsa", "Download Cleaned RSA-911 Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Scores Data Upload and Clean Options
                         fluidRow(
                           column(12, h4("Scores Data Upload and Clean Options")),
                           column(12, fileInput("scores_data", "Choose Scores Data File(s)", accept = c(".csv"), multiple = TRUE)),
                           column(12, checkboxInput("aggregate_scores", "Aggregate Data", value = TRUE)),
                           column(12, downloadButton("download_scores", "Download Cleaned Scores Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Merge Options
                         fluidRow(
                           column(12, h4("Merge Options")),
                           column(6, textInput("quarterly_ID", "Quarterly Data ID Column", value = "Participant_ID")),
                           column(6, textInput("scores_ID", "Scores Data ID Column", value = "Participant.ID")),
                           column(12, downloadButton("download_merged", "Download Cleaned Merged Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Metadata Generation
                         fluidRow(
                           column(12, h4("Metadata Generation")),
                           column(12, actionButton("generate_metadata", "Generate Metadata")),
                           column(12, downloadButton("download_metadata", "Download Cleaned Metadata"))
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
                                     choices = c("Use Generated Metadata",
                                                 "Use Cleaned RSA-911 Data",
                                                 "Use Cleaned Scores Data",
                                                 "Use Cleaned Merged Data",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition = "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset", accept = c(".csv")),
                           radioButtons("dataset_type", "Select Dataset Type",
                                        choices = c("RSA-911" = "rsa",
                                                    "Scores" = "scores",
                                                    "Merged" = "merged",
                                                    "Metadata" = "metadata"),
                                        inline = TRUE)
                         ),
                         uiOutput("validation_message")  # Add a UI output for validation messages
                       ),
                       mainPanel(
                         uiOutput("data_ui"),  # Use uiOutput to dynamically render the content
                         DTOutput("table_selected_data"),
                         verbatimTextOutput("summary_selected_data")
                       )
              ),
              tabPanel('Visualizations',
                       sidebarPanel(
                         h4("Visualization Options"),
                         selectInput("visualization_choice", "Select Visualization",
                                     choices = c("None", "Enrollment Length Histogram"))
                       ),
                       mainPanel(
                         uiOutput("visualization_ui")
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
    validation_message = NULL  # Add a reactive value to store validation messages
  )

  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)
    df_list <- lapply(input$rsa_data$datapath, read.csv)
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
    df_scores_list <- lapply(input$scores_data$datapath, read.csv)
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
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the summary for scores data
  output$summary_scores <- renderPrint({
    df <- read_and_clean_scores_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant.ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the summary for merged data
  output$summary_merged <- renderPrint({
    df <- merged_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the summary for metadata
  output$summary_metadata <- renderPrint({
    df <- generate_metadata()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Function to validate uploaded dataset
  validate_dataset <- function(data, type) {
    if (type == "rsa") {
      if (any(grepl("score", names(data), ignore.case = TRUE))) {
        return("Error: RSA-911 data should not contain 'score' in any variable name.")
      }
      if (!any(grepl("application|gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: RSA-911 data should contain 'application', 'gender', 'plan', or 'disability' in variable names.")
      }
    } else if (type == "scores") {
      if (!any(grepl("score", names(data), ignore.case = TRUE))) {
        return("Error: Scores data should contain 'score' in variable names.")
      }
      if (any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: Scores data should not contain 'gender', 'plan', or 'disability' in variable names.")
      }
    } else if (type == "merged") {
      if (!any(grepl("score", names(data), ignore.case = TRUE)) ||
          !any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: Merged data should contain 'score' and 'gender', 'plan', or 'disability' in variable names.")
      }
    } else if (type == "metadata") {
      if (!any(grepl("score", names(data), ignore.case = TRUE)) ||
          !any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
        return("Error: Metadata should contain 'score' and 'gender', 'plan', or 'disability' in variable names.")
      }
      participant_ids <- unique(data$Participant_ID)
      if (length(participant_ids) != nrow(data)) {
        return("Error: Metadata should have one row per participant (unique Participant_ID).")
      }
    }
    return(NULL)
  }

  # Validate and process new dataset upload
  observeEvent(input$new_data, {
    req(input$new_data)
    data <- read.csv(input$new_data$datapath)
    validation_message <- validate_dataset(data, input$dataset_type)
    rv$validation_message <- validation_message
    if (is.null(validation_message)) {
      rv$new_data <- data
      rv$dataset_type <- input$dataset_type
    } else {
      rv$new_data <- NULL
      rv$dataset_type <- NULL
    }
  })

  # Display validation message if any
  output$validation_message <- renderUI({
    if (!is.null(rv$validation_message)) {
      div(style = "color: red;", rv$validation_message)
    }
  })

  # Render selected data UI
  output$data_ui <- renderUI({
    if (input$data_choice == "Upload New Dataset" && !is.null(rv$new_data)) {
      h3("Uploaded Data")
    } else {
      h3(input$data_choice)
    }
  })

  # Render selected data table
  output$table_selected_data <- renderDT({
    if (input$data_choice == "Upload New Dataset" && !is.null(rv$new_data)) {
      datatable(rv$new_data)
    } else if (input$data_choice == "Use Generated Metadata") {
      datatable(generate_metadata())
    } else if (input$data_choice == "Use Cleaned RSA-911 Data") {
      datatable(read_and_clean_rsa_data())
    } else if (input$data_choice == "Use Cleaned Scores Data") {
      datatable(read_and_clean_scores_data())
    } else if (input$data_choice == "Use Cleaned Merged Data") {
      datatable(merged_data())
    }
  })

  # Render selected data summary
  output$summary_selected_data <- renderPrint({
    if (input$data_choice == "Upload New Dataset" && !is.null(rv$new_data)) {
      df <- rv$new_data
    } else if (input$data_choice == "Use Generated Metadata") {
      df <- generate_metadata()
    } else if (input$data_choice == "Use Cleaned RSA-911 Data") {
      df <- read_and_clean_rsa_data()
    } else if (input$data_choice == "Use Cleaned Scores Data") {
      df <- read_and_clean_scores_data()
    } else if (input$data_choice == "Use Cleaned Merged Data") {
      df <- merged_data()
    }
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    unique_ids <- length(unique(df$Participant_ID))
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
    cat("Unique Participant IDs:", unique_ids, "\n")
  })

  # Render the visualization UI
  output$visualization_ui <- renderUI({
    if (input$visualization_choice == "Enrollment Length Histogram") {
      plotOutput("histogram_enrollment_length")
    }
  })

  # Render the enrollment length histogram
  output$histogram_enrollment_length <- renderPlot({
    req(input$visualization_choice == "Enrollment Length Histogram")
    metadata <- generate_metadata()
    ggplot(metadata, aes(x = enrollment_length)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "white") +
      labs(title = "Histogram of Enrollment Length",
           x = "Enrollment Length (Years)",
           y = "Frequency")
  })
}

shinyApp(ui, server)



################################################################################
# Function to validate uploaded dataset
validate_dataset <- function(data, type) {
  if (type == "rsa") {
    if (any(grepl("score", names(data), ignore.case = TRUE))) {
      return("Error: RSA-911 data should not contain 'score' in any variable name.")
    }
    if (!any(grepl("application|gender|plan|disability", names(data), ignore.case = TRUE))) {
      return("Error: RSA-911 data should contain 'application', 'gender', 'plan', or 'disability' in variable names.")
    }
  } else if (type == "scores") {
    if (!any(grepl("score", names(data), ignore.case = TRUE))) {
      return("Error: Scores data should contain 'score' in variable names.")
    }
    if (any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
      return("Error: Scores data should not contain 'gender', 'plan', or 'disability' in variable names.")
    }
  } else if (type == "merged") {
    if (!any(grepl("score", names(data), ignore.case = TRUE)) ||
        !any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
      return("Error: Merged data should contain 'score' and 'gender', 'plan', or 'disability' in variable names.")
    }
  } else if (type == "metadata") {
    if (!any(grepl("score", names(data), ignore.case = TRUE)) ||
        !any(grepl("gender|plan|disability", names(data), ignore.case = TRUE))) {
      return("Error: Metadata should contain 'score' and 'gender', 'plan', or 'disability' in variable names.")
    }
    participant_ids <- unique(data$Participant_ID)
    if (length(participant_ids) != nrow(data)) {
      return("Error: Metadata should have one row per participant (unique Participant_ID).")
    }
  }
  return(NULL)
}

# Validate and process new dataset upload
observeEvent(input$new_data, {
  req(input$new_data)
  data <- read.csv(input$new_data$datapath)
  validation_message <- validate_dataset(data, input$dataset_type)
  rv$validation_message <- validation_message
  if (is.null(validation_message)) {
    rv$new_data <- data
    rv$dataset_type <- input$dataset_type
  } else {
    rv$new_data <- NULL
    rv$dataset_type <- NULL
  }
})

# Display validation message if any
output$validation_message <- renderUI({
  if (!is.null(rv$validation_message)) {
    div(style = "color: red;", rv$validation_message)
  }
})
