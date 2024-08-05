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
                                                 # "Use Generated Metadata",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition = "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset",
                                     accept = c(".csv")),
                           uiOutput("validation_message")
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
                         verbatimTextOutput("summary_selected_data")
                       )
              ),
              # tabPanel('Visualizations'),
              tabPanel('Visualizations',
                       sidebarPanel(
                         h4("Visualization Options"),
                         selectInput("visualization_choice",
                                     "Select Visualization",
                                     choices = c(" ",
                                                 "Demographics",
                                                 "Trends Across Grade Level",
                                                 "Trends Over Time"))
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
    dataset_type = NULL
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
    if (input$data_choice == "Upload New Dataset") {
      req(input$new_data, input$dataset_type)
      new_data <- read.csv(input$new_data$datapath)

      # Store the new dataset based on the selected type
      rv$new_data <- new_data
      rv$dataset_type <- input$dataset_type

      return(new_data)
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

  # Render status message
  output$data_status <- renderText({
    data <- selected_data()
    if (is.null(data) && input$data_choice %in% c("Use Cleaned RSA-911 Data", "Use Cleaned Scores Data", "Use Cleaned Merged Data", "Use Generated Metadata")) {
      return("No data available. Please ensure data has been cleaned or uploaded.")
    }
    return(NULL)
  })

  # output$data_select_check <- renderText({
  #   data <- selected_data()
  #   choice <- input$data_choice
  #   dataset_type <- input$dataset_type
  #   if (choice == "Upload New Data" && dataset_type == "rsa"){
  #     return("No data available. Please ensure data has been cleaned or uploaded.")
  #   }
  #   return(NULL)
  # })

  # Render visualization options UI
  # output$visualization_ui <- renderUI({
  #   if (input$visualization_choice == "Enrollment Length Histogram") {
  #     plotOutput("histogram_enrollment_length")
  #   } else {
  #     NULL
  #   }
  # })

  # Render the UI for visualization options
  # output$visualization_ui <- renderUI({
  #   if (input$data_choice %in% c("Use Cleaned Merged Data", "Use Generated Metadata")) {
  #     switch(input$visualization_choice,
  #            "Demographics" = plotOutput("plot_demographics"),
  #            "Trends Across Grade Level" = plotOutput("plot_trends_grade_level"),
  #            "Trends Over Time" = plotOutput("plot_trends_over_time"),
  #            NULL)
  #   } else {
  #     if (input$visualization_choice == "Demographics") {
  #       # plotOutput("histogram_enrollment_length")
  #     } else if (input$visualization_choice == "Trends Across Grade Level ") {
  #
  #     } else if () {
  #
  #     } else {
  #       NULL
  #     }
  #   }
  # })

  # Render histogram for enrollment length
  # output$histogram_enrollment_length <- renderPlot({
  #   data <- selected_data()
  #   if (is.null(data)) {
  #     return()
  #   }
  #   if ("Participant_ID" %in% colnames(data)) {
  #     # Ensure the enrollment length column exists
  #     if ("Enroll_Length" %in% colnames(data)) {
  #       ggplot(data, aes(x = Enroll_Length)) +
  #         geom_histogram(binwidth = 1, fill = "blue", color = "black") +
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
  # })


}

# Run the application
shinyApp(ui = ui, server = server)


