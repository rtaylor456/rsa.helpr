library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # RSA-911 Data Upload and Clean Options
      fluidRow(
        column(12, h4("RSA-911 Data Upload and Clean Options")),
        column(12, fileInput("rsa_data", "Choose RSA-911 CSV Files", accept = c(".csv"), multiple = TRUE)),
        column(12, checkboxInput("aggregate_utah", "Aggregate Data", value = FALSE)),
        column(12, checkboxInput("unidentified_to_0", "Convert Unidentified to 0", value = TRUE)),
        column(12, checkboxInput("convert_sex", "Convert Sex", value = TRUE)),
        column(12, checkboxInput("clean_specials", "Clean Specials", value = FALSE)),
        column(12, checkboxInput("remove_desc", "Remove Description Columns", value = TRUE)),
        column(12, checkboxInput("remove_strictly_na", "Remove Strictly NA Columns", value = TRUE))
      ),
      # Scores Data Upload and Clean Options
      fluidRow(
        column(12, h4("Scores Data Upload and Clean Options")),
        column(12, fileInput("scores_data", "Choose Scores Data File(s)", accept = c(".csv"), multiple = TRUE)),
        column(12, checkboxInput("aggregate_scores", "Aggregate Data", value = TRUE))
      ),
      # Merge Options
      fluidRow(
        column(12, h4("Merge Options")),
        column(6, textInput("quarterly_ID", "Quarterly Data ID Column", value = "Participant_ID")),
        column(6, textInput("scores_ID", "Scores Data ID Column", value = "Participant.ID"))
      ),
      # Metadata Generation
      fluidRow(
        column(12, h4("Metadata Generation")),
        column(12, actionButton("generate_metadata", "Generate Metadata"))
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
  )
)

# Define server logic
server <- function(input, output) {

  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)

    # Read all RSA-911 CSV files and combine them
    df_list <- lapply(input$rsa_data$datapath, read.csv)
    df_combined <- do.call(rbind, df_list)

    # Apply cleaning functions with user-specified arguments
    df_cleaned <- clean_utah(df_combined,
                             aggregate = input$aggregate_utah,
                             unidentified_to_0 = input$unidentified_to_0,
                             convert_sex = input$convert_sex,
                             clean_specials = input$clean_specials,
                             remove_desc = input$remove_desc,
                             remove_strictly_na = input$remove_strictly_na)

    return(df_cleaned)
  })

  # Function to read and clean scores data files
  read_and_clean_scores_data <- reactive({
    req(input$scores_data)

    # Read all scores data files and combine them
    df_scores_list <- lapply(input$scores_data$datapath, read.csv)
    df_scores_combined <- do.call(rbind, df_scores_list)

    # Apply cleaning functions with user-specified arguments
    cleaned_scores <- clean_scores(df_scores_combined,
                                   aggregate = input$aggregate_scores)

    return(cleaned_scores)
  })

  # Function to merge cleaned RSA-911 and scores data
  merged_data <- reactive({
    req(read_and_clean_rsa_data(), read_and_clean_scores_data())

    # Perform merge using merge_scores function
    merged <- merge_scores(read_and_clean_rsa_data(),
                           read_and_clean_scores_data(),
                           quarterly_ID = input$quarterly_ID,
                           scores_ID = input$scores_ID)

    return(merged)
  })

  # Function to create metadata from merged data
  generate_metadata <- eventReactive(input$generate_metadata, {
    req(merged_data())

    # Generate metadata using create_metadata function
    metadata <- create_metadata(merged_data())

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
}

# Run the application
shinyApp(ui = ui, server = server)



################################################################################

library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("Data Cleaning Dashboard"),


  sidebarLayout(

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
        # column(12, h6("Download Cleaned Data")),
        column(12, downloadButton("download_rsa", "Download Cleaned RSA-911 Data"))
      ),
      # br(),  # Add space
      tags$hr(style = "margin: 20px 0;"),  # Add larger space


      # Scores Data Upload and Clean Options
      fluidRow(
        column(12, h4("Scores Data Upload and Clean Options")),
        column(12, fileInput("scores_data", "Choose Scores Data File(s)", accept = c(".csv"), multiple = TRUE)),
        column(12, checkboxInput("aggregate_scores", "Aggregate Data", value = TRUE)),
        # column(12, h6("Download Cleaned Data")),
        column(12, downloadButton("download_scores", "Download Cleaned Scores Data"))
      ),
      # br(),  # Add space
      tags$hr(style = "margin: 20px 0;"),  # Add larger space


      # Merge Options
      fluidRow(
        column(12, h4("Merge Options")),
        column(6, textInput("quarterly_ID", "Quarterly Data ID Column", value = "Participant_ID")),
        column(6, textInput("scores_ID", "Scores Data ID Column", value = "Participant.ID")),
        # column(12, h6("Download Cleaned Data")),
        column(12, downloadButton("download_merged", "Download Cleaned Merged Data"))
      ),
      # br(),  # Add space
      tags$hr(style = "margin: 20px 0;"),  # Add larger space


      # Metadata Generation
      fluidRow(
        column(12, h4("Metadata Generation")),
        column(12, actionButton("generate_metadata", "Generate Metadata")),
        # column(12, h6("Download Cleaned Data")),
        column(12, downloadButton("download_metadata", "Download Cleaned Metadata"))
      ),
      # # Download Buttons
      # fluidRow(
      #   column(12, h4("Download Cleaned Data")),
      #   column(12, downloadButton("download_rsa", "Download RSA-911 Data")),
      #   column(12, downloadButton("download_scores", "Download Scores Data")),
      #   column(12, downloadButton("download_merged", "Download Merged Data")),
      #   column(12, downloadButton("download_metadata", "Download Metadata"))
      # )
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
  )
)

# Define server logic
server <- function(input, output) {

  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)

    # Read all RSA-911 CSV files and combine them
    df_list <- lapply(input$rsa_data$datapath, read.csv)
    df_combined <- do.call(rbind, df_list)

    # Apply cleaning functions with user-specified arguments
    df_cleaned <- clean_utah(df_combined,
                             aggregate = input$aggregate_utah,
                             unidentified_to_0 = input$unidentified_to_0,
                             convert_sex = input$convert_sex,
                             clean_specials = input$clean_specials,
                             remove_desc = input$remove_desc,
                             remove_strictly_na = input$remove_strictly_na)

    return(df_cleaned)
  })

  # Function to read and clean scores data files
  read_and_clean_scores_data <- reactive({
    req(input$scores_data)

    # Read all scores data files and combine them
    df_scores_list <- lapply(input$scores_data$datapath, read.csv)
    df_scores_combined <- do.call(rbind, df_scores_list)

    # Apply cleaning functions with user-specified arguments
    cleaned_scores <- clean_scores(df_scores_combined,
                                   aggregate = input$aggregate_scores)

    return(cleaned_scores)
  })

  # Function to merge cleaned RSA-911 and scores data
  merged_data <- reactive({
    req(read_and_clean_rsa_data(), read_and_clean_scores_data())

    # Perform merge using merge_scores function
    merged <- merge_scores(read_and_clean_rsa_data(),
                           read_and_clean_scores_data(),
                           quarterly_ID = input$quarterly_ID,
                           scores_ID = input$scores_ID)

    return(merged)
  })

  # Function to create metadata from merged data
  generate_metadata <- eventReactive(input$generate_metadata, {
    req(merged_data())

    # Generate metadata using create_metadata function
    metadata <- create_metadata(merged_data())

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

  # Download handler for RSA-911 data
  output$download_rsa <- downloadHandler(
    filename = function() {
      paste("cleaned_rsa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_rsa_data(), file, row.names = FALSE)
    }
  )

  # Download handler for scores data
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("cleaned_scores_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_scores_data(), file, row.names = FALSE)
    }
  )

  # Download handler for merged data
  output$download_merged <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  # Download handler for metadata
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(generate_metadata(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)




################################################################################

library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("Data Dashboard"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Cleaning',
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
                           # column(12, h6("Download Cleaned Data")),
                           column(12, downloadButton("download_rsa", "Download Cleaned RSA-911 Data"))
                         ),
                         # br(),  # Add space
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space


                         # Scores Data Upload and Clean Options
                         fluidRow(
                           column(12, h4("Scores Data Upload and Clean Options")),
                           column(12, fileInput("scores_data", "Choose Scores Data File(s)", accept = c(".csv"), multiple = TRUE)),
                           column(12, checkboxInput("aggregate_scores", "Aggregate Data", value = TRUE)),
                           # column(12, h6("Download Cleaned Data")),
                           column(12, downloadButton("download_scores", "Download Cleaned Scores Data"))
                         ),
                         # br(),  # Add space
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space


                         # Merge Options
                         fluidRow(
                           column(12, h4("Merge Options")),
                           column(6, textInput("quarterly_ID", "Quarterly Data ID Column", value = "Participant_ID")),
                           column(6, textInput("scores_ID", "Scores Data ID Column", value = "Participant.ID")),
                           # column(12, h6("Download Cleaned Data")),
                           column(12, downloadButton("download_merged", "Download Cleaned Merged Data"))
                         ),
                         # br(),  # Add space
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space


                         # Metadata Generation
                         fluidRow(
                           column(12, h4("Metadata Generation")),
                           column(12, actionButton("generate_metadata", "Generate Metadata")),
                           # column(12, h6("Download Cleaned Data")),
                           column(12, downloadButton("download_metadata", "Download Cleaned Metadata"))
                         ),
                         # # Download Buttons
                         # fluidRow(
                         #   column(12, h4("Download Cleaned Data")),
                         #   column(12, downloadButton("download_rsa", "Download RSA-911 Data")),
                         #   column(12, downloadButton("download_scores", "Download Scores Data")),
                         #   column(12, downloadButton("download_merged", "Download Merged Data")),
                         #   column(12, downloadButton("download_metadata", "Download Metadata"))
                         # )
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
              tabPanel('Choose/Load Data',
              ),
              tabPanel('Visualizations',
              ),
              tabPanel('Modeling')

  )
)

# Define server logic
server <- function(input, output) {

  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)

    # Read all RSA-911 CSV files and combine them
    df_list <- lapply(input$rsa_data$datapath, read.csv)
    df_combined <- do.call(rbind, df_list)

    # Apply cleaning functions with user-specified arguments
    df_cleaned <- clean_utah(df_combined,
                             aggregate = input$aggregate_utah,
                             unidentified_to_0 = input$unidentified_to_0,
                             convert_sex = input$convert_sex,
                             clean_specials = input$clean_specials,
                             remove_desc = input$remove_desc,
                             remove_strictly_na = input$remove_strictly_na)

    return(df_cleaned)
  })

  # Function to read and clean scores data files
  read_and_clean_scores_data <- reactive({
    req(input$scores_data)

    # Read all scores data files and combine them
    df_scores_list <- lapply(input$scores_data$datapath, read.csv)
    df_scores_combined <- do.call(rbind, df_scores_list)

    # Apply cleaning functions with user-specified arguments
    cleaned_scores <- clean_scores(df_scores_combined,
                                   aggregate = input$aggregate_scores)

    return(cleaned_scores)
  })

  # Function to merge cleaned RSA-911 and scores data
  merged_data <- reactive({
    req(read_and_clean_rsa_data(), read_and_clean_scores_data())

    # Perform merge using merge_scores function
    merged <- merge_scores(read_and_clean_rsa_data(),
                           read_and_clean_scores_data(),
                           quarterly_ID = input$quarterly_ID,
                           scores_ID = input$scores_ID)

    return(merged)
  })

  # Function to create metadata from merged data
  generate_metadata <- eventReactive(input$generate_metadata, {
    req(merged_data())

    # Generate metadata using create_metadata function
    metadata <- create_metadata(merged_data())

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

  # Download handler for RSA-911 data
  output$download_rsa <- downloadHandler(
    filename = function() {
      paste("cleaned_rsa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_rsa_data(), file, row.names = FALSE)
    }
  )

  # Download handler for scores data
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("cleaned_scores_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_scores_data(), file, row.names = FALSE)
    }
  )

  # Download handler for merged data
  output$download_merged <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  # Download handler for metadata
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(generate_metadata(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)


################################################################################

library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("Data Dashboard"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Cleaning',
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
              tabPanel('Choose/Load Data',
                       sidebarPanel(
                         selectInput("data_choice", "Select Data Source",
                                     choices = c("Use Cleaned RSA-911 Data",
                                                 "Use Cleaned Scores Data",
                                                 "Use Cleaned Merged Data",
                                                 "Use Generated Metadata",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition = "input.data_choice == 'Upload New Dataset' ",
                           fileInput("new_data", "Upload New Dataset", accept = c(".csv"))
                         )
                       ),
                       mainPanel(
                         DTOutput("table_selected_data"),
                         verbatimTextOutput("summary_selected_data")
                       )
              ),
              tabPanel('Visualizations'),
              tabPanel('Modeling')
  )
)

# Define server logic
server <- function(input, output) {

  # Function to read and clean RSA-911 CSV files
  read_and_clean_rsa_data <- reactive({
    req(input$rsa_data)

    # Read all RSA-911 CSV files and combine them
    df_list <- lapply(input$rsa_data$datapath, read.csv)
    df_combined <- do.call(rbind, df_list)

    # Apply cleaning functions with user-specified arguments
    df_cleaned <- clean_utah(df_combined,
                             aggregate = input$aggregate_utah,
                             unidentified_to_0 = input$unidentified_to_0,
                             convert_sex = input$convert_sex,
                             clean_specials = input$clean_specials,
                             remove_desc = input$remove_desc,
                             remove_strictly_na = input$remove_strictly_na)

    return(df_cleaned)
  })

  # Function to read and clean scores data files
  read_and_clean_scores_data <- reactive({
    req(input$scores_data)

    # Read all scores data files and combine them
    df_scores_list <- lapply(input$scores_data$datapath, read.csv)
    df_scores_combined <- do.call(rbind, df_scores_list)

    # Apply cleaning functions with user-specified arguments
    cleaned_scores <- clean_scores(df_scores_combined,
                                   aggregate = input$aggregate_scores)

    return(cleaned_scores)
  })

  # Function to merge cleaned RSA-911 and scores data
  merged_data <- reactive({
    req(read_and_clean_rsa_data(), read_and_clean_scores_data())

    # Perform merge using merge_scores function
    merged <- merge_scores(read_and_clean_rsa_data(),
                           read_and_clean_scores_data(),
                           quarterly_ID = input$quarterly_ID,
                           scores_ID = input$scores_ID)

    return(merged)
  })

  # Function to create metadata from merged data
  generate_metadata <- eventReactive(input$generate_metadata, {
    req(merged_data())

    # Generate metadata using create_metadata function
    metadata <- create_metadata(merged_data())

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

  # Download handler for RSA-911 data
  output$download_rsa <- downloadHandler(
    filename = function() {
      paste("cleaned_rsa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_rsa_data(), file, row.names = FALSE)
    }
  )

  # Download handler for scores data
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("cleaned_scores_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_scores_data(), file, row.names = FALSE)
    }
  )

  # Download handler for merged data
  output$download_merged <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  # Download handler for metadata
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(generate_metadata(), file, row.names = FALSE)
    }
  )

  # Reactive expression to handle selected data
  selected_data <- reactive({
    if (input$data_choice == "Use Generated Metadata") {
      req(generate_metadata())
      return(generate_metadata())
    } else {
      req(input$new_data)
      new_data <- read.csv(input$new_data$datapath)
      return(new_data)
    }
  })

  # Render the selected data table
  output$table_selected_data <- renderDT({
    datatable(selected_data(),
              options = list(
                scrollX = TRUE,
                title = "Selected Data"
              ))
  })

  # Render the summary for selected data
  output$summary_selected_data <- renderPrint({
    df <- selected_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)



################################################################################


library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("Data Dashboard"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Cleaning',
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
              tabPanel('Choose/Load Data',
                       sidebarPanel(
                         selectInput("data_choice", "Select Data Source",
                                     choices = c("Use Cleaned RSA-911 Data",
                                                 "Use Cleaned Scores Data",
                                                 "Use Cleaned Merged Data",
                                                 "Use Generated Metadata",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition = "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset", accept = c(".csv"))
                         )
                       ),
                       mainPanel(
                         DTOutput("table_selected_data"),
                         verbatimTextOutput("summary_selected_data")
                       )
              ),
              tabPanel('Visualizations'),
              tabPanel('Modeling')
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive values to store data
  rv <- reactiveValues(
    rsa_data_cleaned = NULL,
    scores_data_cleaned = NULL,
    merged_data = NULL,
    metadata = NULL
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

  # Download handler for RSA-911 data
  output$download_rsa <- downloadHandler(
    filename = function() {
      paste("cleaned_rsa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_rsa_data(), file, row.names = FALSE)
    }
  )

  # Download handler for scores data
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("cleaned_scores_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_scores_data(), file, row.names = FALSE)
    }
  )

  # Download handler for merged data
  output$download_merged <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  # Download handler for metadata
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(generate_metadata(), file, row.names = FALSE)
    }
  )

  # Reactive expression to handle selected data
  selected_data <- reactive({
    req(input$data_choice)

    if (input$data_choice == "Use Cleaned RSA-911 Data") {
      req(rv$rsa_data_cleaned)
      return(rv$rsa_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Scores Data") {
      req(rv$scores_data_cleaned)
      return(rv$scores_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Merged Data") {
      req(rv$merged_data)
      return(rv$merged_data)
    } else if (input$data_choice == "Use Generated Metadata") {
      req(rv$metadata)
      return(rv$metadata)
    } else if (input$data_choice == "Upload New Dataset") {
      req(input$new_data)
      new_data <- read.csv(input$new_data$datapath)
      return(new_data)
    }
  })

  # Render the selected data table
  output$table_selected_data <- renderDT({
    datatable(selected_data(),
              options = list(
                scrollX = TRUE,
                title = "Selected Data"
              ))
  })

  # Render the summary for selected data
  output$summary_selected_data <- renderPrint({
    df <- selected_data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

################################################################################

library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("Data Dashboard"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Cleaning',
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
              tabPanel('Choose/Load Data',
                       sidebarPanel(
                         selectInput("data_choice", "Select Data Source",
                                     choices = c("Use Cleaned RSA-911 Data",
                                                 "Use Cleaned Scores Data",
                                                 "Use Cleaned Merged Data",
                                                 "Use Generated Metadata",
                                                 "Upload New Dataset (csv or xlsx)")),
                         conditionalPanel(
                           condition = "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset", accept = c(".csv")),
                           radioButtons("dataset_type", "Select Dataset Type",
                                        choices = c("RSA-911" = "rsa",
                                                    "Scores" = "scores",
                                                    "Merged" = "merged",
                                                    "Metadata" = "metadata"),
                                        inline = TRUE)
                         )
                       ),
                       mainPanel(
                         DTOutput("table_selected_data"),
                         verbatimTextOutput("summary_selected_data"),
                         textOutput("data_status")  # Added for displaying messages
                       )
              ),
              tabPanel('Visualizations'),
              tabPanel('Modeling')
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive values to store data
  rv <- reactiveValues(
    rsa_data_cleaned = NULL,
    scores_data_cleaned = NULL,
    merged_data = NULL,
    metadata = NULL
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

  # Download handler for RSA-911 data
  output$download_rsa <- downloadHandler(
    filename = function() {
      paste("cleaned_rsa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_rsa_data(), file, row.names = FALSE)
    }
  )

  # Download handler for scores data
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("cleaned_scores_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_scores_data(), file, row.names = FALSE)
    }
  )

  # Download handler for merged data
  output$download_merged <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  # Download handler for metadata
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(generate_metadata(), file, row.names = FALSE)
    }
  )

  # Reactive expression to handle selected data
  selected_data <- reactive({
    req(input$data_choice)

    if (input$data_choice == "Use Cleaned RSA-911 Data") {
      if (is.null(rv$rsa_data_cleaned)) {
        return(NULL)
      }
      return(rv$rsa_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Scores Data") {
      if (is.null(rv$scores_data_cleaned)) {
        return(NULL)
      }
      return(rv$scores_data_cleaned)
    } else if (input$data_choice == "Use Cleaned Merged Data") {
      if (is.null(rv$merged_data)) {
        return(NULL)
      }
      return(rv$merged_data)
    } else if (input$data_choice == "Use Generated Metadata") {
      if (is.null(rv$metadata)) {
        return(NULL)
      }
      return(rv$metadata)
    } else if (input$data_choice == "Upload New Dataset") {
      req(input$new_data)
      new_data <- read.csv(input$new_data$datapath)
      return(new_data)
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
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
  })

  # Render status message
  output$data_status <- renderText({
    data <- selected_data()
    if (is.null(data) && input$data_choice %in% c("Use Cleaned RSA-911 Data", "Use Cleaned Scores Data", "Use Cleaned Merged Data", "Use Generated Metadata")) {
      return("No data available. Please ensure data has been cleaned or uploaded.")
    }
    return(NULL)
  })
}

# Run the application
shinyApp(ui = ui, server = server)



################################################################################

library(shiny)
library(DT)
library(data.table)
library(tidyverse)

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("Data Dashboard"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Cleaning',
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
              tabPanel('Choose/Load Data',
                       sidebarPanel(
                         selectInput("data_choice", "Select Data Source",
                                     choices = c("Use Cleaned RSA-911 Data",
                                                 "Use Cleaned Scores Data",
                                                 "Use Cleaned Merged Data",
                                                 "Use Generated Metadata",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition = "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset", accept = c(".csv"))
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
              tabPanel('Visualizations'),
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
    metadata = NULL
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

  # Download handler for RSA-911 data
  output$download_rsa <- downloadHandler(
    filename = function() {
      paste("cleaned_rsa_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_rsa_data(), file, row.names = FALSE)
    }
  )

  # Download handler for scores data
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("cleaned_scores_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(read_and_clean_scores_data(), file, row.names = FALSE)
    }
  )

  # Download handler for merged data
  output$download_merged <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )

  # Download handler for metadata
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste("metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(generate_metadata(), file, row.names = FALSE)
    }
  )

  # Reactive expression to handle selected data
  selected_data <- reactive({
    req(input$data_choice)

    if (input$data_choice == "Upload New Dataset") {
      req(input$new_data)
      dataset_type <- input$dataset_type
      new_data <- read.csv(input$new_data$datapath)

      # Handle the new dataset based on the selected type
      if (dataset_type == "rsa") {
        rv$rsa_data_cleaned <- new_data
      } else if (dataset_type == "scores") {
        rv$scores_data_cleaned <- new_data
      } else if (dataset_type == "merged") {
        rv$merged_data <- new_data
      } else if (dataset_type == "metadata") {
        rv$metadata <- new_data
      }
      return(new_data)
    } else {
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
      # No need to show file input or dataset type selection
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
    cat("Number of Rows:", n_rows, "\n")
    cat("Number of Columns:", n_cols, "\n")
  })

  # Render status message
  output$data_status <- renderText({
    data <- selected_data()
    if (is.null(data) && input$data_choice %in% c("Use Cleaned RSA-911 Data", "Use Cleaned Scores Data", "Use Cleaned Merged Data", "Use Generated Metadata")) {
      return("No data available. Please ensure data has been cleaned or uploaded.")
    }
    return(NULL)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
