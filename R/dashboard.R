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



