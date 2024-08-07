server <- function(input, output, session) {

  # Reactive expression to get the dataset
  dataset <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath)  # Modify based on your file type
  })

  # Reactive expression to check dataset type
  dataset_type <- reactive({
    # Add logic to determine the dataset type
    if ("rsa-911" %in% colnames(dataset())) {
      return("rsa-911")
    } else {
      return("other")
    }
  })

  # Render UI conditionally based on dataset type
  output$visualization_ui <- renderUI({
    if (dataset_type() == "rsa-911") {
      fluidPage(
        # Directly create visuals and captions for rsa-911 type
        plotOutput("rsa911_plot1"),
        plotOutput("rsa911_plot2"),
        # Add other visuals and captions as needed
      )
    } else {
      fluidPage(
        selectInput("visualization_type", "Select Visualization",
                    choices = c("Demographics", "Trends Across Grade Level", "Trends Over Time")),
        uiOutput("visualizations_ui")
      )
    }
  })

  # Render the visuals for rsa-911 type
  output$rsa911_plot1 <- renderPlot({
    # Add code to generate the first plot for rsa-911
  })

  output$rsa911_plot2 <- renderPlot({
    # Add code to generate the second plot for rsa-911
  })

  # Render the visuals based on selected type for other datasets
  output$visualizations_ui <- renderUI({
    switch(input$visualization_type,
           "Demographics" = plotOutput("demographics_plot"),
           "Trends Across Grade Level" = plotOutput("grade_level_trends_plot"),
           "Trends Over Time" = plotOutput("trends_over_time_plot")
    )
  })

  # Add rendering code for other plots
  output$demographics_plot <- renderPlot({
    # Add code for demographics plot
  })

  output$grade_level_trends_plot <- renderPlot({
    # Add code for trends across grade level plot
  })

  output$trends_over_time_plot <- renderPlot({
    # Add code for trends over time plot
  })
}






output$data_select_check <- renderDT({
  # Ensure new data is uploaded
  req(input$new_data)

  # Read the uploaded data
  data <- read.csv(input$new_data$datapath)

  # Check the conditions
  choice <- input$data_choice
  dataset_type <- input$dataset_type

  if (choice == "Upload New Dataset") {
    if (dataset_type == "rsa") {
      # Check for the presence of required variables and absence of excluded variables
      include_patterns <- "(?i)application|gender|sex|plan|disability"
      exclude_patterns <- "(?i)disability.*score"

      included_variables <- grep(include_patterns, names(data), value = TRUE, perl = TRUE)
      excluded_variables <- grep(exclude_patterns, included_variables, value = TRUE, perl = TRUE)

      if (length(included_variables) < 1 || length(excluded_variables) > 0) {
        return(datatable(data.frame(Message = "This does not appear to be an RSA-911 dataset. Please ensure it contains RSA-911 variables and no score variables."), options = list(scrollX = TRUE, title = "Selected Data")))
      }
    } else if (dataset_type == "merged") {
      # Check for the presence of required variables and score variables
      required_patterns <- "(?i)application|gender|sex|plan|disability"
      score_patterns <- "(?i)score"

      required_variables <- grep(required_patterns, names(data), value = TRUE, perl = TRUE)
      score_variables <- grep(score_patterns, names(data), value = TRUE, perl = TRUE)

      if (length(required_variables) < 1 || length(score_variables) < 1) {
        return(datatable(data.frame(Message = "THIS DOES NOT APPEAR TO BE A MERGED DATASET. Please ensure it is classified correctly and contains BOTH RSA-911 variables and score variables."), options = list(scrollX = TRUE, title = "Selected Data")))
      }
    } else if (dataset_type == "metadata") {
      # Check for the presence of demographic variables and score variables
      demographic_patterns <- "(?i)application|gender|sex|plan|disability"
      score_patterns <- "(?i)score"

      demographic_variables <- grep(demographic_patterns, names(data), value = TRUE, perl = TRUE)
      score_variables <- grep(score_patterns, names(data), value = TRUE, perl = TRUE)

      if (length(demographic_variables) < 1 || length(score_variables) < 1) {
        return(datatable(data.frame(Message = "THIS DOES NOT APPEAR TO BE A METADATA DATASET. Please ensure it contains both demographic and score variables."), options = list(scrollX = TRUE, title = "Selected Data")))
      }

      # Check for only one row per participant
      participant_col <- "participant_id"  # Adjust this to the actual participant column name
      if (!participant_col %in% names(data)) {
        return(datatable(data.frame(Message = paste("Participant column '", participant_col, "' not found.", sep = "")), options = list(scrollX = TRUE, title = "Selected Data")))
      }

      participant_counts <- table(data[[participant_col]])
      if (any(participant_counts > 1)) {
        return(datatable(data.frame(Message = "The dataset contains multiple rows per participant. Please ensure there is only one row per participant."), options = list(scrollX = TRUE, title = "Selected Data")))
      }
    }
  }

  datatable(data,
            options = list(
              scrollX = TRUE,
              title = "Selected Data"
            ))
})






# # Render status message
# output$data_status <- renderText({
#   data <- selected_data()
#   if (is.null(data) && input$data_choice %in% c("Use Cleaned RSA-911 Data", "Use Cleaned Scores Data", "Use Cleaned Merged Data", "Use Generated Metadata")) {
#     return("No data available. Please ensure data has been cleaned or uploaded.")
#   }
#   return(NULL)
# })

# output$data_select_check <- renderText({
#   # data <- selected_data()
#   # choice <- input$data_choice
#   # dataset_type <- input$dataset_type
#   #
#   # Ensure new data is uploaded
#   req(input$new_data)
#
#   # Read the uploaded data
#   data <- read.csv(input$new_data$datapath)
#
#   # Check the conditions
#   choice <- input$data_choice
#   dataset_type <- input$dataset_type
#
#
#   if (choice == "Upload New Data" && dataset_type == "rsa"){
#     variables_check <- grep("((?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability)(?!.*(?i)score)",
#                             names(data),
#                             value = TRUE,
#                             perl = TRUE)
#     if (length(variables_check < 1 )) {
#       return("This does not appear to be an RSA-911 dataset. Please ensure it contains RSA-911 variables and no score variables.")
#     }
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





# output$data_select_check <- renderText({
#   # Ensure new data is uploaded
#   req(input$new_data)
#
#   # Read the uploaded data
#   data <- read.csv(input$new_data$datapath)
#
#   # Check the conditions
#   choice <- input$data_choice
#   dataset_type <- input$dataset_type
#
#   if (choice == "Upload New Dataset") {
#
#     if (dataset_type == "rsa") {
#       # Check for the presence of required variables and absence of excluded variables
#       include_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
#       exclude_patterns <- "(?i)score"
#
#       included_variables <- grep(include_patterns, names(data), value = TRUE, perl = TRUE)
#       excluded_variables <- grep(exclude_patterns, included_variables, value = TRUE, perl = TRUE)
#
#       if (length(included_variables) < 1 || length(excluded_variables) > 0) {
#         return("THIS DOES NOT APPEAR TO BE AN RSA-911 DATASET. Please ensure it is classified correctly and contains RSA-911 variables and no score variables.")
#       }
#     }
#
#     else if (dataset_type == "scores") {
#       # Check for the presence of required variables and absence of excluded variables
#       include_patterns <- "(?i)score"
#       exclude_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
#
#       included_variables <- grep(include_patterns, names(data), value = TRUE, perl = TRUE)
#       excluded_variables <- grep(exclude_patterns, included_variables, value = TRUE, perl = TRUE)
#
#       if (length(included_variables) < 1 || length(excluded_variables) > 0) {
#         return("THIS DOES NOT APPEAR TO BE A SCORES DATASET. Please ensure it is classified correctly and contains score variables and no RSA-911 variables.")
#       }
#     }
#
#     else if (dataset_type == "merged") {
#       # Check for the presence of required variables and absence of excluded variables
#       demo_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
#       score_patterns <- "(?i)score"
#
#       demo_variables <- grep(demo_patterns, names(data), value = TRUE, perl = TRUE)
#       score_variables <- grep(score_patterns, names(data), value = TRUE, perl = TRUE)
#
#       if (length(demo_variables) < 1 || length(score_variables) < 1) {
#         return("THIS DOES NOT APPEAR TO BE A MERGED DATASET. Please ensure it is classified correctly and contains BOTH RSA-911 variables and score variables.")
#       }
#     }
#
#     else if (dataset_type == "metadata") {
#       # Check for the presence of required variables and absence of excluded variables
#       demo_patterns <- "(?i)application|(?i)gender|(?i)sex|(?i)plan|(?i)disability"
#       score_patterns <- "(?i)score"
#
#       demo_variables <- grep(demo_patterns, names(data), value = TRUE,
#                              perl = TRUE)
#       score_variables <- grep(score_patterns, names(data), value = TRUE,
#                               perl = TRUE)
#
#       participant_variable <- grep("(?i)participant|(?i)_ID", names(data), value = TRUE,
#                            perl = TRUE)
#
#       participants <- data[, participant_variable]
#
#       if ((length(demo_variables) < 1 || length(score_variables) < 1)) {
#         return("THIS DOES NOT APPEAR TO BE A METADATA DATASET. Please ensure it is classified correctly and contains BOTH RSA-911 variables and score variables.")
#       }
#       if (length(participants) != length(unique(participants))) {
#         return("THIS DOES NOT APPEAR TO BE A METADATA DATASET. Please ensure it is classified correctly and contains only one row per participant.")
#       }
#     }
#
#   }
#
#
# })

