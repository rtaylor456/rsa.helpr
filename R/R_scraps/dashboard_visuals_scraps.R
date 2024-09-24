# output$meta_diff_plot4 <- renderPlot({
#   req(selected_data())
#   data <- selected_data()
#
#   # the name for the gender/sex column could be varied, so we need to
#   #   account for this possibility
#   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
#                   value = TRUE, perl = TRUE)
#
#   boxplot(Median_Difference_Score ~ data[[sex_col]], data = data,
#           main = "Difference Scores by Gender",
#           names = c("Females", "Males", "Did not identify"),
#           xlab = "Gender",
#           ylab = "Median Difference Scores",
#           col = "steelblue")
#
#   })

# output$meta_diff_plot4 <- renderPlot({
#   req(selected_data())
#   data <- selected_data()
#
#   # the name for the gender/sex column could be varied, so we need to
#   # account for this possibility
#   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
#                   value = TRUE, perl = TRUE)
#
#   # Create density for each group
#   females_density <- density(data$Median_Difference_Score[
#     data[[sex_col]] == 0], na.rm = TRUE)
#   males_density <- density(data$Median_Difference_Score[
#     data[[sex_col]] == 1], na.rm = TRUE)
#   did_not_identify_density <- density(data$Median_Difference_Score[
#     data[[sex_col]] == 9], na.rm = TRUE)
#
#   # Plot the densities
#   plot(females_density, col = "steelblue", lwd = 2,
#        main = "Density Plot of Median Difference Scores by Gender",
#        xlab = "Median Difference Scores", ylab = "Density",
#        xlim = range(females_density$x, males_density$x,
#                     did_not_identify_density$x))
#
#   lines(males_density, col = "darkblue", lwd = 2)
#
#   lines(did_not_identify_density, col = "gray", lwd = 2)
#
#   # Add a legend
#   legend("topright", legend = c("Females", "Males", "Did not identify"),
#          col = c("steelblue", "darkblue", "gray"), lwd = 2)
#   })



# output$meta_diff_plot4 <- renderPlot({
#   req(selected_data())
#   data <- selected_data()
#
#   # the name for the gender/sex column could be varied, so we need to
#   # account for this possibility
#   sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
#                   value = TRUE, perl = TRUE)
#
#   # Create density for each group, checking for sufficient data
#   females_density <- density(data$Median_Difference_Score[data[[sex_col]] == 0], na.rm = TRUE)
#   males_density <- density(data$Median_Difference_Score[data[[sex_col]] == 1], na.rm = TRUE)
#   did_not_identify_density <- density(data$Median_Difference_Score[data[[sex_col]] == 9], na.rm = TRUE)
#
#   # Initialize the plot
#   plot(females_density, col = "steelblue", lwd = 2,
#        main = "Density Plot of Median Difference Scores by Gender",
#        xlab = "Median Difference Scores", ylab = "Density",
#        xlim = range(c(females_density$x, males_density$x,
#                       did_not_identify_density$x)))
#
#   # Add the density lines for other groups
#   lines(males_density, col = "darkblue", lwd = 2)
#   lines(did_not_identify_density, col = "gray", lwd = 2)
#
#   # Determine the maximum y value for setting y limits
#   max_density_value <- max(c(max(females_density$y, na.rm = TRUE),
#                              max(males_density$y, na.rm = TRUE),
#                              max(did_not_identify_density$y, na.rm = TRUE)), na.rm = TRUE)
#
#   # Update y-limits to accommodate the peaks
#   ylim <- c(0, max_density_value * 1.1)  # Add a bit of padding
#   plot(females_density, col = "steelblue", lwd = 2,
#        main = "Density Plot of Median Difference Scores by Gender",
#        xlab = "Median Difference Scores", ylab = "Density",
#        xlim = range(c(females_density$x, males_density$x, did_not_identify_density$x)),
#        ylim = ylim)
#
#   # Re-add the lines for the densities
#   lines(males_density, col = "darkblue", lwd = 2)
#   lines(did_not_identify_density, col = "gray", lwd = 2)
#
#   # Add a legend
#   legend("topright", legend = c("Females", "Males", "Did not identify"),
#          col = c("steelblue", "darkblue", "gray"), lwd = 2)
#   })




# output$meta_diff_plot6 <- renderPlot({
#   req(selected_data())
#   data <- selected_data()
#
#   # the name for the gender/sex column could be varied, so we need to
#   #   account for this possibility
#   # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
#   #                      names(data), value = TRUE, perl = TRUE)
#   severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
#                        names(data), value = TRUE, perl = TRUE)
#
#   boxplot(Median_Difference_Score ~ data[[severity_col]], data = data,
#           main = "Difference Scores by Disability Severity",
#           names = c("Non significant", "Significant",
#                     "Most significant"),
#           xlab = "Disability Severity",
#           ylab = "Median Difference Scores",
#           col = "steelblue")
#
# })




# output$meta_wage_plot6 <- renderPlot({
#   req(selected_data())
#   data <- selected_data()
#
#   wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
#                    value = TRUE, perl = TRUE)
#   wages <- data[, .SD, .SDcols = wage_col]
#   wages_vector <- as.vector(unlist(wages))
#
#   # the name for the gender/sex column could be varied, so we need to
#   #   account for this possibility
#   # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
#   #                      names(data), value = TRUE, perl = TRUE)
#   severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
#                        names(data), value = TRUE, perl = TRUE)
#
#   boxplot(wages_vector ~ data[[severity_col]],
#           main = "Exit Wages by Disability Severity",
#           names = c("Non significant", "Significant",
#                     "Most significant"),
#           xlab = "Disability Severity",
#           ylab = "Exit Wages ($ per Hour)",
#           col = "steelblue")
#
# })



#   output$meta_wage_plot4 <- renderPlot({
#     req(selected_data())
#     data <- selected_data()
#
#     wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
#                      value = TRUE, perl = TRUE)
#     wages <- data[, .SD, .SDcols = wage_col]
#     wages_vector <- as.vector(unlist(wages))
#
#     # the name for the gender/sex column could be varied, so we need to
#     #   account for this possibility
#     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
#                     value = TRUE, perl = TRUE)
#
#     boxplot(wages_vector ~ data[[sex_col]],
#             main = "Exit Wages by Gender",
#             names = c("Females", "Males", "Did not identify"),
#             xlab = "Gender",
#             ylab = "Exit Wages ($ per Hour)",
#             col = "steelblue")
#     })



library(shiny)

ui <- fluidPage(
  titlePanel("Conditional Visualization"),

  sidebarLayout(
    sidebarPanel(
      # Input to select condition
      selectInput("condition", "Choose Condition:",
                  choices = c("Condition A", "Condition B"))
    ),

    mainPanel(
      # Conditional Panel to show dropdown or direct visual based on condition
      uiOutput("visualization_ui")
    )
  )
)

server <- function(input, output, session) {
  # Conditional UI based on the selected condition
  output$visualization_ui <- renderUI({
    if (input$condition == "Condition A") {
      # Show dropdown menu and visualization options
      tagList(
        selectInput("visual_option", "Select Visualization:",
                    choices = c("Plot 1", "Plot 2")),
        plotOutput("selected_plot")
      )
    } else if (input$condition == "Condition B") {
      # Show direct visualization
      plotOutput("direct_plot")
    }
  })

  # Render the selected plot based on the dropdown
  output$selected_plot <- renderPlot({
    if (input$visual_option == "Plot 1") {
      plot(cars)  # Replace with your Plot 1
    } else if (input$visual_option == "Plot 2") {
      hist(iris$Sepal.Length)  # Replace with your Plot 2
    }
  })

  # Render direct visualization
  output$direct_plot <- renderPlot({
    plot(iris)  # Replace with your direct visualization
  })
}

shinyApp(ui, server)



library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Conditional Visualization Across Tabs"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Set Condition",
                 selectInput("condition_select", "Select Condition:",
                             choices = c("None", "Show Plot")),
                 actionButton("apply_condition", "Apply Condition")
        ),
        tabPanel("Visualization",
                 uiOutput("dynamic_visual_ui")
        )
      )
    ),
    mainPanel(
      plotOutput("visualization_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store the condition
  rv <- reactiveValues(condition = NULL)

  observeEvent(input$apply_condition, {
    # Update the condition based on the input
    rv$condition <- input$condition_select
  })

  # Dynamically update the UI in the Visualization tab based on the condition
  output$dynamic_visual_ui <- renderUI({
    if (rv$condition == "Show Plot") {
      plotOutput("visualization_plot")
    } else {
      NULL  # No plot if condition is not met
    }
  })

  # Render the plot based on the condition
  output$visualization_plot <- renderPlot({
    if (rv$condition == "Show Plot") {
      plot(iris)  # Replace with your plot
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


##################
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


output$visualization_ui <- renderUI({
  req(input$data_choice)

  switch(input$data_choice,
         "Use Generated Metadata" = {
           # Insert UI elements for metadata visualizations
         },
         "Use Cleaned RSA-911 Data" = {
           # Insert UI elements for RSA-911 data visualizations
         },
         "Use Cleaned Scores Data" = {
           # Insert UI elements for scores data visualizations
         },
         "Use Cleaned Merged Data" = {
           # Insert UI elements for merged data visualizations
         },
         "Upload New Dataset" = {
           # Insert UI elements for new dataset visualizations
         }
  )
})


################################################################################
# sidebarPanel(
#   h4("Visualization Options"),
#   selectInput("visualization_choice",
#               "Select Visualization",
#               choices = c(" ",
#                           "Demographics",
#                           "Trends Across Grade Level",
#                           "Trends Over Time"))
# ),
# conditionalPanel(
#   condition = "input.data_choice == 'Use Cleaned RSA-911 Data'",
#   h4("Visualization Options"),
#   selectInput("visualization_choice",
#               "Select Visualization",
#               choices = c(" ",
#                           "Demographics",
#                           "Trends Across Grade Level",
#                           "Trends Over Time"))
# ),


# Render UI conditionally based on dataset type
# output$visualization_ui <- renderUI({
#   dataset_type <- input$dataset_type
#   if (dataset_type == "rsa-911") {
#     fluidPage(
#       # Directly create visuals and captions for rsa-911 type
#       plotOutput("rsa911_plot1"),
#       # plotOutput("rsa911_plot2"),
#       # Add other visuals and captions as needed
#     )
#   } else {
#     fluidPage(
#       selectInput("visualization_type", "Select Visualization",
#                   choices = c("Demographics", "Trends Across Grade Level", "Trends Over Time")),
#       uiOutput("visualizations_ui")
#     )
#   }
# })

output$visualization_ui <- renderUI({
  req(input$data_choice)
  req(input$dataset_type)


  switch(input$data_choice,
         "Use Generated Metadata" = {
           hist(iris$Sepal.Length)
         },
         "Use Cleaned RSA-911 Data" = {
           # Insert UI elements for RSA-911 data visualizations
         },
         "Use Cleaned Scores Data" = {
           # Insert UI elements for scores data visualizations
         },
         "Use Cleaned Merged Data" = {
           # Insert UI elements for merged data visualizations
         },
         "Upload New Dataset" = {
           # Insert UI elements for new dataset visualizations
         }
  )
})


# Render the visuals for rsa-911 type
output$rsa911_plot1 <- renderPlot({
  rsa <- read_and_clean_rsa_data()
  barplot(table(rsa$E9_Gender_911))
})


# output$rsa911_plot2 <- renderPlot({
#   # Add code to generate the second plot for rsa-911
# })



