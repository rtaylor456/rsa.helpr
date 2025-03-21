# Verify lengths

# Number of rows in data
num_rows <- nrow(scores_clean3)

# Replicate the services correctly
replication_times <- length(difference_cols)


length_diff_values <- length(c(scores_clean3$Difference_CPSO, scores_clean3$Difference_CSS, scores_clean3$Difference_EMP, scores_clean3$Difference_FL,
                               scores_clean3$Difference_ILOM, scores_clean3$Difference_ISA, scores_clean3$Difference_JOBEX, scores_clean3$Difference_JS,
                               scores_clean3$Difference_QWEX, scores_clean3$Difference_WBLE))

length_services <- length(rep(difference_cols, each = num_rows))



# output$scores_model <- reactive({
#   req(selected_data())
#   data <- selected_data()
#
#   if (input$anova == "ANOVA across Services") {
#     # Assuming `data` is the data table and "Service" is a column
#     anova_result <- aov(Scores ~ Service, data = data)
#
#     output$anova_output <- renderPrint({
#       summary(anova_result)
#     })
#
#   } else if (input$anova == "ANOVA across Providers") {
#     # Assuming `data` is the data table and "Provider" is a column
#     anova_result <- aov(Median_Difference_Score ~ Provider,
#                         data = data)
#
#     output$anova_output <- renderPrint({
#       print(anova_result)
#       summary(anova_result)
#     })
#   }
#
# })


# Define the ANOVA model as a reactive expression
scores_model <- reactive({
  req(selected_data())
  data <- selected_data()

  if (input$anova == "ANOVA across Services") {
    anova_result <- aov(Scores ~ Service, data = data)
  } else if (input$anova == "ANOVA across Providers") {
    anova_result <- aov(Median_Difference_Score ~ Provider, data = data)
  }

  return(anova_result)
})

# Render the ANOVA results in the UI
output$anova_result <- renderPrint({
  req(scores_model())  # Use the reactive expression correctly
  summary(scores_model())
})
