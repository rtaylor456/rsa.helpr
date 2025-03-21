# Define the renderUI for models_sidebar
render_models_sidebar <- function(input, selected_data) {
  data <- selected_data()
  data_choice <- input$data_choice
  dataset_type <- input$dataset_type

  if (data_choice == "Upload New Dataset") {
    validation <- validate_uploaded_dataset(data, dataset_type)
    if (!validation$valid) {
      return(tags$p(validation$message, style = "color: red; font-weight: bold;"))
    }
  }

  if ((data_choice == "Use Cleaned Scores Data") ||
      (data_choice == "Upload New Dataset" && dataset_type == "scores")) {

    return(fluidRow(
      column(12, h4("Scores Data Modeling Options")),
      selectInput("anova", "Select ANOVA Test",
                  choices = c(" ",
                              "ANOVA across Services",
                              "ANOVA across Providers"))
    ))
  } else if ((data_choice == "Use Generated Metadata") ||
             (data_choice == "Upload New Dataset" && dataset_type == "metadata")) {
    return(fluidRow(
      column(12, h4("Metadata Modeling Options")),
      selectInput("response", "Select Response Variable",
                  choices = c(" ",
                              "Predict Median Difference Score",
                              "Predict Ending Wage",
                              "Predict Employment Outcome")),
      column(12, tags$label("Select Predictor Variables:")),
      column(12, checkboxInput("gender", "Gender", value = FALSE)),
      column(12, checkboxInput("race", "Race", value = FALSE)),
      column(12, checkboxInput("severity", "Severity", value = FALSE)),
      column(12, checkboxInput("enroll_length", "Enrollment Length", value = FALSE)),
      column(12, checkboxInput("prim_impairment", "Primary Impairment", value = FALSE)),
      column(12, checkboxInput("second_impairment", "Secondary Impairment", value = FALSE))
      # Uncomment and add interaction options if needed
      # column(12, checkboxInput("interactions", "Interaction Effects", value = FALSE))
    ))
  }
}
