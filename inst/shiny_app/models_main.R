# Define the renderUI for models_main
render_models_main <- function(input, selected_data, anova_run) {
  data_choice <- input$data_choice
  dataset_type <- input$dataset_type
  data <- selected_data()

  if (data_choice == "Upload New Dataset") {
    validation <- validate_uploaded_dataset(data, dataset_type)
    if (!validation$valid) {
      return(tags$p(validation$message, style = "color: red; font-weight: bold;"))
    }
  }

  if (data_choice == "Use Cleaned Scores Data" ||
      (data_choice == "Upload New Dataset" && input$dataset_type == "scores")) {
    fluidRow(
      # Conditionally show the caption and ANOVA results
      if (anova_run()) {
        tagList(
          fluidRow(
            column(12, tags$p("ANOVA results:", style = "font-size: 14px; font-weight: bold;")),
            column(12, verbatimTextOutput("model_scores_summary")),
            column(12, downloadButton("download_model_scores_summary", "Download ANOVA Summary"))
          ),
          fluidRow(
            column(12, tags$p("Significant pairwise comparisons:", style = "font-size: 14px; font-weight: bold;")),
            column(12, verbatimTextOutput("tukey_scores_summary")),
            column(12, downloadButton("download_tukey_scores_summary", "Download Pairwise Summary"))
          )
        )
      },
      fluidRow(
        uiOutput("histogram_explanation2"),
        column(12, plotOutput("scores_residuals1")),
        uiOutput("qqplot_explanation2"),
        column(12, plotOutput("scores_residuals2")),
        uiOutput("residuals_explanation2"),
        column(12, plotOutput("scores_residuals3"))
      )
    )
  } else if ((data_choice == "Use Generated Metadata") ||
             (data_choice == "Upload New Dataset" && input$dataset_type == "metadata")) {
    fluidRow(
      column(12, verbatimTextOutput("model_metadata_summary")),
      column(12, conditionalPanel(
        condition = "input.response === 'Predict Employment Outcome'",
        uiOutput("roc_explanation")
      )),
      column(12, conditionalPanel(
        condition = "input.response !== 'Predict Employment Outcome'",
        uiOutput("histogram_explanation")
      )),
      column(12, plotOutput("metadata_residuals1")),
      column(12, conditionalPanel(
        condition = "input.response === 'Predict Employment Outcome'",
        uiOutput("binned_explanation")
      )),
      column(12, conditionalPanel(
        condition = "input.response !== 'Predict Employment Outcome'",
        uiOutput("qqplot_explanation")
      )),
      column(12, plotOutput("metadata_residuals2")),
      conditionalPanel(
        condition = "input.response === 'Predict Ending Wage' || input.response === 'Predict Median Difference Score'",
        column(12,
               uiOutput("residuals_explanation"),
               plotOutput("metadata_residuals3")
        )
      )
    )
  }
}
