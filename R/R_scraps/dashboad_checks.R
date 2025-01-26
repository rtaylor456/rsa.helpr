conditionalPanel(
  condition = "output.rsa_data_exists == true",
  h3("Quarterly Data"),
  DTOutput("table_rsa"),
  verbatimTextOutput("summary_rsa"),
  br()
)


output$rsa_data_exists <- reactive({
  !is.null(read_and_clean_rsa_data()) &&
    nrow(read_and_clean_rsa_data()) > 0
})


output$model_scores_summary_exists <- reactive({
  !is.null(model_scores_summary())
})


output$tukey_scores_summary_exists <- reactive({
  !is.null(tukey_scores_summary())
})


output$model_metadata_summary_exists <- reactive({
  !is.null(model_metadata_summary())
})



anova_run <- reactiveVal(FALSE)

# Set anova_run to TRUE
anova_run(TRUE) #within model code
