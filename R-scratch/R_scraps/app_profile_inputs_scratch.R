fluidRow(
  # Metadata Profile Comparison Header
  column(12, h4("Metadata Profile Comparison Options", style = "margin-bottom: 10px;")),

  # Response Variable on its own row
  column(12,
         selectInput("response", "Select Variable of Interest",
                     choices = c(" ", "Median Difference Score",
                                 "Ending Wage", "Employment Outcome",
                                 "Post-secondary Enrollment"))
  ),

  # Profile 1 Heading (Aligned with Filters)
  fluidRow(
    column(12, h5("Profile 1 Filters", style = "margin-top: 15px;"))
  ),

  # Profile 1 Filters - Ensuring Alignment
  fluidRow(
    column(2, selectInput("gender_filter", "Gender",
                          choices = c("Male", "Female", "Other", "Did not identify"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("race_filter", "Race",
                          choices = c("Asian", "Black African", "Hawaiian Pacific Islander",
                                      "Hispanic Latino", "Indian Alaskan", "White"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("severity_filter", "Disability Severity",
                          choices = c("Non-significant", "Significant", "Most significant"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("age_group_filter", "Age Group",
                          choices = c("<5", "5-7", "8-10", "11-13", "14-16",
                                      "17-19", "20-22", "23-25", "26-30",
                                      "31-40", "41+"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("prim_impairment_filter", "Primary Impairment",
                          choices = c("Visual", "Auditory/Communicative",
                                      "Intellectual/Learning", "Physical",
                                      "Psychological", "None"),
                          multiple = TRUE, selected = NULL))
  ),

  # Comparison Options
  fluidRow(
    column(6, radioButtons("compare_method", "Comparison Method",
                           choices = c("Compare to Rest of Dataset" = "rest",
                                       "Compare to Second Profile" = "profile"),
                           selected = "rest"))
  ),

  # Conditional Panel for Profile 2 (aligned under Profile 1)
  conditionalPanel(
    condition = "input.compare_method == 'profile'",

    # Profile 2 Heading (Aligned with Filters)
    fluidRow(
      column(12, h5("Profile 2 Filters", style = "margin-top: 15px;"))
    ),

    # Profile 2 Filters - Ensuring Alignment
    fluidRow(
      column(2, selectInput("gender_filter2", "Gender",
                            choices = c("Male", "Female", "Other", "Did not identify"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("race_filter2", "Race",
                            choices = c("Asian", "Black African", "Hawaiian Pacific Islander",
                                        "Hispanic Latino", "Indian Alaskan", "White"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("severity_filter2", "Disability Severity",
                            choices = c("Non-significant", "Significant", "Most significant"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("age_group_filter2", "Age Group",
                            choices = c("<5", "5-7", "8-10", "11-13", "14-16",
                                        "17-19", "20-22", "23-25", "26-30",
                                        "31-40", "41+"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("prim_impairment_filter2", "Primary Impairment",
                            choices = c("Visual", "Auditory/Communicative",
                                        "Intellectual/Learning", "Physical",
                                        "Psychological", "None"),
                            multiple = TRUE, selected = NULL))
    )
  )
)




fluidRow(
  # Response Variable on its own row
  column(12, h4("Metadata Profile Comparison Options")),
  column(12,
         selectInput("response", "Select Variable of Interest",
                     choices = c(" ", "Median Difference Score",
                                 "Ending Wage", "Employment Outcome",
                                 "Post-secondary Enrollment"))
  ),

  # Profile 1 Filters in a single row
  column(12, tags$label("Profile Filters")),
  fluidRow(
    column(2, selectInput("gender_filter", "Gender",
                          choices = c("Male", "Female", "Other", "Did not identify"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("race_filter", "Race",
                          choices = c("Asian", "Black African", "Hawaiian Pacific Islander",
                                      "Hispanic Latino", "Indian Alaskan", "White"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("severity_filter", "Disability Severity",
                          choices = c("Non-significant", "Significant", "Most significant"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("age_group_filter", "Age Group",
                          choices = c("<5", "5-7", "8-10", "11-13", "14-16",
                                      "17-19", "20-22", "23-25", "26-30",
                                      "31-40", "41+"),
                          multiple = TRUE, selected = NULL)),
    column(2, selectInput("prim_impairment_filter", "Primary Impairment",
                          choices = c("Visual", "Auditory/Communicative",
                                      "Intellectual/Learning", "Physical",
                                      "Psychological", "None"),
                          multiple = TRUE, selected = NULL))
  ),

  # Comparison Options
  column(12, tags$label("Comparison Options")),
  column(6, radioButtons("compare_method", "Comparison Method",
                         choices = c("Compare to Rest of Dataset" = "rest",
                                     "Compare to Second Profile" = "profile"),
                         selected = "rest")),

  # Conditional Panel for Profile 2 (aligned under Profile 1)
  conditionalPanel(
    condition = "input.compare_method == 'profile'",
    column(12, h4("Define Second Profile")),
    fluidRow(
      column(2, selectInput("gender_filter2", "Gender",
                            choices = c("Male", "Female", "Other", "Did not identify"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("race_filter2", "Race",
                            choices = c("Asian", "Black African", "Hawaiian Pacific Islander",
                                        "Hispanic Latino", "Indian Alaskan", "White"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("severity_filter2", "Disability Severity",
                            choices = c("Non-significant", "Significant", "Most significant"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("age_group_filter2", "Age Group",
                            choices = c("<5", "5-7", "8-10", "11-13", "14-16",
                                        "17-19", "20-22", "23-25", "26-30",
                                        "31-40", "41+"),
                            multiple = TRUE, selected = NULL)),
      column(2, selectInput("prim_impairment_filter2", "Primary Impairment",
                            choices = c("Visual", "Auditory/Communicative",
                                        "Intellectual/Learning", "Physical",
                                        "Psychological", "None"),
                            multiple = TRUE, selected = NULL))
    )
  )
)






tagList(
  fluidRow(
    column(12, h4("Metadata Profile Comparison Options"))
  ),

  fluidRow(
    column(3, selectInput("response", "Select Variable of Interest",
                          choices = c(" ", "Median Difference Score",
                                      "Ending Wage", "Employment Outcome",
                                      "Post-secondary Enrollment"))),
    column(3, selectInput("gender_filter", "Gender",
                          choices = c("Male", "Female", "Other", "Did not identify"),
                          multiple = TRUE, selected = NULL)),
    column(3, selectInput("race_filter", "Race",
                          choices = c("Asian", "Black African", "Hawaiian Pacific Islander",
                                      "Hispanic Latino", "Indian Alaskan", "White"),
                          multiple = TRUE, selected = NULL)),
    column(3, selectInput("severity_filter", "Disability Severity",
                          choices = c("Non-significant", "Significant", "Most significant"),
                          multiple = TRUE, selected = NULL))
  ),

  fluidRow(
    column(3, selectInput("age_group_filter", "Age Group",
                          choices = c("<5", "5-7", "8-10", "11-13", "14-16",
                                      "17-19", "20-22", "23-25", "26-30",
                                      "31-40", "41+"),
                          multiple = TRUE, selected = NULL)),
    column(3, selectInput("prim_impairment_filter", "Primary Impairment Group",
                          choices = c("Visual", "Auditory/Communicative",
                                      "Intellectual/Learning", "Physical",
                                      "Psychological", "None"),
                          multiple = TRUE, selected = NULL)),
    column(3, radioButtons("compare_method", "Comparison Method",
                           choices = c("Compare to Rest of Dataset" = "rest",
                                       "Compare to Second Profile" = "profile"),
                           selected = "rest"))
  ),

  conditionalPanel(
    condition = "input.compare_method == 'profile'",
    fluidRow(
      column(12, h4("Define Second Profile"))
    ),

    fluidRow(
      column(3, selectInput("gender_filter2", "Gender",
                            choices = c("Male", "Female", "Other", "Did not identify"),
                            multiple = TRUE, selected = NULL)),
      column(3, selectInput("race_filter2", "Race",
                            choices = c("Asian", "Black African", "Hawaiian Pacific Islander",
                                        "Hispanic Latino", "Indian Alaskan", "White"),
                            multiple = TRUE, selected = NULL)),
      column(3, selectInput("severity_filter2", "Disability Severity",
                            choices = c("Non-significant", "Significant", "Most significant"),
                            multiple = TRUE, selected = NULL))
    ),

    fluidRow(
      column(3, selectInput("age_group_filter2", "Age Group",
                            choices = c("<5", "5-7", "8-10", "11-13", "14-16",
                                        "17-19", "20-22", "23-25", "26-30",
                                        "31-40", "41+"),
                            multiple = TRUE, selected = NULL)),
      column(3, selectInput("prim_impairment_filter2", "Primary Impairment Group",
                            choices = c("Visual", "Auditory/Communicative",
                                        "Intellectual/Learning", "Physical",
                                        "Psychological", "None"),
                            multiple = TRUE, selected = NULL))
    )
  )
)




output$profile_sidebar <- renderUI({
  data <- selected_data()
  data_choice <- input$data_choice
  dataset_type <- input$dataset_type


  if (data_choice == "Upload New Dataset") {
    validation <- validate_uploaded_dataset(data, dataset_type)
    if (!validation$valid) {
      return(tags$p(validation$message, style = "color: red;
                      font-weight: bold;"))
    }
  }


  if ((data_choice == "Use Cleaned Scores Data") ||
      (data_choice == "Upload New Dataset" &&
       dataset_type == "scores")) {

    # fluidRow(
    #   column(12,
    #          h4("Scores Data Modeling Options")),
    #   selectInput("anova", "Select ANOVA Test",
    #               choices = c(" ",
    #                           "ANOVA across Services",
    #                           "ANOVA across Providers")
    #   )
    # )



  } else if ((data_choice == "Use Generated Metadata") ||
             (data_choice == "Upload New Dataset" &&
              dataset_type == "metadata")) {
    fluidRow(
      column(12, h4("Metadata Profile Comparison Options")),

      # Response Variable Selector
      selectInput("response", "Select Variable of Interest",
                  choices = c(" ",
                              "Median Difference Score",
                              "Ending Wage",
                              "Employment Outcome",
                              "Post-secondary Enrollment")
      ),

      # Demographic Profile Filters
      column(12, tags$label("Profile Filters")),

      # Multi-Select Dropdowns for Profile Definition
      column(6,
             selectInput("gender_filter", "Gender",
                         choices = c("Male",
                                     "Female",
                                     "Other",
                                     "Did not identify"),
                         multiple = TRUE, selected = NULL)
      ),
      column(6,
             selectInput("race_filter", "Race",
                         choices = c("Asian",
                                     "Black African",
                                     "Hawaiian Pacific Islander",
                                     "Hispanic Latino",
                                     "Indian Alaskan",
                                     "White"),
                         multiple = TRUE, selected = NULL)
      ),
      column(6,
             selectInput("severity_filter", "Disability Severity",
                         choices = c("Non-significant",
                                     "Significant",
                                     "Most significant"),
                         multiple = TRUE, selected = NULL)
      ),
      column(6,
             selectInput("age_group_filter", "Age Group",
                         choices = c("<5",
                                     "5-7",
                                     "8-10",
                                     "11-13",
                                     "14-16",
                                     "17-19",
                                     "20-22",
                                     "23-25",
                                     "26-30",
                                     "31-40",
                                     "41+"),
                         multiple = TRUE, selected = NULL)
      ),
      column(6,
             selectInput("prim_impairment_filter", "Primary Impairment Group",
                         choices = c("Visual",
                                     "Auditory/Communicative",
                                     "Intellectual/Learning",
                                     "Physical",
                                     "Psychological",
                                     "None"),
                         multiple = TRUE, selected = NULL)
      ),

      # Comparison Options
      column(12, tags$label("Comparison Options")),
      column(6,
             radioButtons("compare_method", "Comparison Method",
                          choices = c("Compare to Rest of Dataset" = "rest",
                                      "Compare to Second Profile" = "profile"),
                          selected = "rest")
      ),

      # Conditional Panel to Show Second Profile Selection Only If Needed
      conditionalPanel(
        condition = "input.compare_method == 'profile'",
        fluidRow(
          column(12, h4("Define Second Profile")),
          # Multi-Select Dropdowns for Profile Definition
          column(6,
                 selectInput("gender_filter2", "Gender",
                             choices = c("Male",
                                         "Female",
                                         "Other",
                                         "Did not identify"),
                             multiple = TRUE, selected = NULL)
          ),
          column(6,
                 selectInput("race_filter2", "Race",
                             choices = c("Asian",
                                         "Black African",
                                         "Hawaiian Pacific Islander",
                                         "Hispanic Latino",
                                         "Indian Alaskan",
                                         "White"),
                             multiple = TRUE, selected = NULL)
          ),
          column(6,
                 selectInput("severity_filter2", "Disability Severity",
                             choices = c("Non-significant",
                                         "Significant",
                                         "Most significant"),
                             multiple = TRUE, selected = NULL)
          ),
          column(6,
                 selectInput("age_group_filter2", "Age Group",
                             choices = c("<5",
                                         "5-7",
                                         "8-10",
                                         "11-13",
                                         "14-16",
                                         "17-19",
                                         "20-22",
                                         "23-25",
                                         "26-30",
                                         "31-40",
                                         "41+"),
                             multiple = TRUE, selected = NULL)
          ),
          column(6,
                 selectInput("prim_impairment_filter2",
                             "Primary Impairment Group",
                             choices = c("Visual",
                                         "Auditory/Communicative",
                                         "Intellectual/Learning",
                                         "Physical",
                                         "Psychological",
                                         "None"),
                             multiple = TRUE, selected = NULL)
          )
        )
      )
    )

  }

})


