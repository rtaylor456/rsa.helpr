visuals_ui <- function(input, selected_data) {
  # req(rv)
  req(input$data_choice)
  data <- selected_data()
  data_choice <- input$data_choice
  dataset_type <- input$dataset_type

  # If the user uploads a new dataset, check if it passes validation
  if (data_choice == "Upload New Dataset") {
    validation <- validate_uploaded_dataset(data, dataset_type)
    if (!validation$valid) {
      return(tags$p(validation$message, style = "color: red; font-weight: bold;"))
    }
  }

  tab_panels <- list()

  if ((data_choice == "Use Cleaned Scores Data") ||
      (data_choice == "Upload New Dataset" && dataset_type == "scores")) {

    tab_panels <- list(
      tabPanel("Overview",
               plotOutput("overview_plot1"),
               downloadButton("download_overview_plot1", "Download Plot"),
               plotOutput("overview_plot2"),
               downloadButton("download_overview_plot2", "Download Plot"),
               plotOutput("overview_plot3"),
               downloadButton("download_overview_plot3", "Download Plot")),

      tabPanel("Across Services",
               plotOutput("services_plot1"),
               downloadButton("download_services_plot1", "Download Plot"),
               plotOutput("services_plot2"),
               downloadButton("download_services_plot2", "Download Plot"),
               plotOutput("services_plot3"),
               downloadButton("download_services_plot3", "Download Plot")),

      tabPanel("Across Providers",
               plotOutput("providers_plot1"),
               downloadButton("download_providers_plot1", "Download Plot"))
    )

    if (!is.null(rv$provider_data_cleaned)) {
      tab_panels <- append(tab_panels, list(
        tabPanel("New Provider Variables",
                 plotOutput("provider_data_plot1"),
                 downloadButton("download_providers_plot1", "Download Plot"),
                 plotOutput("provider_data_plot2"),
                 downloadButton("download_providers_plot2", "Download Plot"),
                 plotOutput("provider_data_plot3"),
                 downloadButton("download_providers_plot3", "Download Plot"),
                 plotOutput("provider_data_plot4"),
                 downloadButton("download_providers_plot4", "Download Plot"),
                 plotOutput("provider_data_plot5"),
                 downloadButton("download_providers_plot5", "Download Plot"))
      ))
    }

    return(do.call(tabsetPanel, tab_panels))
  }

  # Handle the "metadata" case
  if ((data_choice == "Use Generated Metadata") ||
      (data_choice == "Upload New Dataset" && dataset_type == "metadata")) {

    return(tabsetPanel(
      tabPanel("General Demographics",
               uiOutput("social_variables_text"),
               uiOutput("gen_demo_label2"),
               tableOutput("meta_gen_demo_table"),
               uiOutput("gen_demo_label_race"),
               plotOutput("meta_gen_demo_plot4"),
               downloadButton("download_meta_gen_demo_plot4", "Download Plot"),
               uiOutput("gen_demo_label_disability"),
               plotOutput("meta_gen_demo_plot5"),
               downloadButton("download_meta_gen_demo_plot5", "Download Plot"),
               plotOutput("meta_gen_demo_plot6"),
               downloadButton("download_meta_gen_demo_plot6", "Download Plot"),
               plotOutput("meta_gen_demo_plot7"),
               downloadButton("download_meta_gen_demo_plot7", "Download Plot"),
               uiOutput("gen_demo_label1"),
               plotOutput("meta_gen_demo_plot1"),
               downloadButton("download_meta_gen_demo_plot1", "Download Plot"),
               plotOutput("meta_gen_demo_plot2"),
               downloadButton("download_meta_gen_demo_plot2", "Download Plot")
      ),
      tabPanel("Difference Scores",
               plotOutput("meta_diff_plot1"),
               downloadButton("download_meta_diff_plot1", "Download Plot"),
               plotOutput("meta_diff_plot2"),
               downloadButton("download_meta_diff_plot2", "Download Plot"),
               plotOutput("meta_diff_plot3"),
               downloadButton("download_meta_diff_plot3", "Download Plot"),
               plotOutput("meta_diff_plot4"),
               downloadButton("download_meta_diff_plot4", "Download Plot"),
               plotOutput("meta_diff_plot5"),
               downloadButton("download_meta_diff_plot5", "Download Plot"),
               plotOutput("meta_diff_plot6"),
               downloadButton("download_meta_diff_plot6", "Download Plot"),
               plotOutput("meta_diff_plot7"),
               downloadButton("download_meta_diff_plot7", "Download Plot")
      ),
      tabPanel("Wage",
               plotOutput("meta_wage_plot1"),
               downloadButton("download_meta_wage_plot1", "Download Plot"),
               plotOutput("meta_wage_plot2"),
               downloadButton("download_meta_wage_plot2", "Download Plot"),
               plotOutput("meta_wage_plot3"),
               downloadButton("download_meta_wage_plot3", "Download Plot"),
               plotOutput("meta_wage_plot4"),
               downloadButton("download_meta_wage_plot4", "Download Plot"),
               plotOutput("meta_wage_plot5"),
               downloadButton("download_meta_wage_plot5", "Download Plot"),
               plotOutput("meta_wage_plot6"),
               downloadButton("download_meta_wage_plot6", "Download Plot"),
               plotOutput("meta_wage_plot7"),
               downloadButton("download_meta_wage_plot7", "Download Plot")
      ),
      tabPanel("Employment",
               plotOutput("meta_employ_plot1"),
               downloadButton("download_meta_employ_plot1", "Download Plot"),
               plotOutput("meta_employ_plot2"),
               downloadButton("download_meta_employ_plot2", "Download Plot"),
               plotOutput("meta_employ_plot3"),
               downloadButton("download_meta_employ_plot3", "Download Plot"),
               plotOutput("meta_employ_plot4"),
               downloadButton("download_meta_employ_plot4", "Download Plot"),
               plotOutput("meta_employ_plot5"),
               downloadButton("download_meta_employ_plot5", "Download Plot"),
               plotOutput("meta_employ_plot6"),
               downloadButton("download_meta_employ_plot6", "Download Plot"),
               plotOutput("meta_employ_plot7"),
               downloadButton("download_meta_employ_plot7", "Download Plot")
      ),
      tabPanel("Post-Secondary Enrollment",
               uiOutput("post_secondary_text"),
               tableOutput("post_secondary_table")
      )
    ))
  }
}
