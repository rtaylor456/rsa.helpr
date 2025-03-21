source("visuals_ui.R")
source("models_sidebar.R")
source("models_main.R")


ui <- fluidPage(

  # to change/play around with display themes
  # shinythemes::themeSelector(),
  # theme = "lumen",

  # to change location of progress bars:
  tags$head(tags$style(".shiny-notification {position: fixed; top: 50% ;left: 50%")),

  titlePanel("RSA-911 Data Exploration"),

  tabsetPanel(type = 'tabs',
              tabPanel('Data Upload & Cleaning',
                       sidebarPanel(
                         fluidRow(
                           column(12,
                                  h4("Please upload in .csv format.")),
                           column(12,
                                  h6("(.xlsx files are accepted, but will take longer.)"))
                         ),

                         tags$hr(style = "margin: 20px 0;"),

                         # RSA-911 Data Upload and Clean Options
                         fluidRow(

                           column(12,
                                  h4("RSA-911 Data Upload and Clean Options")),

                           column(12,
                                  fileInput("rsa_data",
                                            "Choose RSA-911 File(s)",
                                            accept = c(".csv", ".xlsx"),
                                            multiple = TRUE)),
                           column(12,
                                  checkboxInput("aggregate_utah",
                                                "Aggregate Data",
                                                value = TRUE)),
                           column(12,
                                  checkboxInput("unidentified_to_0",
                                                "Convert Unidentified to 0",
                                                value = TRUE)),

                           column(12,
                                  checkboxInput("remove_desc",
                                                "Remove Description Columns",
                                                value = TRUE)),
                           column(12,
                                  checkboxInput("remove_strictly_na",
                                                "Remove Strictly NA Columns",
                                                value = TRUE)),

                           column(12,
                                  textInput("clean_specials",
                                            "Clean Specials (Enter variable names separated by commas)",
                                            placeholder = "e.g., var1, var2, var3")),

                           column(12,
                                  downloadButton("download_rsa",
                                                 "Download Cleaned RSA-911 Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Scores Data Upload and Clean Options
                         fluidRow(
                           column(12,
                                  h4("Scores Data Upload and Clean Options")),
                           column(12,
                                  fileInput("scores_data",
                                            "Choose Scores Data File(s)",
                                            accept = c(".csv", ".xlsx"),
                                            multiple = TRUE)),
                           column(12,
                                  checkboxInput("aggregate_scores",
                                                "Aggregate Data",
                                                value = TRUE)),

                           column(12,
                                  checkboxInput("clean_ID",
                                                "Remove rows with missing IDs",
                                                value = TRUE)),


                           column(12,
                                  textInput("state_filter",
                                            "State(s) of interest (If multiple states, enter names separated by commas)",
                                            placeholder = "e.g., Utah. e.g., Utah, Colorado, Idaho",
                                            value = "Utah")
                           ),


                           column(12,
                                  textInput("ID_col",
                                            "If the variable naming structure for participant ID, please enter here",
                                            placeholder = "e.g., X")),


                           column(12,
                                  downloadButton("download_scores",
                                                 "Download Cleaned Scores Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Merge Options
                         fluidRow(
                           column(12, h4("RSA-911 and Scores Merge Options")),
                           column(6,
                                  textInput("quarterly_ID",
                                            "Quarterly Data ID Column",
                                            value = "Participant_ID")),
                           column(6,
                                  textInput("scores_ID",
                                            "Scores Data ID Column",
                                            value = "Participant_ID")),
                           column(12,
                                  downloadButton("download_merged",
                                                 "Download Cleaned Merged Data"))
                         ),
                         tags$hr(style = "margin: 20px 0;"),  # Add larger space

                         # Metadata Generation
                         fluidRow(
                           column(12,
                                  h4("Metadata Generation")),
                           column(12,
                                  conditionalPanel(
                                    condition =
                                      "output.merged_data_exists == true",
                                    # checkboxInput("use_merged",
                                    #               "(Use Merged Data)",
                                    #               value = TRUE)
                                    radioButtons("meta_source",
                                                 label = NULL,
                                                 choices = list("Using Merged Data" = "merged",
                                                                "Using Cleaned RSA-911 Data" = "rsa"),
                                                 selected = "merged")
                                  )
                           ),
                           column(12,
                                  actionButton("generate_metadata",
                                               "Generate Metadata")),
                           column(12,
                                  downloadButton("download_metadata",
                                                 "Download Cleaned Metadata"))
                         )
                       ),

                       mainPanel(
                         # Quarterly Data
                         conditionalPanel(
                           condition = "output.rsa_data_exists == true",
                           h3("Quarterly Data"),
                           DTOutput("table_rsa"),
                           verbatimTextOutput("summary_rsa"),
                           br()
                         ),

                         # Scores Data
                         conditionalPanel(
                           condition = "output.scores_data_exists == true",
                           h3("Scores Data"),
                           DTOutput("table_scores"),
                           verbatimTextOutput("summary_scores"),
                           br()
                         ),

                         # Merged Quarterly and Scores Data
                         conditionalPanel(
                           condition = "output.merged_data_exists == true",
                           h3("Merged Quarterly and Scores Data"),
                           DTOutput("table_merged"),
                           verbatimTextOutput("summary_merged"),
                           br()
                         ),

                         # Generated Metadata
                         conditionalPanel(
                           condition = "output.metadata_exists == true",
                           h3("Generated Metadata"),
                           DTOutput("table_metadata"),
                           verbatimTextOutput("summary_metadata")
                         )
                       )
              ),
              tabPanel('Select Data (or load new)',
                       sidebarPanel(
                         selectInput("data_choice", "Select Data Source",
                                     choices = c(" ",
                                                 "Use Generated Metadata",
                                                 "Use Cleaned Scores Data",
                                                 # "Use Cleaned RSA-911 Data",
                                                 # "Use Cleaned Merged Data",
                                                 "Upload New Dataset")),
                         conditionalPanel(
                           condition =
                             "input.data_choice == 'Upload New Dataset'",
                           fileInput("new_data", "Upload New Dataset",
                                     accept = c(".csv", ".xlsx")),
                           # uiOutput("validation_message"),
                           uiOutput("data_select_check")
                           # radioButtons("dataset_type", "Select Dataset Type",
                           #              choices = c("RSA-911" = "rsa",
                           #                          "Scores" = "scores",
                           #                          "Merged" = "merged",
                           #                          "Metadata" = "metadata"),
                           #              inline = TRUE)
                         )
                       ),
                       mainPanel(
                         uiOutput("data_ui"),
                         # Use uiOutput to dynamically render the content
                         DTOutput("table_selected_data"),
                         verbatimTextOutput("summary_selected_data"),
                         uiOutput("validation_message")
                         # uiOutput("data_select_check")
                       )
              ),
              tabPanel('Data Exploration',
                       mainPanel(
                         uiOutput("visuals_ui")
                       )
              ),


              tabPanel('Modeling',
                       sidebarPanel(
                         uiOutput("models_sidebar")
                       ),
                       mainPanel(
                         uiOutput("models_main")
                       )
              )
  )
)
