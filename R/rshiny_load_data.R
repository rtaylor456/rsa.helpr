ui <- fluidPage(
  fileInput("file1", "Choose CSV File",
            accept = c("text/csv",
                       "text/comma-separated-values,
                       .csv"))
)

server <- function(input, output) {
  data <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath, header = TRUE)
  })
}



server <- function(input, output) {
  data <- reactive({
    url <- "http://example.com/data.csv"
    read.csv(url)
  })
}


## for large data and/or refreshing the data
server <- function(input, output) {
  data <- reactive({
    file <- "/path/to/data.csv"
    read.csv(file)
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)






############################################################################
library(shiny)
library(DT)  # for interactive tables

# Define UI
ui <- fluidPage(
  titlePanel("Large Dataset Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(".csv")
      )
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Function to read CSV file
  read_data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    return(df)
  })

  # Render the table
  output$table <- renderDT({
    datatable(read_data(),
              options = list(scrollX = TRUE)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


###########################################################
ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("files")
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10 * 1024^2)
  output$files <- renderTable(input$upload)
}

# Run the application
shinyApp(ui = ui, server = server)
############################################################

library(shiny)

# Set maximum request size (e.g., 250MB)
options(shiny.maxRequestSize = 250 * 1024^2)  # 250MB

# Define UI
ui <- fluidPage(
  fileInput("upload", label = "Upload...", multiple = FALSE),
  tableOutput("files")
)

# Define server logic
server <- function(input, output) {

  # Display uploaded file details
  output$files <- renderTable({
    req(input$upload)
    file <- input$upload
    data.frame(
      "File Name" = file$name,
      "File Size (MB)" = round(file$size / 1024^2, 2)
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)



#################################################################
library(shiny)
library(DT)  # for interactive tables

options(shiny.maxRequestSize = 250 * 1024^2)  # 250MB

# Define UI
ui <- fluidPage(
  titlePanel("Large Dataset Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(".csv")
      )
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Function to read CSV file
  read_data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    return(df)
  })

  # Render the table
  output$table <- renderDT({
    datatable(read_data(),
              options = list(scrollX = TRUE)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
