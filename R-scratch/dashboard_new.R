#################################################################
library(shiny)
library(DT)  # for interactive tables

options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB

# Define UI
ui <- fluidPage(
  titlePanel("RSA-911 Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rsa_data", "Choose CSV File",
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
    req(input$rsa_data)
    df <- read.csv(input$rsa_data$datapath)
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
