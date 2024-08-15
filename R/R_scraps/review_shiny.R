library(shiny)

# fix the code chunks
ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)
#
# server1 <- function(input, output, server) {
#   output$greeting <- renderText(paste0("Hello ", input$name))
# }
# shinyApp(ui = ui, server = server1)

# server2 <- function(input, output, server) {
#   greeting <- reactive(paste0("Hello ", input$name))
#   output$greeting <- renderText(greeting())
# }
# shinyApp(ui = ui, server = server2)

# server3 <- function(input, output, server) {
#   output$greeting <- reactive(paste0("Hello", input$name))
# }
# shinyApp(ui = ui, server = server3)

################################################################################

library(shiny)
# draw the reactive graphs

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server1 <- function(input, output, session) {
  c <- reactive(input$a + input$b)
  e <- reactive(c() + input$d)
  output$f <- renderText(e())
}

# a, b --> c -> c + d --> e --> f

server2 <- function(input, output, session) {
  x <- reactive(input$x1 + input$x2 + input$x3)
  y <- reactive(input$y1 + input$y2)
  output$z <- renderText(x() / y())
}

# x1, x2, x3 --> x --> z
# y1 + y2 --> y --> z

server3 <- function(input, output, session) {
  d <- reactive(c() ^ input$d)
  a <- reactive(input$a * 10)
  c <- reactive(b() / input$c)
  b <- reactive(a() + input$b)
}

# a --> b --> c --> d
