#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(bslib)

# UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  titlePanel("Calculating Probability Using Z-scores"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("z1", "Lower Z-Score:", value = 0, step = 0.1),
      numericInput("z2", "Upper Z-Score:", value = 1.0, step = 0.1),
      actionButton("calculate", "Calculate Probability")
    ),
    
    mainPanel(
      plotOutput("plot"),
      textOutput("description"),
      textOutput("probability")
    )
  )
)

# Server
server <- function(input, output) {
  
  observeEvent(input$calculate, {
    z1 <- input$z1
    z2 <- input$z2
    
    # Calculate the probability
    prob <- pnorm(z2) - pnorm(z1)
    
    output$description <- renderText({
      paste("Probability = Area Under The Curve")
    })
    
    # Store the probability for rendering
    output$probability <- renderText({
      paste("Probability between Z =", z1, "and Z =", z2, "is:", round(prob, 4))
    })
    
    # Create the plot
    output$plot <- renderPlot({
      x <- seq(-4, 4, length.out = 1000)
      y <- dnorm(x)
      
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line(color = "blue") +
        geom_area(data = subset(data.frame(x, y), x >= z1 & x <= z2), aes(x, y), fill = "red", alpha = 0.5) +
        labs(title = "Standard Normal Distribution",
             x = "Z-Score",
             y = "Density") +
        theme_minimal()
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
