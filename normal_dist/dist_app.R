library(bslib)
library(shiny)
library(ggplot2)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel("Histogram of distribution"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        sliderInput("mean", "Input the mean: ", min = 0, max = 50, value = 0),
        sliderInput("sd", "Input the standard deviation: ", min = 0, max = 30, value = 10)
      ),
      tags$br(),
      wellPanel(
        h4("Transform"), # Added h2 for better visual separation
        numericInput("add", "Add/Subtract: ", value = 0),
        numericInput("mul", "Multiply/Divide", value = 1, step = 0.10)
      )
    ),
    mainPanel(
      plotOutput("distPlot"),
      p("\n"),
      textOutput("summary_mean"),
      textOutput("summary_sd")
    )
  )
)
x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
# Define server logic required to draw a histogram
server <- function(input, output) { 
  # Generate a dataset based on inputs
  set.seed(123)
  df <- reactive({
    data.frame(
      value = rnorm(500, mean = input$mean, sd = input$sd)
    )
  })
  
  # Apply transformations separately (do not overwrite df)
  transformed_df <- reactive({
    data <- df()  # Get the original df
    data$value <- data$value + input$add
    data$value <- data$value * input$mul
    data
  })
  
  # Calculate mean and sd of the transformed df
  mean_val <- reactive(mean(transformed_df()$value))
  sd_val <- reactive(sd(transformed_df()$value))
  
  # Render the plot
  output$distPlot <- renderPlot({
    ggplot(transformed_df(), aes(x = value)) + 
      geom_histogram(fill = "lightblue", color = "black", alpha = 0.7, bins = 30) + 
      geom_density(aes(y = 2 * ..count..), colour = "red", adjust = 1) + 
      geom_vline(aes(xintercept = mean_val()), color = "blue", linetype = "dashed", size = 1) +  # Mean line
      geom_vline(aes(xintercept = mean_val() + sd_val()), color = "red", linetype = "dashed", size = 1) +  # SD line (+1 SD)
      geom_vline(aes(xintercept = mean_val() - sd_val()), color = "red", linetype = "dashed", size = 1) +  # SD line (-1 SD)
      annotate("text", x = mean_val() + 4, y = 100, label = paste("Mean"), color = "blue") +  # Mean text
      annotate("text", x = mean_val() + sd_val() + 4, y = 100, label = paste("+1 SD"), color = "red") +  # +1 SD text
      annotate("text", x = mean_val() - sd_val() + 4, y = 100, label = paste("-1 SD"), color = "red") +  # -1 SD text
      xlim(-50, 50) + 
      ylim(0, 100) + 
      labs(
        title = "Histogram of Simulated Data", 
        x = "Values", 
        y = "Density"
      ) + 
      theme_minimal()
  })
  
  # Output new mean and sd
  output$summary_mean <- renderText({
    paste("New Mean: ", round(mean_val()))
  })
  output$summary_sd <- renderText({
    paste("New SD: ", round(sd_val()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
