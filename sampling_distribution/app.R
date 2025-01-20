library(shiny)
library(ggplot2)
library(gganimate)
library(dplyr)

ui <- fluidPage(
  titlePanel("Sampling Distribution of Sample Means"),
  sidebarLayout(
    sidebarPanel(
      h3("Inputs"),
      numericInput("pop_mean", "Population Mean:", value = 50, min = 0),
      selectInput("pop_dist", "Population Distribution:", 
                  choices = c("Normal" = "norm", "Uniform" = "unif", "Exponential" = "exp")),
      numericInput("sample_size", "Sample Size (n):", value = 30, min = 1),
      numericInput("num_samples", "Number of Samples:", value = 100, min = 1),
      actionButton("generate", "Generate")
    ),
    mainPanel(
      h3("Visualizations"),
      plotOutput("population_plot"),
      plotOutput("sample_plot"),
      plotOutput("sampling_dist_plot"),
      plotOutput("animation_plot")
    )
  )
)

server <- function(input, output, session) {
  set.seed(123) # For reproducibility
  
  population <- reactive({
    dist <- input$pop_dist
    n <- 10000
    if (dist == "norm") {
      rnorm(n, mean = input$pop_mean, sd = 10)
    } else if (dist == "unif") {
      runif(n, min = input$pop_mean - 20, max = input$pop_mean + 20)
    } else if (dist == "exp") {
      rexp(n, rate = 1 / input$pop_mean)
    }
  })
  
  samples <- eventReactive(input$generate, {
    replicate(input$num_samples, sample(population(), input$sample_size, replace = TRUE), simplify = FALSE)
  })
  
  sample_means <- eventReactive(input$generate, {
    sapply(samples(), mean)
  })
  
  output$population_plot <- renderPlot({
    ggplot(data.frame(value = population()), aes(x = value)) +
      geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
      geom_vline(xintercept = mean(population()), color = "red", linetype = "dashed", size = 1) +
      labs(title = "Population", x = "Value", y = "Frequency") +
      theme_minimal()
  })
  
  output$sample_plot <- renderPlot({
    req(samples())
    single_sample <- samples()[[1]]
    ggplot(data.frame(value = single_sample), aes(x = value)) +
      geom_histogram(bins = 10, fill = "green", alpha = 0.7) +
      geom_vline(xintercept = mean(single_sample), color = "red", linetype = "dashed", size = 1) +
      labs(title = "Sample", x = "Value", y = "Frequency") +
      theme_minimal()
  })
  
  output$sampling_dist_plot <- renderPlot({
    req(sample_means())
    ggplot(data.frame(mean = sample_means()), aes(x = mean)) +
      geom_histogram(bins = 30, fill = "purple", alpha = 0.7) +
      geom_vline(xintercept = mean(sample_means()), color = "red", linetype = "dashed", size = 1) +
      labs(title = "Sample Means", x = "Sample Mean", y = "Frequency") +
      theme_minimal()
  })
  
  output$animation_plot <- renderPlot({
    req(samples())
    
    anim_data <- data.frame(
      Sample = rep(1:length(samples()), each = input$sample_size),
      Value = unlist(samples()),
      Mean = rep(sample_means(), each = input$sample_size)
    )
    
    anim <- ggplot(anim_data, aes(x = Value, frame = Sample)) +
      geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
      geom_vline(aes(xintercept = Mean), color = "blue", linetype = "dashed", size = 1) +
      labs(title = "Sampling Animation", x = "Value", y = "Frequency") +
      theme_minimal()
    
    gganimate::animate(anim, renderer = gifski_renderer(), nframes = length(samples()))
  })
}

shinyApp(ui, server)
