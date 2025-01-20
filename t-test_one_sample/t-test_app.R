library(shiny)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "lumen"),
    
    # Application title
    titlePanel("One-Sample t-test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("df", "Degrees of Freedom (df)", value = 10, min = 1, step = 1),
            numericInput("pval", "P-value", value = 0.05, min = 0.0001, step = 0.01),
            radioButtons("tail", "Test Type:",
                         choices = list("One-Tailed" = "one", "Two-Tailed" = "two"),
                         selected = "two"),
            actionButton("plot", "Generate Plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$plot, {
      output$tPlot <- renderPlot({
        
        # Inputs
        df <- input$df
        pval <- input$pval
        tail <- input$tail
        
        # Compute critical t-values
        if (tail == "two") {
          crit_t <- qt(c(pval/2, 1 - (pval/2)), df)
        } else {
          crit_t <- qt(1- pval, df)
        }
        
        # Generate t-distribution area
        x <- seq(-4, 4, length.out = 1000)
        y <- dt(x, df)
        
        # Plot the t-distribution
        plot(x, y, type = "l", col = "blue",
             main = "t-Distribution with Highlighted Area",
             xlab = "t-value", ylab = "Density")
        
        # Highlight critical areas
        if (tail == "two") {
          polygon(x = c(min(x[x <= crit_t[1]]), x[x <= crit_t[1]], crit_t[1]),
                  y = c(0, y[x <= crit_t[1]], 0), col = "red", border = NA)
          polygon(c(crit_t[2], x[x >= crit_t[2]], max(x[x >= crit_t[2]])),
                  c(0, y[x >= crit_t[2]], 0), col = "red", border = NA)
        } else {
          polygon(c(crit_t, x[x >= crit_t], max(x[x >= crit_t])),
                  c(0, y[x >= crit_t], 0), col = "red", border = NA)
        }
        
        # Add critical t-value(s) to the plot
        abline(v = crit_t, col = "darkred", lty = 2)
        
        # Annotate critical t-values
        if (tail == "two") {
          text(crit_t[1] - 0.5, 0.1, labels = sprintf("t = %.2f", crit_t[1]), col = "darkred", pos = 1)
          text(crit_t[2] + 0.5, 0.1, labels = sprintf("t = %.2f", crit_t[2]), col = "darkred", pos = 1)
        } else {
          text(crit_t + 0.5, 0.1, labels = sprintf("t = %.2f", crit_t), col = "darkred", pos = 1)
        }
        
        # Add legend
        legend("topright", legend = c("Critical Region", "t-Distribution"),
               fill = c("red", "blue", border = NA))
      })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
