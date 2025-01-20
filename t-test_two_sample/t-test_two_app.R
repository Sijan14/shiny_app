library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lumen"),
  
  titlePanel("Two-Sample t-Test"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Control Group"),
        numericInput("mean1", "Mean:", value = 0),
        numericInput("sd1", "SD:", value = 3),
        numericInput("n1", "Sample Size:", value = 30, min = 2)
      ),
      tags$br(),
      wellPanel(
        h4("Experimental Group"),
        numericInput("mean2", "Mean:", value = 1),
        numericInput("sd2", "SD:", value = 3),
        numericInput("n2", "Sample Size:", value = 30, min = 2)
      ),
      tags$br(),
      wellPanel(
        numericInput("crit_t", "Critical t-value:", value = 1.96, step = 1),
        selectInput("tail", "Tail Type:", choices = c("Two-tailed" = "two", "One-tailed" = "one"), selected = "one")
      )
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      htmlOutput("significance_text")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Inputs
    mean1 <- input$mean1
    mean2 <- input$mean2
    sd1 <- input$sd1 / sqrt(input$n1)
    sd2 <- input$sd2 / sqrt(input$n2)
    crit_t <- input$crit_t
    tail <- input$tail
    
    # Calculate t-statistic
    t_stat <- (mean2 - mean1) / sqrt((sd1^2) + (sd2^2))
    
    # Dynamic x-axis range calculation
    x_min <- min(mean1 - 4 * sd1, mean2 - 4 * sd2, -crit_t -1) # Extend based on both distributions and critical t
    x_max <- max(mean1 + 4 * sd1, mean2 + 4 * sd2, crit_t +1)
    x <- seq(x_min, x_max, length.out = 1000)
    
    dist1 <- dnorm(x, mean = mean1, sd = sd1)
    dist2 <- dnorm(x, mean = mean2, sd = sd2)
    
    # Critical region for Distribution 1 (same as before)
    if (tail == "two") {
      crit_region_right <- x[x >= crit_t]
      crit_region_left <- x[x <= -crit_t]
      dist1_crit_right <- dnorm(crit_region_right, mean = mean1, sd = sd1)
      dist1_crit_left <- dnorm(crit_region_left, mean = mean1, sd = sd1)
      overlap_right<- x[x >= crit_t]
      overlap_left <- x[x <= -crit_t]
      dist2_overlap_right <- dnorm(overlap_right, mean = mean2, sd = sd2)
      dist2_overlap_left <- dnorm(overlap_left, mean = mean2, sd = sd2)
    } else {
      crit_region <- x[x >= crit_t]
      dist1_crit <- dnorm(crit_region, mean = mean1, sd = sd1)
      overlap <- x[x >= crit_t]
      dist2_overlap <- dnorm(overlap, mean = mean2, sd = sd2)
    }
    
    # Plot (same as before, but using the dynamic x)
    plot(x, dist1, type = "l", lwd = 2, col = "blue",
         ylim = c(0, max(c(dist1, dist2)) * 1.2),
         main = "Two-Sample t-Test Distributions",
         xlab = "t-value", ylab = "Density")
    lines(x, dist2, lwd = 2, col = "red")
    if(tail == "two"){
      polygon(c(crit_region_right, rev(crit_region_right)),
              c(dist1_crit_right, rep(0, length(dist1_crit_right))),
              col = rgb(0, 0, 1, 0.3), border = NA)
      polygon(c(overlap_right, rev(overlap_right)),
              c(dist2_overlap_right, rep(0, length(dist2_overlap_right))),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
      polygon(c(crit_region_left, rev(crit_region_left)),
              c(dist1_crit_left, rep(0, length(dist1_crit_left))),
              col = rgb(0, 0, 1, 0.3), border = NA)
      polygon(c(overlap_left, rev(overlap_left)),
              c(dist2_overlap_left, rep(0, length(dist2_overlap_left))),
              col = rgb(1, 0, 0, 0.3), border = NA)
      
      abline(v = c(crit_t, -crit_t), col = "darkblue", lty = 2)
      text(crit_t, -0.02, sprintf("Crit t = %.2f", crit_t), col = "darkblue", pos = 4)
      text(-crit_t, -0.02, sprintf("Crit t = %.2f", -crit_t), col = "darkblue", pos = 2)
    } else {
      polygon(c(crit_region, rev(crit_region)),
              c(dist1_crit, rep(0, length(dist1_crit))),
              col = rgb(0, 0, 1, 0.3), border = NA)
      polygon(c(overlap, rev(overlap)),
              c(dist2_overlap, rep(0, length(dist2_overlap))),
              col = rgb(1, 0, 0, 0.3), border = NA)
      abline(v = crit_t, col = "darkblue", lty = 2)
      text(crit_t, -0.02, sprintf("Crit t = %.2f", crit_t), col = "darkblue", pos = 4)
    }
    
    # Add legend
    legend("topright", legend = c("Control Group", "Experimental Group", "Critical Region (Control)", "Overlap (Experimental)"),
           col = c("blue", "red", rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.3)), lwd = 2, pch = 15, pt.cex = 2)
  })
  # ... (rest of the server code remains the same)
  output$significance_text <- renderText({
    mean1 <- input$mean1
    mean2 <- input$mean2
    sd1 <- input$sd1 / sqrt(input$n1)
    sd2 <- input$sd2 / sqrt(input$n2)
    crit_t <- input$crit_t
    tail <- input$tail
    
    t_stat <- (mean2 - mean1) / sqrt((sd1^2) + (sd2^2))
    
    if (tail == "two") {
      if (abs(t_stat) > crit_t) {
        paste("<br>The groups are significantly different (t =", round(t_stat, 2), ", critical t =", crit_t, ").")
      } else {
        paste("<br>The groups are not significantly different (t =", round(t_stat, 2), ", critical t =", crit_t, ").")
      }
    } else { # one-tailed
      if (t_stat > crit_t) {
        paste("<br>The Experimental group is significantly greater than the Control group (t =", round(t_stat, 2), ", critical t =", crit_t, ").")
      } else {
        paste("<br>The Experimental group is not significantly greater than the Control group (t =", round(t_stat, 2), ", critical t =", crit_t, ").")
      }
    }
  })
}

shinyApp(ui, server)