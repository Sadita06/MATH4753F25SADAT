# ============================================================
# MLE Visualizer App - 5 Univariate Distributions
# ============================================================

library(shiny)

ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose a distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Bernoulli", "Uniform")),
      numericInput("n", "Sample size (n):", 50, min = 5, max = 1000),
      numericInput("param1", "True parameter (e.g., mean, rate, etc.):", 1),
      actionButton("simulate", "Simulate Sample"),
      hr(),
      helpText("This app shows how MLEs change based on simulated data.")
    ),
    mainPanel(
      h4("Sample Summary"),
      verbatimTextOutput("summary"),
      h4("Likelihood Function and MLE"),
      plotOutput("likelihoodPlot", height = "350px"),
      h4("Histogram with Fitted Distribution"),
      plotOutput("histPlot", height = "350px")
    )
  )
)

server <- function(input, output, session) {
  data <- eventReactive(input$simulate, {
    n <- input$n
    theta <- input$param1
    dist <- input$dist

    switch(dist,
           "Normal" = rnorm(n, mean = theta, sd = 1),
           "Exponential" = rexp(n, rate = theta),
           "Poisson" = rpois(n, lambda = theta),
           "Bernoulli" = rbinom(n, size = 1, prob = theta),
           "Uniform" = runif(n, 0, theta))
  })

  # Sample summary
  output$summary <- renderPrint({
    x <- data()
    cat("Sample size:", length(x), "\n")
    cat("Mean:", mean(x), "\n")
    cat("Variance:", var(x), "\n")
  })

  # Likelihood visualization
  output$likelihoodPlot <- renderPlot({
    x <- data()
    dist <- input$dist
    n <- length(x)

    if (dist == "Normal") {
      grid <- seq(min(x) - 1, max(x) + 1, length.out = 200)
      ll <- sapply(grid, function(mu) sum(dnorm(x, mean = mu, sd = 1, log = TRUE)))
      mle <- mean(x)
      param_label <- "μ"
    } else if (dist == "Exponential") {
      grid <- seq(0.01, 3 / mean(x), length.out = 200)
      ll <- sapply(grid, function(lambda) sum(dexp(x, rate = lambda, log = TRUE)))
      mle <- 1 / mean(x)
      param_label <- "λ"
    } else if (dist == "Poisson") {
      grid <- seq(0.01, 2 * mean(x), length.out = 200)
      ll <- sapply(grid, function(lambda) sum(dpois(x, lambda = lambda, log = TRUE)))
      mle <- mean(x)
      param_label <- "λ"
    } else if (dist == "Bernoulli") {
      grid <- seq(0.001, 0.999, length.out = 200)
      ll <- sapply(grid, function(p) sum(dbinom(x, 1, p, log = TRUE)))
      mle <- mean(x)
      param_label <- "p"
    } else if (dist == "Uniform") {
      grid <- seq(max(x), 2 * max(x), length.out = 200)
      ll <- sapply(grid, function(theta) ifelse(all(x <= theta), -n * log(theta), -Inf))
      mle <- max(x)
      param_label <- "θ"
    }

    plot(grid, exp(ll - max(ll)), type = "l", lwd = 2, col = "blue",
         xlab = param_label, ylab = "Scaled Likelihood",
         main = paste("Likelihood for", dist, "Distribution"))
    abline(v = mle, col = "red", lwd = 2, lty = 2)
    legend("topright", legend = sprintf("MLE = %.3f", mle), col = "red", lwd = 2, bty = "n")
  })

  # Histogram with fitted density
  output$histPlot <- renderPlot({
    x <- data()
    dist <- input$dist

    hist(x, freq = FALSE, col = "#e8eef9", border = "white",
         main = paste("Sample Histogram and Fitted", dist), xlab = "x")
    xs <- seq(min(x), max(x), length.out = 200)

    if (dist == "Normal") {
      lines(xs, dnorm(xs, mean = mean(x), sd = 1), col = "red", lwd = 2)
    } else if (dist == "Exponential") {
      lines(xs, dexp(xs, rate = 1 / mean(x)), col = "red", lwd = 2)
    } else if (dist == "Poisson") {
      points(0:max(x), dpois(0:max(x), lambda = mean(x)), col = "red", pch = 16)
    } else if (dist == "Bernoulli") {
      barplot(c(mean(x == 0), mean(x == 1)), names.arg = c("0", "1"),
              col = c("#cde"), border = "white", ylim = c(0, 1))
      points(c(1, 2), c(1 - mean(x), mean(x)), col = "red", pch = 16)
    } else if (dist == "Uniform") {
      lines(xs, dunif(xs, 0, max(x)), col = "red", lwd = 2)
    }
  })
}

shinyApp(ui, server)
