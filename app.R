#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



install.packages(c("shiny", "ggplot2", "dplyr", "rsconnect"))


library(shiny)
library(ggplot2)
library(dplyr)

# ---- 1. DATA (simulated illustrative data — replace with your own anonymised/aggregated data) ----
set.seed(42)
n <- 300
eco_data <- data.frame(
  country      = sample(c("Nepal", "India"), n, replace = TRUE),
  eco_anxiety  = round(rnorm(n, 3.2, 0.8), 2),
  solastalgia  = round(rnorm(n, 2.9, 0.9), 2),
  depression   = round(rnorm(n, 2.5, 1.0), 2),
  anxiety      = round(rnorm(n, 2.7, 1.0), 2),
  stress       = round(rnorm(n, 3.0, 0.9), 2)
)
# make relationships non-trivial so the plot looks real
eco_data$depression <- with(eco_data, 0.4 * eco_anxiety + rnorm(n, 1, 0.7))
eco_data$anxiety    <- with(eco_data, 0.5 * eco_anxiety + rnorm(n, 0.8, 0.7))

eco_vars    <- c("eco_anxiety", "solastalgia")
health_vars <- c("depression", "anxiety", "stress")

# ---- 2. USER INTERFACE ----
ui <- fluidPage(
  titlePanel("Edinburgh Eco-emotions: Interactive Explorer"),
  p(em("Illustrative demo using simulated data. Built with R Shiny.")),
  sidebarLayout(
    sidebarPanel(
      selectInput("eco",    "Eco-emotion (x-axis):",    choices = eco_vars),
      selectInput("health", "Mental health (y-axis):",  choices = health_vars),
      selectInput("group",  "Colour by:", choices = c("None" = "none", "Country" = "country")),
      checkboxInput("trend", "Show fitted line", value = TRUE)
    ),
    mainPanel(
      plotOutput("scatter"),
      br(),
      verbatimTextOutput("corr")
    )
  )
)

# ---- 3. SERVER LOGIC ----
server <- function(input, output) {
  
  output$scatter <- renderPlot({
    p <- ggplot(eco_data, aes(x = .data[[input$eco]], y = .data[[input$health]]))
    
    if (input$group == "country") {
      p <- p + geom_point(aes(colour = country), alpha = 0.6, size = 2)
    } else {
      p <- p + geom_point(alpha = 0.6, size = 2, colour = "#2c3e50")
    }
    
    if (input$trend) {
      p <- p + geom_smooth(method = "lm", se = TRUE, colour = "#c0392b")
    }
    
    p + labs(x = input$eco, y = input$health) +
      theme_minimal(base_size = 14)
  })
  
  output$corr <- renderText({
    r <- cor(eco_data[[input$eco]], eco_data[[input$health]], use = "complete.obs")
    paste0("Pearson correlation (r) between ", input$eco,
           " and ", input$health, ": ", round(r, 3))
  })
}

# ---- 4. RUN ----
shinyApp(ui = ui, server = server)


library(rsconnect)
rsconnect::deployApp("~/Desktop/eco-emotions-app/eco-emotions-app")















