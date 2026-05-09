# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(colourpicker)
library(ggplot2)

# Load data (make sure this CSV is in the SAME folder as this app.R file)
mydata <- read.csv("smoking_data.csv")


#Clean the data
mydata$Smoke.everyday  <- as.numeric(gsub("%", "", mydata$Smoke.everyday))
mydata$Smoke.some.days <- as.numeric(gsub("%", "", mydata$Smoke.some.days))
mydata$Former.smoker   <- as.numeric(gsub("%", "", mydata$Former.smoker))
mydata$Never.smoked    <- as.numeric(gsub("%", "", mydata$Never.smoked))

ui <- fluidPage(
  
  titlePanel("Distribution of Smoking Behaviors in the U.S."),
  
  h3("Research Question"),
  p("What is the distribution of smoking behaviors across the U.S. population?"),
  
  h3("App Overview"),
  p("This app provides an interactive visualization of smoking behavior prevalence 
across U.S. states using data from the Behavioral Risk Factor Surveillance 
System (BRFSS). Users can explore the distribution of different smoking 
categories through histograms and summary statistics."),
  
  h4("Author"),
  p("Joel Adam Thuo"),
  
  h4("Data Source"),
  p("Behavioral Risk Factor Surveillance System (BRFSS), Centers for Disease 
Control and Prevention (CDC)."),
  
  h4("Methods"),
  p("The app provides a clear visualization of the distribution of smoking 
    behaviors across U.S. states, supported by summary statistics such as 
    measures of central tendency. While the analysis focuses primarily on 
    descriptive insights, it effectively allows users to explore variability 
    in smoking prevalence. More advanced statistical methods could further 
    enhance the depth of analysis in future publications or research."),
  
  h4("AI Disclosure"),
  p("This app was developed with assistance from ChatGPT for coding support and 
debugging."),
  
  tags$p(
    strong("GitHub Repository: "),
    tags$a(
      href = "https://github.com/mecheo245/Shinyapp",
      "View Source Code",
      target = "_blank"
    )
  ),
  
  br(),
  
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("var", "Choose a smoking category:",
                  choices = c("Daily smokers" = "Smoke.everyday",
                              "Some-day smokers" = "Smoke.some.days",
                              "Former smokers" = "Former.smoker",
                              "Never smoked" = "Never.smoked")),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      colourInput("color", "Choose color:", value = "blue")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("summaryStats")
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x <- mydata[[input$var]]
    
    # Ensure numeric data
    validate(
      need(is.numeric(x), "Please select a numeric column")
    )
    
    bins <- seq(min(x, na.rm = TRUE),
                max(x, na.rm = TRUE),
                length.out = input$bins + 1)
    
    hist(x,
         breaks = bins,
         col = input$color,
         border = "white",
         xlab = paste(input$var, "(%)"),
         main = paste("Distribution of", input$var, "across U.S. states"))
    
    abline(v = mean(x, na.rm = TRUE), col = "red", lwd = 2)
  })
  
  # Summary statistics 
  output$summaryStats <- renderPrint({
    x <- mydata[[input$var]]
    
    validate(
      need(is.numeric(x), "Please select a numeric column")
    )
    
    summary(x)
  })
}

shinyApp(ui = ui, server = server)
