# Data cleansing steps,already covered in Rmd file
my_data <- read.csv("freeway crashes.CSV", stringsAsFactors = FALSE)
new_data<-my_data[-c(2:9,12,13,10,19,20,21,22,24,25,26)]
new_data$alc_drug <- ifelse(new_data$alch_susp_ind | new_data$drug_susp_ind, 1, 0)
new_data<-new_data[-c(9,10)]
new_data<-na.omit(new_data)
new_data <- subset(new_data, milt_time != "UNK")
View(new_data)

# Load required libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Accident Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select a variable to plot:",
                  choices = c("Speed Limit", "Driver Age", "Year", "Alcohol/Drug Related",
                              "Time of Accident", "Road Condition", "Weather Condition",
                              "Light Condition", "Injury Severity", "Number of Lanes",
                              "Traffic Volume"),
                  selected = "Speed Limit"),
      sliderInput("binwidth", "Select Binwidth for Histogram:",
                  min = 1, max = 20, value = 5, step = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  # Reactive data based on selected variable
  data <- reactive({
    switch(input$variable,
           "Speed Limit" = new_data$spd_limt,
           "Driver Age" = new_data$prty_age,
           "Year" = new_data$year,
           "Alcohol/Drug Related" = new_data$alc_drug,
           "Time of Accident" = new_data$milt_time,
           "Road Condition" = new_data$rd_cond_cd,
           "Weather Condition" = new_data$wthr_cd,
           "Light Condition" = new_data$lit_cd,
           "Injury Severity" = new_data$injy_svty_cd,
           "Number of Lanes" = new_data$num_lns,
           "Traffic Volume" = new_data$traffic_volume)
  })
  
  # Create plot based on selected variable
  output$plot <- renderPlot({
    ggplot() +
      aes(x = data()) +
      geom_histogram(binwidth = input$binwidth, fill = "steelblue") +
      labs(title = paste("Distribution of", input$variable, "in Accidents (2010-2017)"),
           x = input$variable,
           y = "Count")
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)
