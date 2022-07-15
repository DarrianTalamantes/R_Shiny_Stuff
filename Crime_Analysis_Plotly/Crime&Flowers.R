###################
# Author: Darrian Talamantes
# Email: darrianrtalamantes6@gmail.com
# Purpose: Redo the crime analysis using plotly to get better visualization
########################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lintr)

# Things generated before UI
choices <- colnames(USArrests)
choices <- choices[choices != "UrbanPop"]

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# Define UI for application that draws a histogram
# The main parts of this type of UI is dashboardHeader(),dashboardSidebar(),dashboardBody()
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "C&F Crime & FLowers"),
  dashboardSidebar(
    sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("USA Crime", tabName = "usacrime", icon = icon("eye")),
      # The next icon is from font-awsome, remember you have to use the free ones
      menuItem("Flower", tabName = "flowers", icon = icon("fa-solid fa-seedling", class = NULL, lib ="font-awesome", verify_fa = FALSE))
    )
  ),
  dashboardBody(
    # The tab items is allowing me to create multiple pages
    tabItems(
      tabItem("usacrime",
              h1("Crime in the USA by State"),
              selectInput("crime",
                          label = "Type of Crime:",
                          choices=choices,
                          selected = "Assault"),
              box(plotlyOutput("barplot"), width = 10)
              ),
      tabItem("flowers",
              box(
                h1("Flower Prediction"),
                solidHeader = FALSE,
                background = "black",
                sliderInput("Sepal.Length",
                            label = "Sepal Length",
                            value = 4.4,
                            min = min(TrainSet$Sepal.Length),
                            max = max(TrainSet$Sepal.Length)
                ),
                sliderInput("Sepal.Width",
                            label = "Sepal Width",
                            value = 3,
                            min = min(TrainSet$Sepal.Width),
                            max = max(TrainSet$Sepal.Width),
                ),
                sliderInput("Petal.Length",
                            label = "Petal Length",
                            value = 3,
                            min = min(TrainSet$Petal.Length),
                            max = max(TrainSet$Petal.Length)
                ),
                sliderInput("Petal.Width",
                            label = "Petal Width",
                            value = 0.3,
                            min = min(TrainSet$Petal.Width),
                            max = max(TrainSet$Petal.Width)
                ),
                actionButton("submitbutton", "Submit",
                             class = "btn btn-primary")
                ),
              box(
                tags$label(h3('Status/Output')), # Status/Output Text Box
                verbatimTextOutput('contents'),
                tableOutput('tabledata') # Prediction results table
              )
            )
      )
  )
)

# Define server logic 
server <- function(input, output){
  # This output is for the Crime Data
  output$barplot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    crime <- input$crime  
    # draw the bar plot of the selected crime using plotly
    fig <- plot_ly(data = USArrests, x = rownames(USArrests),  y= ~.data[[crime]], type = "bar", name = crime) 
    fig <- fig %>% layout(xaxis = list(title = "States", categoryorder = "total descending"), barmode = "group")
    fig <- fig %>% layout(yaxis = list(title = paste0(crime, "s per 100,000 people")))
    fig    
  })
  
  # This helps gather all the input data for the Flower Predictor
  datasetInput <- reactive({  
    # Creates a big 2 column dataframe that has all data labels
    df <- data.frame(
      Name = c("Sepal Length",
               "Sepal Width",
               "Petal Length",
               "Petal Width"),
      Value = as.character(c(input$Sepal.Length,
                             input$Sepal.Width,
                             input$Petal.Length,
                             input$Petal.Width)),
      stringsAsFactors = FALSE)
    # Create test data 
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  # renderPrint prints words for the Flower Predictor 
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  # renderTable renders a data table for the flower predictor
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
