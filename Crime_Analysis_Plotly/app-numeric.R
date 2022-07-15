############################################
# Data Professor                           #
# http://youtube.com/dataprofessor         #
# http://github.com/dataprofessor          #
# http://facebook.com/dataprofessor        #
# https://www.instagram.com/data.professor #
############################################

# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")
# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Iris Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input Your Flowers Parameters')),
    sliderInput("Sepal.Length", 
       label = "Sepal Length", 
       value = 3,
       min = min(TrainSet$Sepal.Length),
       max = max(TrainSet$Sepal.Length)
      ),
    sliderInput("Sepal.Width", 
      label = "Sepal Width", 
      value = 3,
      min = min(TrainSet$Sepal.Width),
      max = max(TrainSet$Sepal.Width)
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
  )
)
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )


####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
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
  # renderPrint prints words
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  # renderTable renders a data table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
