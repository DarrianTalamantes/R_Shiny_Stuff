####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################


# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
weather <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv"), stringsAsFactors = TRUE )
# # The stringasFactors allows us to look at strings as facotrs rather than strings. Allows for random forest to work on them.
# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE) 
# ntree is how many predictions we are making
# mtry number of variables tried at each split. Just use 4.
# importance = TRUE insures that there is some kind of correlation between our facotors and data
# The first input "play" is the column we are trying to predict

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Play Golf?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"), # The HTML way to make a 3rd level heading
                  h3("INpuT ParaMEtErs"), # R shiny way to make 3rd level heading
                  selectInput("outlook", label = "Outlook:", 
                              choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                              selected = "Rainy"),
                  sliderInput("temperature", label = "Temperature:",
                              min = 64, max = 86,
                              value = 70),
                  sliderInput("humidity", label = "Humidity:",
                              min = 65, max = 96,
                              value = 90),
                  selectInput("windy", label = "Windy:", 
                              choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                              selected = "TRUE"),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny")) # Because our test only has 1 factor we must tell R there are 3 possible facotrs
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    # This is using the outputs of predict and putting them into columns. The first is Yes or No, the second shows the predicted amounts of yes and no's.
    print(Output)
    # The predict function uses a model to predict the outcome of inputed numbers into the model. Easiest way to use it is a linear model.
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
# The isolate allows for a non reactive output. It will wait for an action button to be pressed  
# datasetInput is the name of the data set we create. The final form of it is called Output.
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
