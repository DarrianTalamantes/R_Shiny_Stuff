###################
# Author: Darrian Talamantes
# Email: darrianrtalamantes6@gmail.com
# Purpose: Make my first Shiny applicatino
########################

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)


# Things generated before UI
Choices    <- colnames(USArrests)
Choices <- Choices[Choices != "UrbanPop"]

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),
                
    # Application title
    h1(id="big-heading", "Crime In The USA by State", align = "center"),
    tags$style(HTML("#big-heading{color: blue;}")),
    # titlePanel("Crime In The USA by State"), #calls title in a less custom maner

    # Sidebar with a dropdown input for crime type 
      sidebarPanel(
          selectInput("crime",
                      label = "Type of Crime:",
                      choices=Choices,
                      selected = "Assault")
      ),

      # Show a plot 
      mainPanel(
         plotOutput("barplot")
      )
)

# Define server logic 
server <- function(input, output) {
    
    output$barplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        crime <- input$crime  
        # draw the bar plot of the selected crime
        ggplot(data=USArrests, aes(x= reorder(rownames(USArrests), -.data[[crime]]), y=.data[[crime]])) +
          geom_bar(stat="identity")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
