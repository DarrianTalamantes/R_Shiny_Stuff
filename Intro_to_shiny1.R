####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)
library(shinythemes)
data(airquality)


# UI ---------------------------------------------------------------------- 
# To create code sections: crt + shift + R
# To jump sections: Shift+alt + J 


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = shinytheme("slate"),  # This line of code allows you to insert a theme into your application
  
  # App title, align allows you to align it where you want ----
  titlePanel(
    h1("Ozone level!", align = "center")
    ), 
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30,  # This is the default value
                  step = 1 ) # step size is the increment you can increase it by,  if step >1 then start on 0.
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    x    <- airquality$Ozone # Choosing data to use
    x    <- na.omit(x)  # Omits na's  
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Ozone level")
    
  })
  
}

# Run Shiny ---------------------------------------------------------------


# Create Shiny app ----
shinyApp(ui = ui, server = server)