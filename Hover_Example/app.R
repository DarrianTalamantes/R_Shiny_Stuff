library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 350,hover = hoverOpts(id ="plot_hover"))
    )
  ),
  fluidRow(
    column(width = 5,
           verbatimTextOutput("hover_info")
    )
  )
)

server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    
    ggplot(mtcars, aes(x=mpg,y=disp,color=factor(cyl))) + geom_point()
    
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      dist=sqrt((hover$x-mtcars$mpg)^2+(hover$y-mtcars$disp)^2)
      cat("Weight (lb/1000)\n")
      if(min(dist) < 3)
        mtcars$wt[which.min(dist)]
    }
    
    
  })
}
shinyApp(ui, server)