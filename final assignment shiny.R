#
# This is a Shiny web application (Data Products Week 4 Assignment)
#
# This application is about speed (x) and stopping distance of cars (y)
# - get range of speeds in sliderinput and filter the data
#     all the calculations and displays take into account the range of speed
# - Calculate lowess (regression method : locally weighted scatterplot smoothing) and draw it
#     only if checkbox "show lowess" is true
#
# Output : there is two tabs :
# - The first tab is for the plotting
# - The second tab is about the data (selected data)
#
# Plot : 
# Brush the plot causes 
#     . the calculation of the linear model
#     . the calculation and display of slope and intercept of the model
#     . add the straight line of the linear model to the plot (in accord with the selected data)
# 
# Data :
# Display the selected data in a data table with sort and search
#
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(shinydashboard)


ui <- fluidPage(
   
          # Application title
          titlePanel("Speed and stopping distances of cars"),
   
          sidebarLayout(
        
            sidebarPanel(
              
              h4("Slope"),
              textOutput("slopeOut"),
              
              h4("Intercept"),
              textOutput("interceptOut"),
              
              br(),
              # slider input for range of speed
              sliderInput("sliderX", "Range of speed (mph)", 0, 25, step=1, value = c(0, 25)),
        
              # CheckBox for printing lowess (Locally weighted regression and smoothing scatter plots)
              checkboxInput("show", "Show lowess", value = TRUE)
            ),
                  
            mainPanel(
              tabsetPanel(type = "tabs",
                  # tab "plot"
                  tabPanel(
                    # Show a plot of the generated distribution
                    "Plot", plotOutput("plot1", brush = brushOpts(id = "brush1",delay = 5000))
                  ),
                  # tab "data table"
                  tabPanel(
                    # Show a plot of the generated distribution
                    "Table", fluidRow(column(4, dataTableOutput("cars")))
                  )
              )
          )
      )
)

server <- function(input, output) {
  
  model <- reactive({
    # Filter data with range in sliderInput
    c <- filter(cars, cars$speed >= input$sliderX[1] & cars$speed <=  input$sliderX[2])
    brushed_data <- brushedPoints(c, input$brush1, xvar= "speed", yvar = "dist")
    if (nrow(brushed_data) < 2) {return(NULL)}
    # calculate linear model
    lm(dist ~ speed, data = brushed_data)
  })
   
   # get slope from model
   output$slopeOut <- renderText({
     m <- model()
     if (is.null(m)){"No model found"} else {m$coefficients[1]} 
   })
   
   # get intercept from model
   output$interceptOut <- renderText({
     m <- model()
     if (is.null(m)){"No model found"} else {m$coefficients[2]} 
   })
   
   # get data table (data are filtered whith sliderInput range)
   output$cars <- renderDataTable({
     filter(cars, cars$speed >= input$sliderX[1] & cars$speed <=  input$sliderX[2])
   })
   
   # plot with filtered data
   output$plot1 <- renderPlot({
     c <- filter(cars, cars$speed >= input$sliderX[1] & cars$speed <=  input$sliderX[2])
     m <- model()
     plot(c$speed, c$dist, xlab="Speed", ylab= "Stopping distance (ft)", 
          xlim = input$sliderX, xaxp = (c(input$sliderX,10)),
          main = "Cars", cex= 1.5, pch=16, bty="n")
     # add linear straight line to the plot
     if (!is.null(m)) { abline(m, col="blue", lwd=2) }
     # add lowess if required (check checkbox) 
     if (input$show == TRUE) lines(lowess(c), col="red")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(webshot)
appshot(app = "app/", file = "screenshot.png", vheight = 300)
knitr::include_graphics("screenshot.png")




  

