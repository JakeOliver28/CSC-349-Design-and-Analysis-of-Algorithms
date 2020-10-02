#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Creating three different tabs
  tabsetPanel(
  
  # Tab 1
    tabPanel("Visualizations",
           titlePanel("Wild Fires"),
  
      # Show heat map with sliders
      fluidRow(column(width = 1), column(width = 4,
        sliderInput("month",
                  "Month of the year:",
                  min = 1,
                  max = 12,
                  value = 1)
        ),
        column(width = 7, plotOutput("heatMap"))
      ),
      
      # Show temperature graph with checkboxes 
      fluidRow(column(width = 1), column(width = 4,
        checkboxGroupInput("tempLevel",
                         "Temperature",
                         choiceNames = c("0-5","5- 10", "10-15", "15-20", "20-25", "25-30", ">30"),
                         choiceValues = c(5,10,15,20,25,30,35),
                         selected = 5
                         
          )
        ), column(width = 7, plotOutput("tempGraph"))),
      
      # Show boxplots with radio buttons
      fluidRow(column(width = 1), 
              column(width = 4, 
                    radioButtons("weekday", NULL , choices = c("sun", "mon", "tue", "wed", "thu", "fri", "sat"), selected = "sun")), 
              column(width = 7, plotOutput("weekday"))
      )
    ),
    
  # Tab 2
  tabPanel("Linear Regression", 
          
           # Show linear model output, plots, and MSE
           radioButtons("logtran",label = "Transformation", choices = c("No Transformation", "Log Transformation on Y")),
           verbatimTextOutput("linearsummary"),
           plotOutput("residplot"),
           plotOutput("residhist"),
           plotOutput("qqlinear"),
           paste("MSE: "),
           verbatimTextOutput("linearmse")
      ),
  # Tab 3
  tabPanel("Neural Network",
           
           # Show neural net plot and MSE
           plotOutput("netplot"),
           paste("MSE: "),
           verbatimTextOutput("netmse")
      
    
  )
  ) 
)
)

