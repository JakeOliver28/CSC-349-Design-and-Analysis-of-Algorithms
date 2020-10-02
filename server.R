#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(neuralnet)
library(ggplot2)

forestfires <- read.csv("forestfires.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  temp_factor <- cut(forestfires$temp, c(0,5,10,15,20,25,30,35), labels = c(5,10,15,20,25,30,35), right = T)
  
  
  
  
  months <- factor(forestfires$month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
  
  
  
  ## everything for the linear model
  output$fit <- reactive({
    if (input$logtran == "Log Transformation on Y"){
      fit <- lm(log(area+1) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    
    if (input$logtran == "No Transformation"){
      fit <- lm((area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    return(fit)
  })
  
  # Summary of linear model
  output$linearsummary <- renderPrint({
    if (input$logtran == "Log Transformation on Y"){
      fit <- lm(log(area+1) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    
    if (input$logtran == "No Transformation"){
      fit <- lm((area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    summary(fit)})
  
  # Residual Plot for Linear Regression
  output$residplot <- renderPlot({
    if (input$logtran == "Log Transformation on Y"){
      fit <- lm(log(area+1) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    
    if (input$logtran == "No Transformation"){
      fit <- lm((area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    plot(fit$residuals ~ fit$fitted.values)})
  
  # Residual Histogram for Linear Model
  output$residhist <- renderPlot({
    if (input$logtran == "Log Transformation on Y"){
      fit <- lm(log(area+1) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    
    if (input$logtran == "No Transformation"){
      fit <- lm((area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    hist(fit$residuals)})
  
  # QQ Plot for Linear Model
  output$qqlinear <- renderPlot({
    if (input$logtran == "Log Transformation on Y"){
      fit <- lm(log(area+1) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    
    if (input$logtran == "No Transformation"){
      fit <- lm((area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    qqnorm(fit$residuals)})
  
  # MSE for Linear Model
  output$linearmse <- renderPrint({
    if (input$logtran == "Log Transformation on Y"){
      fit <- lm(log(area+1) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    
    if (input$logtran == "No Transformation"){
      fit <- lm((area) ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = forestfires)
    }
    sum(fit$residuals[401:513]^2) / (length(fit$residuals[401:513]) - 1)})
  
  
  # Neural Network Model
  output$net <- reactive({
    library(neuralnet)
    scaledData <- scale(forestfires[,c(1,2,5,6,7,8,9,10,11,12,13)])
    training <- scaledData[1:400, ]
    testing <- scaledData[401:513, ]
    
    net <- neuralnet(area ~ X + Y + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = training, hidden = 12,
                     threshold = 0.1, rep = 10, algorithm = "rprop+",     err.fct = "sse", act.fct = "logistic", linear.output = FALSE)
    
    return(net)
  })
  
  # Neural Net MSE
  output$netmse <- reactive({
    
    scaledData <- scale(forestfires[,c(1,2,5,6,7,8,9,10,11,12,13)])
    training <- scaledData[1:400, ]
    testing <- scaledData[401:513, ]
    
    net <- neuralnet(area ~ X + Y + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = training, hidden = 12,
                     threshold = 0.1, rep = 10, algorithm = "rprop+",     err.fct = "sse", act.fct = "logistic", linear.output = FALSE)
    
    
    tester <- compute(net, testing[,1:10])
    mse <- sum((tester$net.result - testing[,11])^2) / (length(testing[,11]) - 1)
    return(mse)
  })
  
  # Neural Net Plot
  output$netplot <- renderPlot({
    scaledData <- scale(forestfires[,c(1,2,5,6,7,8,9,10,11,12,13)])
    training <- scaledData[1:400, ]
    testing <- scaledData[401:513, ]
    net <- neuralnet(area ~ X + Y + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data = training, hidden = 12,
                     threshold = 0.1, rep = 10, algorithm = "rprop+",     err.fct = "sse", act.fct = "logistic", linear.output = FALSE)
    
    
    tester <- compute(net, testing[,1:10])
    qplot(forestfires[401:513, 13], tester$net.result) + xlab("Actual Values") + ylab("Predicted Values") + ggtitle("Fire Area (Hectares)")})
  
  
  
  
  
  # Temperature Graph
  output$tempGraph <- renderPlot(
    ggplot(data = subset(forestfires, temp_factor %in% input$tempLevel), aes(x = RH, y = ISI, color = temp)) + 
      geom_point() + 
      labs(x = "Relative Humidity", y = "ISI Index", color = "Temperature", title = "Initial Spread Index (ISI), Relative Humidity, and Temperature")+
      xlim(c(20,100))+
      ylim(c(0,20))
  )
  
  months <- factor(forestfires$month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "nov", "dec"))
  
  
  fire_subset <- reactive({
    a <- subset(forestfires, months == levels(months)[input$month], select = c(X,Y))
    a <- a[,1:2]
    a <- -table(a)
    basegrid <- matrix(rep(rep(0,9),9), nrow = 9)
    
    for (i in row.names(a)) {
      for (j in colnames(a)) {
        basegrid[as.numeric(i),as.numeric(j)] <- a[i, j]
      }
    }
    return(basegrid)
  })
  
  # Heat Map
  output$heatMap <- renderPlot(
    image(fire_subset(), col = heat.colors(10), xlab = "x-Axis Spatial Coordinate", ylab = "y-Axis Spatial Coordinate", main = "Heatmap of x/y Spatial Coordinates Within Montesinho Park Map")
  )
  
  # Boxplots
  output$weekday <- renderPlot(ggplot(data = subset(forestfires, day == input$weekday), aes(x = day, y = FFMC, fill = day)) + 
                                 geom_boxplot() +
                                 guides(fill=FALSE) +
                                 labs(x = "Day of the Week", y = "FFMC Index", title = "The Fine Fuel Moisture Code (FFMC) Index by Day of the Week")
                               
  )
  
  
  
  
  
})
