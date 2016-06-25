library(shiny)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # get data
    #unzip("activity.zip")
    activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
    nrow(activity)
    dt <- data.table(steps = activity$steps, date = as.Date(activity$date), interval = activity$interval)
    
    output$slider <- renderUI({
        n <- input$days
        strdays <- "days"
        
        stepsPerDay <- aggregate(steps ~ cut(as.Date(date), as.character(paste(n, strdays, sep = " "))), dt, sum)
        nmax <- nrow(stepsPerDay)
        
        sliderInput("slider", "Number of Groups", value = nmax, min = 1, max = nmax, step = 1)       
    })
    
    output$distPlot <- renderPlot({
        n <- input$days
        strdays <- "days"

        stepsPerDay <- aggregate(steps ~ cut(as.Date(date), as.character(paste(n, strdays, sep = " "))), dt, sum)
        colnames(stepsPerDay) <- c("date", "steps")  
        
        # draw the histogram with the specified number of bins
        hist(stepsPerDay$steps, 
             col = 'darkgray', 
             border = 'white', 
             main = "Total number of steps taken per day", 
             xlab = "Number of steps")
    })
    
})