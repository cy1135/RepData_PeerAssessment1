library(shiny)
library(data.table)

shinyServer(function(input, output, session){
    sliderValue <- reactive({   
    # get data
    unzip("activity.zip")
    activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
    nrow(activity)
    # touch up date for dataset
    dt <- data.table(steps = activity$steps, date = as.Date(activity$date), interval = activity$interval)
    
    n = input$days
    strdays = "days"
    
    #stepsPerDay <- aggregate(steps ~ date, dt, sum)
    #stepsPerDay <- aggregate(steps ~ cut(date, "8 days"), dt, sum)

    
    # add index
    #stepsPerDay <- data.table(stepsPerDay, index = 1:nrow(stepsPerDay))
    
    # update max number of days based on aggregated
    observe({
        nmax <- nrow(stepsPerDay)
        updateSliderInput(session, "days", value = nmax, min = 1, max = nmax, step = 1)
        stepsPerDay <- aggregate(steps ~ cut(date, as.character(paste(n, strdays, sep = " "))), dt, sum)
        colnames(stepsPerDay) <- c("date", "steps")
    })
    
    output$distPlot <- renderPlot({
        # aggregate steps by date
        #stepsPerDay <- aggregate(steps ~ cut(as.Date(date, format = "%d/%m/%Y"), paste(input$days, "of days", sep = " ")), activity, sum)
        
        # histogram
        hist(stepsPerDay$steps, main = "Total number of steps taken per day", xlab = "Number of steps")
    })
    
    })
})