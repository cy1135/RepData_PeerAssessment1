library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # get data
    unzip("activity.zip")
    activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
    nrow(activity)
    
    output$distPlot <- renderPlot({
        n <- input$days
        strdays <- "days"

        stepsPerDay <- aggregate(steps ~ cut(as.Date(date), as.character(paste(n, strdays, sep = " "))), activity, sum)
        colnames(stepsPerDay) <- c("date", "steps")  
        
        sliderValue <- reactive({
            nmax <- nrow(stepsPerDay)
            updateSliderInput(session, "days", value = nmax, min = 1, max = nmax, step = 1)
            
        })
        
        # draw the histogram with the specified number of bins
        hist(stepsPerDay$steps, 
             col = 'darkgray', 
             border = 'white', 
             main = "Total number of steps taken per day", 
             xlab = "Number of steps")
    })
    
})