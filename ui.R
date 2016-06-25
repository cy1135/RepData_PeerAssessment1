# todo: add imputed selection option
#

library(shiny)

fluidPage(
    # app title
    titlePanel("Total Number of Steps Taken per Day"),
    
    # sidebar with slider input for number of days
    sidebarLayout(
        sidebarPanel(
            sliderInput("days",
                        "Number of days:",
                        min = 1,
                        max = 60,
                        value = 1)
           #selectInput("imputed", "Choose imputed:",
            #            choices = c("none", "imputed"))
        ),
    
        # plot of generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)