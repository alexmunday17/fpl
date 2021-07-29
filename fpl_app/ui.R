#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("FPL"),

    # Sidebar with a slider input for number of bins
    mainPanel(
        tabsetPanel(
            tabPanel("Summary",
                     tableOutput("t1")
            ),
            tabPanel("Breakdown",
                     tableOutput("t2"),
                     tableOutput("t3")
            )
        )
    )
))
