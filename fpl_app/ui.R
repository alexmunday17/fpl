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
            tabPanel("Overall",
                     tableOutput("t1")
            ),
            tabPanel("League Points",
                     tableOutput("t2")
            ),
            tabPanel("Total Points",
                     tableOutput("t3")
            ),
            tabPanel("1st Wildcard",
                     tableOutput("t4")
            ),
            tabPanel("2nd Wildcard",
                     tableOutput("t5")
            ),
            tabPanel("Triple Captain",
                     tableOutput("t6")
            ),
            tabPanel("Bench Boost",
                     tableOutput("t7")
            ),
            tabPanel("Free Hit",
                     tableOutput("t8")
            )
        )
    )
))
