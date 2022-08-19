library(shiny)

shinyUI(fluidPage(

    titlePanel("Sextuple Threat"),

    mainPanel(
        tabsetPanel(
            tabPanel("Summary",
                     tableOutput("t1"),
                     uiOutput("link")
            ),
            tabPanel("Breakdown",
                     tableOutput("t2"),
                     tableOutput("t3")
            )
        )
    )
))
