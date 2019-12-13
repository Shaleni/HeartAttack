library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Heart Attack"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                'input.tab === "About"',
                h4("about")
            ),
            conditionalPanel(
                'input.tab === "Summaries"',
                h4("Summaries"),
                
                uiOutput('pairOpts')
            ),
            conditionalPanel(
                'input.tab === "Clustering"',
                h4("Clustering")
            ),
            conditionalPanel(
                'input.tab === "Models"',
                h4("Models"),
                helpText("Click the column header to sort a column.")
            ),
            conditionalPanel(
                'input.tab === "Raw Data"',
                h4("Raw Data"),
                helpText("Click the column header to sort a column."),
            )

        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type="tabs", id = 'tab',
                tabPanel("About",
                    h4("Source: ")
                         ),
                tabPanel("Summaries",
                    h4("Missing Values"),
                    plotOutput("missingPlot"),
                    
                    h4("Pairs"),
                    plotOutput("pairsPlot")
                         ),
                tabPanel("Clustering"),
                tabPanel("Models"),
                tabPanel("Raw Data",
                    h4("Raw Data"),
                    dataTableOutput('echo')
                )
            )
            
            
        )
    )
))
