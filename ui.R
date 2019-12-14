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
                uiOutput('model1Opts')
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
                tabPanel("Models",
                        textOutput("formula"),
                        h4("Logistic Regression"),
                        textOutput("logAcc"),
                        h4("Classification Tree"),
                        plotOutput("treePlot"),
                        textOutput("treeAcc")
                ),
                tabPanel("Raw Data",
                    h4("Raw Data"),
                    dataTableOutput('echo')
                )
            )
            
            
        )
    )
))
