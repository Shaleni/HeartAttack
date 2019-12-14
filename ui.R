library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Serious as a Heart Attack"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                'input.tab === "About"',
                h4("Welcome!"),
                h5("This is where controls will be located on other tabs.")
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
                    h4("Data"),
                    p("This dataset contains information on patients who have suffered heart attacks in the past. There is information on 132 patients."),
                    p("The data can be found at the ",
                      a("UCI Machine Learning Library", 
                        href="https://archive.ics.uci.edu/ml/datasets/Echocardiogram"),"."),
                    h4("App"),
                    p("This application contains several tabs for exploring and modeling this dataset. Since the dataset contains missing values, the MICE package in R has been used to impute the data, and all models/plots/exports will be of the imputed data. The raw data can be obtained from the link above."),
                    p("There are tabs for summarizing the data numerically and graphically, a view of PCA on the data, several types of supervised learning models and their applications to the data, as well as a tab in which the data can be explored."),
                    p("To the left of the screen there is a sidebar that contains controls that can be used to adjust and interact with the information on each of the tabs."),
                    p("Enjoy!")
                ),
                tabPanel("Summaries",
                    h4("Missing Values"),
                    plotOutput("missingPlot"),
                    
                    h4("Pairs"),
                    plotOutput("pairsPlot")
                ),
                tabPanel("PCA",
                    plotOutput("screePlot")
                ),
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
