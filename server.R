library(shiny)
library(tidyverse)
library(DT)
library(lmtest)
library(VIM)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #import data
    echo = read_csv("EKGdata.csv")


    ########## Summaries Tab ############
    output$pairOpts <- renderUI({
        checkboxGroupInput("pairOpts", "Columns in data to compare:",
                           names(select_if(echo,"is.numeric")),
                           selected=names(select_if(echo,"is.numeric")))
    })
    
    #output$formula <- renderText({
        pairVars <- reactive({ 
        p <- paste(input$pairOpts,collapse="+") 
        p <- paste("~",p)
        p <- formula(p)
        })
   # })

    output$pairsPlot <- renderPlot({
        pairs(pairVars(), data=echo)
    })
    
    output$missingPlot <- renderPlot({
        aggr(echo, col=c('navyblue','red'),
             numbers=TRUE, sortVars=TRUE,
             labels=names(echo), cex.axis=.7,
             gap=3, ylab=c("Missing data","Pattern"))
    })
    
    ########## Data Tab #################
    output$echo <- renderDataTable(echo)

})
