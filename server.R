library(shiny)
library(tidyverse)
library(DT)
library(lmtest)
library(VIM)
library(mice)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #import data
    echo_raw = read_csv("EKGdata.csv")
    
    #imputed with mice
    echo <- mice(echo_raw)$data
    
    #other cleanup
    echo[is.na(echo)] <- 0
    echo$AliveAtOne
    #book-keeping
    sapply(echo,class)
    EPSS <- as.numeric(levels(echo[,5]))[echo[,5]]
    Survival <- as.numeric(levels(echo[,9]))[echo[,9]]
    AliveAtOne <- as.factor(echo$AliveAtOne)
    PericardialEffusion <- as.factor(echo$PericardialEffusion)
    echo$EPSS <- EPSS
    echo$Survival <- Survival
    echo$AliveAtOne <- AliveAtOne
    echo$PericardialEffusion <- PericardialEffusion
    levels(echo$AliveAtOne) <- c("Dead", "Alive")

    ########## Summaries Tab ############
    output$pairOpts <- renderUI({
        checkboxGroupInput("pairOpts", "Columns in data to compare:",
                           names(select_if(echo,"is.numeric")),
                           selected=names(select_if(echo,"is.numeric")))
    })
    
    # output$formula <- renderText({
    #     p <- paste(input$pairOpts,collapse="+") 
    #     p <- paste("~",p)
    #     p
    # })
    
    pairVars <- reactive({ 
        p <- paste(input$pairOpts,collapse="+") 
        p <- paste("~",p)
        p <- formula(p)
    })
    
    output$pairsPlot <- renderPlot({
        pairs(pairVars(), data=echo)
    })
    
    output$missingPlot <- renderPlot({
        aggr(echo_raw, col=c('navyblue','red'),
             numbers=TRUE, sortVars=TRUE,
             labels=names(echo_raw), cex.axis=.7,
             gap=3, ylab=c("Missing data","Pattern"))
    })
    
    ########## Modelling Tab #################
    
    # variables to select for the models
    output$modelOpts <- renderUI({
        checkboxGroupInput("modelOpts", "Columns in data to compare:",
                           setdiff(names(echo),c("Survival","StillAlive","AliveAtOne")),
                           selected=setdiff(names(echo),c("Survival","StillAlive","AliveAtOne")))
    })
    
    # logistic regression
    
    # classification tree
    
    # svm
    
    ########## Data Tab #################
    output$echo <- renderDataTable(echo)

})
