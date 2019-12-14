library(shiny)
library(tidyverse)
library(DT)
library(lmtest)
library(VIM)
library(mice)
library(caret)
library(tree)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #import data
    echo_raw = read_csv("EKGdata.csv")
    
    #imputed with mice
    echo <- mice(echo_raw)$data
    
    #other cleanup
    echo$EPSS <- as.numeric(echo$EPSS)
    echo$EPSS[is.na(echo$EPSS)] <- mean(echo$EPSS,na.rm=T)
    echo$Survival <- as.numeric(echo$Survival)
    echo$Survival[is.na(echo$Survival)] <- mean(echo$Survival,na.rm=T)
    echo$AliveAtOne <- as.factor(echo$AliveAtOne)
    echo$PericardialEffusion <- as.factor(echo$PericardialEffusion)
    levels(echo$AliveAtOne) <- c("Dead", "Alive")
    
    # train and test
    a <- createDataPartition(echo$AliveAtOne, p = 0.8, list=F)
    train <- echo[a,]
    test <- echo[-a,]

    ########## Summaries Tab ############
    output$pairOpts <- renderUI({
        checkboxGroupInput("pairOpts", "Columns in data to compare:",
                           names(select_if(echo,"is.numeric")),
                           selected=names(select_if(echo,"is.numeric")))
    })
    
    pairVars <- reactive({ 
        p <- paste(input$pairOpts,collapse="+") 
        p <- paste("~",p)
        p <- formula(p)
        
        return(p)
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
    output$model1Opts <- renderUI({
        checkboxGroupInput("modelOpts", "Variables to include in Models:",
                           setdiff(names(echo),c("Survival","StillAlive","AliveAtOne")),
                           selected=setdiff(names(echo),c("Survival","StillAlive","AliveAtOne")))
    })
    
    # formula to use in the models
    model1Form <-  reactive({
        form <- paste(input$modelOpts,collapse="+")
        form <- paste("AliveAtOne~",form)
        form <- formula(form)
        
        return(form)
    })
    
    output$formula <- renderText({
        form <- paste(input$modelOpts,collapse="+")
        form <- paste("AliveAtOne~",form)
        form
    })
    
    # logistic regression
    output$logAcc <- renderText({
        log.reg <- glm(model1Form(),family=binomial,data=train)
        fit.log <- predict(log.reg,test,type="response")
        fitted.results <- ifelse(fit.log > 0.5,1,0)
        actual.results <- ifelse(test$AliveAtOne == "Dead",0,1)
        misClasificError <- mean(fitted.results != actual.results, na.rm=T)
    
        paste("Accuracy: ", round((1-misClasificError)*100,2))
    })
    
    # classification tree
    output$treePlot <- renderPlot({
        tree.echo=tree(model1Form(),echo)
        plot(tree.echo)
        text(tree.echo,pretty =0)
    })
    
    output$treeAcc <- renderText({
        tree.echo=tree(model1Form(),echo)
        misclass <- summary(tree.echo)$misclass
        paste("Accuracy: ", round((misclass[1]/misclass[2])*100,2))
    })
    
    # svm
    
    
    ########## Data Tab #################
    output$echo <- renderDataTable(echo)

})
