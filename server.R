library(shiny)
library(tidyverse)
library(DT)
library(lmtest)
library(VIM)
library(mice)
library(caret)
library(tree)
library(e1071)
library(factoextra)

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
    
    plottest <- reactive({
        if ( "Missing" %in% input$selectplot) return(aggr(echo_raw, col=c('navyblue','red'),
                                                       numbers=TRUE, sortVars=TRUE,
                                                       labels=names(echo_raw), cex.axis=.7,
                                                       gap=3, ylab=c("Missing data","Pattern")))
        if ( "Pairs" %in% input$selectplot) return(pairs(pairVars(), data=echo))
    })
    
    output$ExportPlot <- downloadHandler(
            filename = function() {
              paste("output.png")
            },
            content = function(file) {
                png(filename="pairs.png")
                print(plottest())
                dev.off()
            },
            contentType = 'image/png'
    )
    
    output$plots <- renderPlot({   
        plottest()
    }) 
    
    output$tbl1 <- renderUI({
        opts <- names(select_if(echo,"is.numeric"))
        names(opts) <- names(select_if(echo,"is.numeric"))
        
        selectInput("tbl1", label = h4("Select First Variable"),
                    choices = names(echo))
    })

    output$tbl2 <- renderUI({
        opts <- names(select_if(echo,"is.numeric"))
        names(opts) <- names(select_if(echo,"is.numeric"))
        
        selectInput("tbl2", label = h4("Select Second Variable"),
                    choices = names(echo))
    })
    
    output$numeric <- renderTable({
        form<-formula(paste('~',input$tbl1,'+',input$tbl2))
      xtabs(form,data=echo)
    })
    
    
    ########## Clustering Tab #################
    
    # variables to select for the PCA
    output$pcaOpts <- renderUI({
        checkboxGroupInput("pcaOpts", "Variables to include in PCA:",
                           setdiff(names(echo),c("Survival","StillAlive","AliveAtOne","PericardialEffusion")),
                           selected=setdiff(names(echo),c("Survival","StillAlive","AliveAtOne","PericardialEffusion")))
    })
    
    # formula to use in the pca
    pcaForm <-  reactive({
        form <- paste(input$pcaOpts,collapse="+")
        form <- paste("~",form)
        form <- formula(form)
        
        return(form)
    })
    
    #PCA Plots
    output$biPlot <- renderPlot({
        echo_num <- select_if(echo,"is.numeric")
        PCs <- prcomp(pcaForm(),echo_num[complete.cases(echo_num), ] ,center=T,scale=T)
        fviz_pca_biplot(PCs, repel = TRUE,
                        col.var = "#2E9FDF", # Variables color
                        col.ind = "#696969"  # Individuals color
        )
    })
    
    output$screePlot <- renderPlot({
        echo_num <- select_if(echo,"is.numeric")
        PCs <- prcomp(pcaForm(),echo_num[complete.cases(echo_num), ] ,center=T,scale=T)
        fviz_eig(PCs)
    })
    
    ########## Modelling Tab #################
    
    # variables to select for the models
    output$model1Opts <- renderUI({
        checkboxGroupInput("model1Opts", "Variables to include in Models:",
                           setdiff(names(echo),c("Survival","StillAlive","AliveAtOne")),
                           selected=setdiff(names(echo),c("Survival","StillAlive","AliveAtOne")))
    })
    
    # formula to use in the models
    model1Form <-  reactive({
        form <- paste(input$model1Opts,collapse="+")
        form <- paste("AliveAtOne~",form)
        form <- formula(form)
        
        return(form)
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
    
    output$logregSummary <- renderPrint({
        log.reg <- glm(model1Form(),family=binomial,data=train)
        summary(log.reg)
    })
    
    output$logPred <- renderText({
        log.reg <- glm(model1Form(),family=binomial,data=train)
        newData <- data.frame(EPSS = mean(echo$EPSS,na.rm=T),
                              AgeAtMI = input$pred,
                              LVDD = mean(echo$LVDD, na.rm=T),
                              PericardialEffusion = names(table(echo$PericardialEffusion)[which.max(table(echo$PericardialEffusion))]),
                              WallMotionIndex = mean(echo$WallMotionIndex, na.rm=T),
                              FractionalShortening = mean(echo$FractionalShortening,na.rm=T))
        paste("Probability of dying within a year:", round(predict(log.reg,newData,type="response"),4))
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
    
    output$treePred <- renderText({
        tree.echo=tree(model1Form(),echo)
        newData <- data.frame(EPSS = mean(echo$EPSS,na.rm=T),
                              AgeAtMI = input$pred,
                              LVDD = mean(echo$LVDD, na.rm=T),
                              PericardialEffusion = names(table(echo$PericardialEffusion)[which.max(table(echo$PericardialEffusion))]),
                              WallMotionIndex = mean(echo$WallMotionIndex, na.rm=T),
                              FractionalShortening = mean(echo$FractionalShortening,na.rm=T))
        paste("Dead or alive within a year?:", predict(tree.echo,newData,type="class"))
    })
    
    ########## Data Tab #################
    output$echo <- renderDataTable(echo)

})
