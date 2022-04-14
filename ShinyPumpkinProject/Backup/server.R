library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(ragtop)
library(knitr)
library(class)
library(caret)
library(e1071)
library(randomForest)
library(gbm)
library(shinyjs)
library(V8)
library(mathjaxr)
library(plotly)
library(wordcloud2)
library(tm)
library(data.table)
library(autoplotly)
library(kableExtra)

# Call data generated from save function within the "README.RMD" file
load("pumpkinData.RData")

function(input, output, session) {
    
    ###################### REACTIVE ELEMENTS #####################################
    
    #####
    ##### STUDY SELECTION
    #####
    
    studyFinder <- reactive({
        # Read user selected study and assign corresponding data set
        if (input$radioStudySelect == "Spacing"){
            pumpkins <- spacingData
        } else {
            if (input$radioStudySelect == "Nitrogen"){
                pumpkins <- nitrogenData
            } else {
                pumpkins <- tibble()
            }
        }
    })
    
    #####
    ##### SEASON SLIDER VALUES
    #####
    

    
    #####
    ##### PUMPKIN COLOR
    #####
    
    colorFinder <- reactive({
        # Filter on user selected team - generates a single record with team attributes
        if (input$radioColor == "All"){
            pumpkins <- studyFinder()
        } else {
            pumpkins <- studyFinder() %>% filter(color == input$radioColor)
        }
    })
    
    #####
    ##### PUMPKINS
    #####
    
    pumpkins <- reactive({
        colorFinder()
    })
    
    ###################### RENDER OUTPUT ################################################
    
    #####
    ##### UI ACTIONS
    #####
    
    ###
    ### SIDEBAR
    ###
    
    #####
    ##### DATA EXPLORATION
    #####
    
    ###
    ### PUMPKIN SUMMARY
    ###
    
    # TEXT
    
    # GRAPHS
    
    # Boxplot of weight by spacing dimension for orange pumpkins
    output$gsBoxWeightTrt <- renderPlotly({
        getPumpkins <- pumpkins()
        boxWeightTrt <- ggplot(getPumpkins, aes(x=spacingDim,y=weight,fill=spacingDim)) +
            geom_boxplot(varwidth = TRUE, alpha=0.2) +
            theme(legend.position="none") +
            labs(x="Treatment",
                 y="Weight (Pounds)")
        y <- ggplotly(boxWeightTrt)
        y
    })

    # TABLE
    
    output$tablePumpkinExplore <- DT::renderDataTable({
        getPumpkins <- pumpkins()
        customPrintName <- paste0(input$radioStudySelect," Study Pumpkin Summary")
        customFileName <- paste0(input$radioStudySelect," Study Pumpkin Summary")
        DT::datatable(getPumpkins,extensions = 'Buttons',
                      options = list(orderClasses = TRUE, pageLength = 5,dom = 'Blfrtip',
                                     lengthMenu = list(c(5,10,25,50,100,-1),c('5','10','25','50','100','All')),
                                     buttons = c(list(list(extend = 'copy', title= "")),
                                                 list(list(extend = 'print', title= customPrintName)),
                                                 list(list(extend = 'csv', filename= customFileName)),
                                                 list(list(extend = 'excel',filename= customFileName,title= "")),
                                                 list(list(extend = 'pdf', filename= customFileName,title= customPrintName,orientation='landscape',pageSize= 'LEGAL'))
                                     )
                      )
        )
    })
    
} # END OF function
