library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(RColorBrewer)
library(shinyjs)
library(V8)
library(mathjaxr)
library(wordcloud2)
library(plotly)

# Shinydashboard information here https://rstudio.github.io/shinydashboard/index.html
# Icons sourced from https://fontawesome.com/icons?d=gallery&m=free

# Call data generated from save function within the "README.RMD" file
load("pumpkinData.RData")

dashboardPage(
    
    # Set color theme
    skin="red",
    
    #
    ##
    ### HEADER
    ##
    #
    
    dashboardHeader(title = "NCSU Pumpkin Study",titleWidth = 250),
    
    #
    ##
    ### SIDEBAR
    ##
    #
    
    dashboardSidebar(
        width = 250,
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("info")),
            menuItem("Data Exploration", tabName = "exploration", icon = icon("binoculars")),
            menuItem("Modeling", tabName = "model", icon = icon("gears"),
                     menuSubItem("Generalized Linear Model", tabName = "modelGLM")
            ),
            menuItem("Simulation", tabName = "sim", icon = icon("seedling")),
            radioButtons("radioStudySelect","Study Selection",
                         choiceNames=list("Spacing","Nitrogen"),
                         choiceValues=list("Spacing","Nitrogen")
            ),
            radioButtons("radioColor","Pumpkin Color",
                         choiceNames=list("All","Green","Orange"),
                         choiceValues=list("All","Green","Orange")
            ),
            conditionalPanel(condition = "input.radioStudySelect == 'spacing'",
                             sliderInput("sliderYear", "Growing Season",min=2020, max=2021,value=c(2020,2021),step=1,sep="")
            )
        )
    ),
    
    #
    ##
    ### BODY
    ##
    #
    
    dashboardBody(
        
        tabItems(
            
            #####
            ##### INFORMATION
            #####
            tabItem(tabName = "info",
                    fluidRow(
                        column(width=6,
                               box(title = "Information", footer = "Data Last Refreshed: 7-April-2022",status="danger",width=NULL,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Overview", uiOutput("infoOver"))
                                   )
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(title = "Word Cloud of Teams by Wins",status="danger",width=NULL
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            ),
            
            #####
            ##### DATA EXPLORATION
            #####
            tabItem(tabName="exploration",
                    fluidRow(
                        column(width = 5,
                               box(title="Pumpkin Summary",status="danger",width=NULL,
                                   "Basic summary statistics and visualizations from the pumpkins data set are provided."),
                               box(title="Numeric Summaries",status="danger",width=NULL,
                                   tabsetPanel(type="tabs",
                                               tabPanel("Summary",uiOutput("gsSumStats"))
                                   ))
                        ), # end column
                        column(width = 7,
                               box(title="Graphical Summaries",status="danger",width=NULL,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Weight Box Plot",plotlyOutput("gsBoxWeightTrt"))
                                   ))
                        ) # end column
                    ), # end fluidRow
                    fluidRow(
                        column(width = 12,
                               box(title="Pumpkin Summary Data Set",status="danger",
                                   div(style='overflow-x: scroll',DT::dataTableOutput("tablePumpkinExplore")),width=NULL)
                        ) # end column
                    ) # end fluidRow
            ),
            
            #####
            ##### MODELING
            #####
            tabItem(tabName = "model",
                    fluidRow(
                        column(width=6,
                               box(title = "Information", footer = "Data Last Refreshed: 7-April-2022",status="danger",width=NULL,
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(title = "Word Cloud of Teams by Wins",status="danger",width=NULL
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            ),
            
            #####
            ##### SIMULATION
            #####
            tabItem(tabName = "sim",
                    fluidRow(
                        column(width=6,
                               box(title = "Information", footer = "Data Last Refreshed: 7-April-2022",status="danger",width=NULL,
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(title = "Word Cloud of Teams by Wins",status="danger",width=NULL
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            )
            
        ) # END OF tabItems(
        
    ) # END of dashboardBody
    
) # END OF dashboardPage
