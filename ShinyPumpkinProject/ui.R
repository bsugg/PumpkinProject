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

# Utilize shinyjs package for javascript code to collapse boxes on command
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

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
            menuItem("Data Exploration", tabName = "exploration", icon = icon("chalkboard-teacher")),
            menuItem("Modeling", tabName = "model", icon = icon("code-branch"),
                     menuSubItem("Generalized Linear Model", tabName = "modelGLM"),
                     menuSubItem("Ensemble Model", tabName = "modelRF")
            ),
            radioButtons("radioStudySelect","Study Selection",
                         choiceNames=list("Spacing","Nitrogen","Leaf"),
                         choiceValues=list("spacing","nitrogen","leaf")
            ),
            conditionalPanel(condition = "input.radioStudySelect == 'spacing'",
                             sliderInput("sliderYear", "Season",min=2020, max=2021,value=c(2020,2021),step=1,sep="")
            ),
            conditionalPanel(condition = "input.radioStudySelect == 'nitrogen'",
                             sliderInput("sliderYear", "Season",min=2021, max=2022,value=c(2021,2022),step=1,sep="")
            ),
            conditionalPanel(condition = "input.radioStudySelect == 'leaf'",
                             sliderInput("sliderYear", "Season",min=2021, max=2022,value=c(2021,2022),step=1,sep="")
            ),
            uiOutput("fruitColor"),
            radioButtons("radioColor","Pumpkin Color",
                         choiceNames=list("All","Green","Orange"),
                         choiceValues=list("all","green","orange")
            )
        )
    ),
    
    #
    ##
    ### BODY
    ##
    #
    
    dashboardBody(
        
    ) # END of dashboardBody
    
) # END OF dashboardPage
