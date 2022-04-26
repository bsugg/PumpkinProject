library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(RColorBrewer)
library(shinyjs)
library(V8)
library(wordcloud2)
library(plotly)
library(mathjaxr)

# radioButtons("radioStudySelect","Study Selection",
#              choiceNames=list("Spacing","Nitrogen","Leaf"),
#              choiceValues=list("Spacing","Nitrogen","Leaf")
# )

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
        sidebarMenu(selectInput(inputId = "studySelection",
                                label = "Study Selection",
                                choices = c("Plant Spacing", "Nitrogen Level", "Leaf Analysis"))),
        sidebarMenuOutput("menu"),
        sidebarMenuOutput("yearMenu"),
        sidebarMenuOutput("colorMenu"),
        uiOutput("colorImage")
    ),
    
    #
    ##
    ### BODY
    ##
    #
    
    dashboardBody(
        
        # SPACING ITEMS
        tabItems(
            
            #####
            ##### INFORMATION - SPACING
            #####
            tabItem(tabName = "infoSpaceStudy",
                    fluidRow(
                        column(width=6,
                               box(title = "Information - Research Study", footer = "Data Last Refreshed: 21-April-2022",status="danger",width=NULL,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Introduction", uiOutput("infoSpaceStudyIntro")),
                                               tabPanel("Glossary", uiOutput("infoSpaceStudyGloss")),
                                               tabPanel("Contact", uiOutput("infoSpaceStudyContact")),
                                               tabPanel("Download Data", uiOutput("infoSpaceStudyDown"))
                                   )
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(status="danger",width=NULL,
                                   tags$img(src="images/spacing/infoSpace.JPG",
                                            width="100%",style="display: block; margin-left: auto; margin-right: auto;")
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            ),
            
            tabItem(tabName = "infoSpaceTreat",
                    fluidRow(
                        column(width=7,
                               fluidRow(  # A row within another column has its own total width of 12
                                   column(width=4,
                                          box(status="danger",width=NULL,
                                              selectInput(inputId = "infoSpaceSelectArea",
                                                          label = "Plant Area",
                                                          choices = c("10 sq. ft."="10",
                                                                      "20 sq. ft."="20",
                                                                      "30 sq. ft."="30",
                                                                      "40 sq. ft."="40")
                                              ),
                                          )
                                   ), # END COLUMN
                                   column(width=8, #
                                          box(status="danger",width=NULL,
                                              "Aerial images of plots by the possible Spacing Dimensions for the selected Plant Area can be seen on the right. 
                                              These show the plots during the vegetative growth phase (top image) and the harvest phase (bottom image)."),
                                   ) # END COLUMN
                               ), # END FLUID ROW
                               fluidRow( # A row within another column has its own total width of 12
                                   column(width=12,
                                          box(status="danger",width=NULL,
                                              tableOutput("infoSpaceAreaTable")
                                          )
                                          
                                   ), # END COLUMN
                               ) # END FLUID ROW
                        ), # end column
                        column(width=5,
                               box(status="danger",width=NULL,
                                   uiOutput("infoSpaceAreaImage") # This is reactive and controlled in server
                               ),
                               box(status="danger",width=NULL,
                                   uiOutput("infoSpaceHarvestImage") # This is reactive and controlled in server
                               )
                        ) # END COLUMN
                    ), # END FLUID ROW

            ),
            
            #####
            ##### DATA EXPLORATION - SPACING CHARTS FOR QUANTITY
            #####
            tabItem(tabName="exploreSpaceQuantity",
                    fluidRow(
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   selectInput(inputId = "exSpaceQtyPlot",
                                               label = "Count By",
                                               choices = c("Average per Plot"="averagePlot",
                                                           "Scaled Average per Plot"="scaledAveragePlot",
                                                           "Scaled Average per Acre"="scaledAverageAcre",
                                                           "Total"="totalPlot",
                                                           "Scaled Total"="scaledTotalPlot"
                                               )
                                   )
                               )
                        ), # end column
                        column(width = 4,
                               box(title="What is this...",status="danger",width=NULL,
                                   "Explain it to me.")
                        ), # end column
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   selectInput(inputId = "exSpaceQtyGroup",
                                               label = "Group By",
                                               choices = c("Plant Area"="plantArea",
                                                           "Spacing Dimension"="spacingDim",
                                                           "Between Row"="betweenRow",
                                                           "In Row"="inRow",
                                                           "Year"="year",
                                                           "Plot"="plot"
                                               )
                                   ),
                               )
                        ), # end column
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   selectInput(inputId = "exSpaceQtyArea",
                                               label = "Plant Area",
                                               choices = c("All",
                                                           "10 sq. ft."="10",
                                                           "20 sq. ft."="20",
                                                           "30 sq. ft."="30",
                                                           "40 sq. ft."="40")
                                   )
                               )
                        ), # end column
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   uiOutput("exSpaceQtyDim") # This is reactive and controlled in server
                               )
                        ) # end column
                    ), # end fluidRow
                    fluidRow(
                        column(width = 6,
                               box(plotlyOutput("exSpaceQtyDia"),status="danger",width=NULL)
                        ), # end column
                        column(width = 6,
                               box(plotlyOutput("exSpaceQtyBar"),status="danger",width=NULL)
                        ) # end column
                    ), # end fluidRow
                    fluidRow(
                        column(width = 6,
                               box(plotlyOutput("exSpaceQtyDiaColor"),status="danger",width=NULL)
                        ), # end column
                        column(width = 6,
                               box(plotlyOutput("exSpaceQtyColor"),status="danger",width=NULL)
                        ) # end column
                    ), # end fluidRow
            ),
            
            #####
            ##### DATA EXPLORATION - SPACING CHARTS FOR METRICS
            #####
            tabItem(tabName="exploreSpaceMetrics",
                    fluidRow(
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   selectInput(inputId = "exSpaceMetPlot",
                                               label = "Chart Type",
                                               choices = c("Density"="density",
                                                           "Histogram"="histogram",
                                                           "Box Plot"="boxplot",
                                                           "Scatter"="scatter"
                                                           )
                                   )
                               )
                        ), # end column
                        column(width = 4,
                               box(title="What is this...",status="danger",width=NULL,
                                   "Explain it to me.")
                        ), # end column
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   selectInput(inputId = "exSpaceMetGroup",
                                               label = "Group By",
                                               choices = c("Plant Area"="plantArea",
                                                           "Spacing Dimension"="spacingDim",
                                                           "Between Row"="betweenRow",
                                                           "In Row"="inRow",
                                                           "Year"="year",
                                                           "Plot"="plot",
                                                           "Color"="color")
                                               ),
                               )
                        ), # end column
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   selectInput(inputId = "exSpaceMetArea",
                                               label = "Plant Area",
                                               choices = c("All",
                                                           "10 sq. ft."="10",
                                                           "20 sq. ft."="20",
                                                           "30 sq. ft."="30",
                                                           "40 sq. ft."="40")
                                   )
                               )
                        ), # end column
                        column(width = 2,
                               box(status="danger",width=NULL,
                                   uiOutput("exSpaceMetDim") # This is reactive and controlled in server
                               )
                        ) # end column
                    ), # end fluidRow
                    fluidRow(
                        column(width = 6,
                               box(plotlyOutput("exSpaceMetVol"),status="danger",width=NULL)
                        ), # end column
                        column(width = 6,
                               box(plotlyOutput("exSpaceMetWei"),status="danger",width=NULL)
                        ) # end column
                    ), # end fluidRow
                    fluidRow(
                        column(width = 6,
                               box(plotlyOutput("exSpaceMetDia"),status="danger",width=NULL)
                        ), # end column
                        column(width = 6,
                               box(plotlyOutput("exSpaceMetLen"),status="danger",width=NULL)
                        ) # end column
                    ), # end fluidRow
            ),
            
            #####
            ##### ANALYSIS - SPACING
            #####
            tabItem(tabName = "analysisSpace",
                    fluidRow(
                        column(width = 7,
                               fluidRow(
                                   column(width = 8,
                                          box(title = "Significance Test",status="danger",width=NULL,
                                              "A two-way analysis of variance (ANOVA) with interaction is used to estimate how the mean of a quantitative response variable changes according to the levels of two independent, categorical variables and their interaction. Select a response variable to get started."
                                          ),
                                   ), # END COLUMN
                                   column(width = 4,
                                          box(status="danger",width=NULL,
                                              selectInput(inputId = "aSpaceResponse",
                                                          label = withMathJax("$$Response Mean, Y_{ijk} $$"),
                                                          choices = c("Volume"="volumeEllipsoid",
                                                                      "Diameter"="diameter",
                                                                      "Length"="length",
                                                                      "Weight"="weight"
                                                          )
                                              )
                                          )
                                   ) # END COLUMN
                               ), # END FLUID ROW
                               fluidRow( # A row within another column has its own total width of 12
                                   column(width=4,
                                          valueBoxOutput("aSpaceAnovaArea",
                                                         width=NULL)
                                   ), # END COLUMN
                                   column(width=4,
                                          valueBoxOutput("aSpaceAnovaBetween",
                                                         width=NULL)
                                   ), # END COLUMN
                                   column(width=4,
                                          valueBoxOutput("aSpaceAnovaInteract",
                                                         width=NULL)
                                   ) # END COLUMN
                               ), # END FLUID ROW
                        ), # END COLUMN
                        column(width = 5,
                               box(title="Model for a Two-Way ANOVA with Interaction",status="danger",width=NULL,
                                   withMathJax("$$ Y_{ijk} = \\mu + \\alpha_{i} + \\beta_{j} + (\\alpha\\beta)_{ij} + \\epsilon_{ijk} $$"),
                                   withMathJax("$$ \\mu = Mean $$"),
                                   withMathJax("$$ \\alpha = Effect Plant Area, i = 10, 20, 30, 40 $$"),
                                   withMathJax("$$ \\beta = Effect Between Row, j = 5, 10 $$"),
                                   withMathJax("$$ \\alpha\\beta = Effect Interaction $$"),
                                   withMathJax("$$ \\epsilon = Effect Random Error $$")
                               )
                        ) # END COLUMN
                    ), # END FLUID ROW
                           fluidRow(
                               column(width=7,
                                      box(status="danger",width=NULL,
                                          tabsetPanel(type = "tabs",
                                                      tabPanel("Two-Way ANOVA Results",verbatimTextOutput("aSpaceAnovaTable")),
                                                      tabPanel("Interaction Plot",plotlyOutput("aSpaceIntPlot")),
                                                      tabPanel("Coefficients",tableOutput("aSpaceCoefTable")),
                                                      tabPanel("Tukey Results",verbatimTextOutput("aSpaceTukeyTable"))
                                          )
                                      )
                               ), # END COLUMN
                               column(width=5,
                                      
                                      box(title = "Significance of Plant Area and Between Row",status="danger",width=NULL,
                                          uiOutput("aSpaceSigResult")
                                      ),
                               ) # END COLUMN
                           ) # END FLUID ROW
            ),
                               # box(status="danger",width=NULL,
                               #     uiOutput("aSpaceResponseImage") # This is reactive and controlled in server
                               # ),
                               # box(status="danger",width=NULL,
                               #     selectInput(inputId = "aSpacePredictor",
                               #                 label = "Image Display",
                               #                 choices = c("Plant Area"="plantArea",
                               #                             "Spacing Dimension"="spacingDim",
                               #                             "Between Row"="betweenRow",
                               #                             "In Row"="inRow"
                               #                 )
                               #     ),
                               # ),
                               # box(status="danger",width=NULL,
                               #     tags$img(src="images/spacing/harvestPumpkins.JPG",
                               #              width="100%",style="display: block; margin-left: auto; margin-right: auto;")
                               # ),

            
            #####
            ##### SIMULATION - SPACING
            #####
            tabItem(tabName = "simSpace",
                    fluidRow(
                        column(width=6,
                               box(title = "Spacing Simulation",status="danger",width=NULL,
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(title = "Spacing Simulation",status="danger",width=NULL
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            ),
            
            #####
            ##### INFORMATION - NITROGEN
            #####
            tabItem(tabName = "infoNit",
                    fluidRow(
                        column(width=6,
                               box(title = "Information", footer = "Data Last Refreshed: 21-April-2022",status="danger",width=NULL,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Overview", uiOutput("infoNitOver")),
                                               tabPanel("Download Data", uiOutput("infoNitDown"))
                                   )
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(title = "Nitrogen",status="danger",width=NULL
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            ),
            
            #####
            ##### DATA EXPLORATION - NITROGEN
            #####
            tabItem(tabName="exploreNit",
                    fluidRow(
                        column(width = 5,
                               box(title = "Nitrogen Exploration",status="danger",width=NULL,
                               )
                        ), # end column
                        column(width = 7,
                               box(title = "Nitrogen Exploration",status="danger",width=NULL,
                               )
                        ) # end column
                    ), # end fluidRow
                    fluidRow(
                        column(width = 12,
                               
                        ) # end column
                    ) # end fluidRow
            ),
            
            #####
            ##### ANALYSIS - NITROGEN
            #####
            tabItem(tabName = "analysisNit",
                    fluidRow(
                        column(width=6,
                               box(title = "Nitrogen Analysis", footer = "Data Last Refreshed: 21-April-2022",status="danger",width=NULL,
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(title = "Nitrogen Analysis",status="danger",width=NULL
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            ),
            
            #####
            ##### INFORMATION - LEAF
            #####
            tabItem(tabName = "infoLeaf",
                    fluidRow(
                        column(width=6,
                               box(title = "Leaf Information", footer = "Data Last Refreshed: 21-April-2022",status="danger",width=NULL,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Overview", uiOutput("infoLeafOver")),
                                               tabPanel("Download Data", uiOutput("infoLeafDown"))
                                   )
                               )
                        ), # END COLUMN
                        column(width=6,
                               box(title = "Leaf",status="danger",width=NULL
                               ),
                        ), # END COLUMN
                    ) # END FLUID ROW
            ),
            
            #####
            ##### DATA EXPLORATION - LEAF
            #####
            tabItem(tabName="exploreLeaf",
                    fluidRow(
                        column(width = 5,
                               box(title = "Leaf Exploration",status="danger",width=NULL,
                               )
                        ), # end column
                        column(width = 7,
                               box(title = "Leaf Exploration",status="danger",width=NULL,
                               )
                        ) # end column
                    ), # end fluidRow
                    fluidRow(
                        column(width = 12,
                               
                        ) # end column
                    ) # end fluidRow
            )
            
        ) # END OF tabItems(
        
    ) # END of dashboardBody
    
) # END OF dashboardPage
