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
library(gbm)
library(shinyjs)
library(V8)
library(plotly)
library(wordcloud2)
library(tm)
library(data.table)
library(autoplotly)
library(kableExtra)
library(scales)
library(mathjaxr)

# Call data generated from save function within the "README.RMD" file
load("pumpkinData.RData")

# Utilize shinyjs package for javascript code to collapse boxes on command
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

function(input, output, session) {
    
    ###################### GENERATE DYNAMIC MENU DEPENDING ON STUDY SELECTION #####################################
    
    # Whenever input$studySelection changes through user input then the output$menu adapts
    observeEvent(input$studySelection,{
        
        if(input$studySelection == "Plant Spacing"){
            output$yearMenu <- renderMenu({
                sidebarMenu(
                    selectInput(inputId = "yearSelection",
                                label = "Year",
                                choices = c("All","2020","2021")))
            })
            output$colorMenu <- renderMenu({
                sidebarMenu(
                    selectInput(inputId = "colorSelection",
                                label = "Pumpkin Color",
                                choices = c("All", "Orange", "Green")))
            })
            output$menu <- renderMenu({
                sidebarMenu(
                    menuItem("Information", tabName = "infoSpace", icon = icon("info"),
                             menuSubItem("Research Study",tabName = "infoSpaceStudy"),
                             menuSubItem("Treatments",tabName = "infoSpaceTreat")
                             ),
                    menuItem("Data Exploration", tabName = "exploreSpace", icon = icon("binoculars"),
                             menuSubItem("Charts for Quantity",tabName = "exploreSpaceQuantity"),
                             menuSubItem("Charts for Size Metrics",tabName = "exploreSpaceMetrics")
                             ),
                    menuItem("Analysis", tabName = "analysisSpace", icon = icon("gears")),
                    menuItem("Simulation", tabName = "simSpace", icon = icon("seedling"))
                )
            })
            output$colorImage <- renderUI({
                tags$img(src="images/allPumpkins.JPG",width=150,style="display: block; margin-left: auto; margin-right: auto;")
            })
        }
        
        if(input$studySelection == "Nitrogen Level"){
            output$yearMenu <- renderMenu({
                sidebarMenu(
                    selectInput(inputId = "yearSelection",
                                label = "Year",
                                choices = c("All","2021")))
            })
            output$colorMenu <- renderMenu({
                sidebarMenu(
                    selectInput(inputId = "colorSelection",
                                label = "Pumpkin Color",
                                choices = c("All", "Orange", "Green")))
            })
            output$menu <- renderMenu({
                sidebarMenu(
                    menuItem("Information", tabName = "infoNit", icon = icon("info")),
                    menuItem("Data Exploration", tabName = "exploreNit", icon = icon("binoculars")),
                    menuItem("Analysis", tabName = "analysisNit", icon = icon("gears"))
                )
            })
            output$colorImage <- renderUI({
                tags$img(src="images/allPumpkins.JPG",width=150,style="display: block; margin-left: auto; margin-right: auto;")
            })
        }
        
        if(input$studySelection == "Leaf Analysis"){
            output$yearMenu <- renderMenu({
                sidebarMenu(
                    selectInput(inputId = "yearSelection",
                                label = "Year",
                                choices = c("All","2021")))
            })
            output$colorMenu <- renderMenu({
                sidebarMenu()
            })
            output$menu <- renderMenu({
                sidebarMenu(
                    menuItem("Information", tabName = "infoLeaf", icon = icon("info")),
                    menuItem("Data Exploration", tabName = "exploreLeaf", icon = icon("binoculars"))
                )
            })
            output$colorImage <- renderUI({
                tags$img(src="images/leaf.JPG",width=150,style="display: block; margin-left: auto; margin-right: auto;")
            })
        }
        
    }) # END Observe Event - Study Selection
    
    # Listen to any changed selections of pumpkin color to display correct image
    # NOTE: shinyapps.io requires file paths that are case sensitive (.jpg vs .JPG matters)
    observeEvent(input$colorSelection,{
        if(input$colorSelection == "All"){
            output$colorImage <- renderUI({
                tags$img(src="images/allPumpkins.JPG",width=150,style="display: block; margin-left: auto; margin-right: auto;")
            })
        }
        if(input$colorSelection == "Orange"){
            output$colorImage <- renderUI({
                tags$img(src="images/orangePumpkins.JPG",width=150,style="display: block; margin-left: auto; margin-right: auto;")
            })
        }
        if(input$colorSelection == "Green"){
            output$colorImage <- renderUI({
                tags$img(src="images/greenPumpkins.JPG",width=150,style="display: block; margin-left: auto; margin-right: auto;")
            })
        }
    })
    
    ###################### REACTIVE ELEMENTS FOR MAIN DATA SET #####################################
    
    #####
    ##### STUDY SELECTION
    #####
    
    studyFinder <- reactive({
        # Read user selected study and assign corresponding data set
        if (input$studySelection == "Plant Spacing"){
            pumpkins <- spacingData
        } else {
            if (input$studySelection == "Nitrogen Level"){
                pumpkins <- nitrogenData
            } else {
                if (input$studySelection == "Leaf Analysis"){
                    pumpkins <- leafData
                } else {
                    pumpkins <- tibble()
                  }
              }
          }
    })
    
    #####
    ##### YEAR
    #####
    
    yearFinder <- reactive({
        # Filter on user selected year
        if (input$yearSelection == "All"){
            pumpkins <- studyFinder()
        } else {
            pumpkins <- studyFinder() %>% filter(year == input$yearSelection)
        }
    })
    
    #####
    ##### PUMPKIN COLOR
    #####
    
    colorFinder <- reactive({
        # Filter on user selected color
        if (input$colorSelection == "All"){
            pumpkins <- yearFinder()
        } else {
            pumpkins <- yearFinder() %>% filter(color == input$colorSelection)
        }
    })
    
    #####
    ##### PUMPKINS
    #####
    
    pumpkins <- reactive({
        colorFinder()
    })
    
    ###################### RENDER SPACING OUTPUT #############################################################################
    
    #####
    ##### INFORMATION - SPACING
    #####
    
    output$infoSpaceStudyIntro <- renderUI({
        tags$div(tags$br(),
                 tags$blockquote(
                     tags$b("Hypothesis"),
                     tags$br(),
                     tags$i("Pumpkins grown closer together will have a higher overall yield.")
                 ),
                 tags$h4(tags$b("Background")),
                 HTML(paste("Pumpkins are an emerging specialty crop in North Carolina. In 2019 the state produced 713,000 cwt of pumpkins on 3,100 acres, ranking 6th in the United States where overall national production is steadily increasing ")),
                 tags$a(href="https://usda.library.cornell.edu/concern/publications/02870v86p?locale=en","(USDA Feb 16, 2022)",target="_blank"),
                 HTML(paste(". With the rise in production there is an increasing need for providing clear guidelines to North Carolina farmers on how plant density impacts overall yield.")),
                 tags$br(),
                 tags$h4(tags$b("Why is plant density important?")),
                 tags$ul(
                          tags$li("As plant density increases, yields will generally increase"), 
                          tags$ul(tags$li("Decrease vegetative growth, increase reproductive growth")), 
                          tags$li("Increased plant density aids in weed maintenance"),
                          tags$li("Fuller canopies protect fruit from sun damage")
                 ),
                 tags$h4(tags$b("Objectives of Study")),
                 tags$ul(
                     tags$li("Understand the impact of spacing on pumpkin production and fruit size"), 
                     tags$li("Find the pumpkin spacing which produces the highest yield"),
                     tags$li("Compare 10 ft and 5 ft between-row spacing")
                 ),
                 tags$h4(tags$b("Methods")),
                 HTML(paste("The Spacing Study was planted for two seasons in 2020 and 2021 at the NC State Extension Upper Mountain Research Station under the care of NC Extension Agents. Crops were planted in June and harvested in September during both years after a 12 week growing period.")),
                 tags$ul(
                     tags$li("Four different plant areas were studied, each with a corresponding 10 ft and 5 ft between-row spacing distance"), 
                     tags$ul(tags$li(HTML(paste("10 ft", tags$sup(2),", 20 ft", tags$sup(2),", 30 ft", tags$sup(2),", and 40 ft", tags$sup(2), sep = "")))),
                     tags$li("Variety: Kratos pumpkins (20-30 lbs average weight)"),
                     tags$li("Fruit was evaluated for size, weight, and maturity (orange or green in color)")
                 ),
        )
    })
    
    output$infoSpaceStudyGloss <- renderUI({
        tags$div(tags$br(),
                 tags$blockquote(
                     tags$b("Glossary of Terms"),
                     tags$br(),
                     tags$i("Defining variables in the plant spacing study data set...")
                 ),
                 HTML(paste("There are a total of ",ncol(spacingData)," variables and ",format(round(as.numeric(nrow(spacingData)),1),big.mark=",")," observations from the plant spacing study feeding this application. All variables, their descriptions, and unit of measurement are listed below!")),
                 tags$br(),
                 tags$h4(tags$b("Response Variables")),
                 tags$ul(
                     tags$li(tags$code("color"),tags$b(" - Pumpkin Color: "),"Listed as being either orange or green, as noted at time of harvest."),
                     tags$li(tags$code("diameter"),tags$b(" - Pumpkin Diameter ",tags$i("(inches)"),": "),"The linear distance of the width of a pumpkin as measured at time of harvest."),
                     tags$li(tags$code("diameterRoundDown"),tags$b(" - Pumpkin Diameter Rounded Down ",tags$i("(inches)"),": "),"The pumpkin diameter rounded down to the nearest whole number. ",tags$i("This pumpkin is at least x inches in diameter.")),
                     tags$li(tags$code("diameterRoundUp"),tags$b(" - Pumpkin Diameter Rounded Down ",tags$i("(inches)"),": "),"The pumpkin diameter rounded up to the nearest whole number. ",tags$i("This pumpkin is at most x inches in diameter.")),
                     tags$li(tags$code("length"),tags$b(" - Pumpkin Length ",tags$i("(inches)"),": "),"The linear distance of the height of a pumpkin as measured at time of harvest, from the bottom of the fruit to the base of the stem."),
                     tags$li(tags$code("volumeEllipsoid"),tags$b(" - Pumpkin Volume ",tags$i("(cubic inches)"),": "),"The calculated volume using the formula for volume of an ellipsoid."),
                     tags$li(tags$code("weight"),tags$b(" - Pumpkin Weight ",tags$i("(lbs)"),": "),"The weight as measured at time of harvest.")
                 ),
                 tags$h4(tags$b("Predictor Variables")),
                 tags$ul(
                     tags$li(tags$code("betweenRow"),tags$b(" - Between Row Distance ",tags$i("(feet)"),": "),"The distance between rows."),
                     tags$li(tags$code("harvestArea"),tags$b(" - Harvest Area ",tags$i("(square feet)"),": "),"The inner area of each plot, marked by ribbon, where all pumpkins within the marked off area are sampled."),
                     tags$ul(tags$li("Between Row Distance of 10 ft = 20 ft across middle two rows x inner 20 ft of row length = Harvest Area of 400 ft",tags$sup(2)),
                             tags$li("Between Row Distance of 5 ft = 10 ft across middle two rows x inner 20 ft of row length = Harvest Area of 200 ft",tags$sup(2))
                             ),
                     tags$li(tags$code("inRow"),tags$b(" - In Row Distance ",tags$i("(feet)"),": "),"The distance between plants on the same row."),
                     tags$li(tags$code("plantArea"),tags$b(" - Plant Spacing Area ",tags$i("(square feet)"),": "),"The amount of allocated space per plant, either 10 ft", tags$sup(2),", 20 ft", tags$sup(2),", 30 ft", tags$sup(2),", or 40 ft", tags$sup(2)," in size."),
                     tags$li(tags$code("plot"),tags$b(" - Plot: "),"An area containing four rows of plants, where each row is 40 ft long. Only one spacing dimension treatment is applied per plot."),
                     tags$li(tags$code("plotArea"),tags$b(" - Plot Area ",tags$i("(square feet)"),": "),"The total area of each plot."),
                     tags$ul(tags$li("Between Row Distance of 10 ft = 40 ft across four rows x 40 ft row length = Plot Area of 1,600 ft",tags$sup(2)),
                             tags$li("Between Row Distance of 5 ft = 20 ft across four rows x 40 ft row length = Plot Area of 800 ft",tags$sup(2))
                             ),
                     tags$li(tags$code("pumpkinID"),tags$b(" - Pumpkin Identifier: "),"A unique identifier for each harvested pumpkin comprised of the Study Abbreviation (*S*pacing) - Year - Plot - Color - Pumpkin Number"),
                     tags$li(tags$code("pumpkinNum"),tags$b(" - Pumpkin Number: "),"The assigned number each harvested pumpkin was labeled with in each plot. Non-unique across plots."),
                     tags$li(tags$code("rep"),tags$b(" - Repetition: "),"The repetition number of each plant spacing dimension treatment in each year."),
                     tags$li(tags$code("spacingDim"),tags$b(" - Plant Spacing Dimension ",tags$i("(feet)"),": "),"A plant spacing treatment representing the between row by in row spacing dimension of each plant, used to calculate the plant area."),
                     tags$li(tags$code("standCount"),tags$b(" - Stand Count: "),"The total number of surviving stands (plants) on the inner two rows of each plot."),
                     tags$li(tags$code("standCountIdeal"),tags$b(" - Ideal Stand Count: "),"The ideal, or expected total number of stands (plants) on the inner two rows of each plot based on the plant spacing dimension."),
                     tags$li(tags$code("standCountIdealPct"),tags$b(" - Percent of Ideal Stand Count: "),"Stand Count / Ideal Stand Count = Percent of the ideal total number of stands (plants) on the inner two rows of each plot based on the plant spacing dimension."),
                     tags$li(tags$code("treatment"),tags$b(" - Treatment: "),"A numerical value of 1, 2, 3, 4, 5, 6, 7, or 8 assigned to each of the eight different spacing dimension treatments."),
                     tags$li(tags$code("year"),tags$b(" - Year: "),"The year of the growing season the study was conducted. 12-week period from June to September.")
                 ),
        )
    })
    
    output$infoSpaceStudyContact <- renderUI({
        tags$div(tags$h3(tags$b("Researcher: "),"Kim Heagy"),
                 tags$h4("Department of Horticultural Science"),
                 tags$h5("North Carolina State University"),
                 tags$br(),
                 tags$h3(tags$b("Analysts: "),"Brian Sugg and Chengxi Zou"),
                 tags$h4("Department of Statistics"),
                 tags$h5("North Carolina State University"),
        )
    })
    
    output$infoSpaceStudyDown <- renderUI({
        tags$div(tags$br(),
                 HTML(paste("All of the prepared data from the plant spacing study can be downloaded in the preferred 
                            file format using the below links:")),
                 tags$br(),
                 tags$br(),
                 a(href="files/R/pumpkinData.RData", "Download R", download=NA, target="_blank"),
                 tags$i("(Object Name: spacingData)"),
                 tags$br(),
                 a(href="files/SAS/spacingdata.sas7bdat", "Download SAS", download=NA, target="_blank"),
                 tags$br(),
                 a(href="files/Excel/spacingData.xlsx", "Download Excel", download=NA, target="_blank")
        )
    })
    
    output$infoSpaceAreaTable <- function() {
        req(input$infoSpaceSelectArea)
        pumpkinsAreaTable <- pumpkins() %>% filter(plantArea == input$infoSpaceSelectArea)
        pumpkinsAreaTable <- pumpkinsAreaTable %>% select(spacingDim,plot,plotArea,harvestArea,pumpkinID,diameter,volumeEllipsoid,weight,standCount,standCountIdeal,standCountIdealPct) %>%
            group_by(spacingDim,plot) %>% summarise("Plot Area (sq. ft.)"=mean(plotArea),
                                                    "Harvest Area (sq. ft.)"=mean(harvestArea),
                                                    "Harvest Pumpkin Count"=n(),
                                                    "Avg Diameter (Inches)"=round(mean(diameter),1),
                                                    "Avg Volume (cu. in.)"=round(mean(volumeEllipsoid),1),
                                                    "Avg Weight (lbs)"=round(mean(weight),1),
                                                    "Stand Count"=mean(standCount),
                                                    "Ideal Stand Count"=mean(standCountIdeal),
                                                    "% of Ideal Stand Count"=percent(mean(standCountIdealPct),accuracy=0.1),
                                                    ) %>%
            knitr::kable("html") %>%
            kable_styling("striped",full_width=TRUE,position="left",fixed_thead = TRUE)
        }
    
    # Two RenderUI() for two dynamic images in information tab for treatments
    output$infoSpaceAreaImage <- renderUI({
        if(input$infoSpaceSelectArea=="10"){
            tags$img(src="images/spacing/drone/plantSpace10.JPG",
                     width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                     tags$caption("Plot Area of Spacing Dimensions: 10 x 1 (Left), 5 x 2 (Right)")
            )
        } else{
            if(input$infoSpaceSelectArea=="20"){
                tags$img(src="images/spacing/drone/plantSpace20.JPG",
                         width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("Plot Area of Spacing Dimensions: 10 x 2 (Left), 5 x 4 (Right)")
                )
            } else{
                if(input$infoSpaceSelectArea=="30"){
                    tags$img(src="images/spacing/drone/plantSpace30.JPG",
                             width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                             tags$caption("Plot Area of Spacing Dimensions: 10 x 3 (Left), 5 x 6 (Right)")
                    )
                } else{
                    if(input$infoSpaceSelectArea=="40"){
                        tags$img(src="images/spacing/drone/plantSpace40.JPG",
                                 width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                                 tags$caption("Plot Area of Spacing Dimensions: 10 x 4 (Left), 5 x 8 (Right)")
                        )
                    } else{}}}}
    })
    output$infoSpaceHarvestImage <- renderUI({
        if(input$infoSpaceSelectArea=="10"){
            tags$img(src="images/spacing/drone/plantSpaceHarvest10.JPG",
                     width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                     tags$caption("Harvest Area of Spacing Dimensions: 10 x 1 (Left), 5 x 2 (Right)")
            )
        } else{
            if(input$infoSpaceSelectArea=="20"){
                tags$img(src="images/spacing/drone/plantSpaceHarvest20.JPG",
                         width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("Harvest Area of Spacing Dimensions: 10 x 2 (Left), 5 x 4 (Right)")
                )
            } else{
                if(input$infoSpaceSelectArea=="30"){
                    tags$img(src="images/spacing/drone/plantSpaceHarvest30.JPG",
                             width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                             tags$caption("Harvest Area of Spacing Dimensions: 10 x 3 (Left), 5 x 6 (Right)")
                    )
                } else{
                    if(input$infoSpaceSelectArea=="40"){
                        tags$img(src="images/spacing/drone/plantSpaceHarvest40.JPG",
                                 width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                                 tags$caption("Harvest Area of Spacing Dimensions: 10 x 4 (Left), 5 x 8 (Right)")
                        )
                    } else{}}}}
    })
    
    #####
    ##### DATA EXPLORATION - SPACING
    #####
    
    ###
    ### PUMPKIN SUMMARY
    ###
    
    # TEXT
    
    # GRAPHS
    
    ############# SPACING CHARTS FOR QUANTITY ###############
    
    # 100pct stack color finder
    colorFinderStack <- reactive({
        # Filter on user selected color
        if (input$colorSelection == "Orange"){
            stackColor <- as.vector(c("dark orange"))
        } else{
            if (input$colorSelection == "Green"){
                stackColor <- as.vector(c("dark green"))
            } else {
                stackColor <- as.vector(c("dark green", "dark orange"))
            }}
    })
    
    # Custom formula for 100pct stack color
    plot_color_100 <- function(df, feature, label_column) {
        plt <- ggplot(df, aes(x=eval(parse(text=label_column)),fill=eval(parse(text=feature)))) +
            geom_bar(stat = "count",
                     position = "fill") +
            scale_fill_manual(values = feature) +
            scale_y_continuous(labels = scales::percent_format(scale = 100)) +
            theme(legend.position="none") +
            labs(x==eval(parse(text=label_column)),
                 y="Pumpkin Count %",
                 fill="Pumpkin Color")
    }
    
    # REACTIVE chain for drop down selection values of spacing dimension
    dimChoicesQty <- reactive({
        if(input$exSpaceQtyArea=="All"){
            dimChoice <- c("All","10 x 1","10 x 2","10 x 3","10 x 4","5 x 2","5 x 4","5 x 6","5 x 8")}
        if(input$exSpaceQtyArea=="10"){
            dimChoice <- c("All","10 x 1","5 x 2")}
        if(input$exSpaceQtyArea=="20"){
            dimChoice <- c("All","10 x 2","5 x 4")}
        if(input$exSpaceQtyArea=="30"){
            dimChoice <- c("All","10 x 3","5 x 6")}
        if(input$exSpaceQtyArea=="40"){
            dimChoice <- c("All","10 x 4","5 x 8")}
        return(dimChoice)
    })
    output$exSpaceQtyDim = renderUI({
        selectInput(inputId = "exSpaceQtyDimReact", label = "Spacing Dimension", dimChoicesQty())
    })
    
    # REACTIVE variables that react to user input
    # Solo reaction for group by
    qtyGroup <- reactive({
        # Apply user selected grouping
        quantityGroupSelect <- input$exSpaceQtyGroup
    })
    # REACTIVE chain for plant area and spacing dimension
    pumpkinsQtyArea <- reactive({
        # Filter on user selected plant area
        if (input$exSpaceQtyArea=="All") {
            pumpkinsQty <- pumpkins()
        } else{pumpkinsQty <- pumpkins() %>% filter(plantArea == input$exSpaceQtyArea)}
    })
    pumpkinsQtyDim <- reactive({
        # Filter on user selected spacing dimension
        if (input$exSpaceQtyDimReact=="All") {
            pumpkinsQty <- pumpkinsQtyArea()
        } else{pumpkinsQty <- pumpkinsQtyArea() %>% filter(spacingDim == input$exSpaceQtyDimReact)}
    })
    
    # Data table to feed the quantity bar graphs
    pumpkinsQtyBarTab <- reactive({
        # Generate summary table from previous user selections for graphing
        pumpkinsQtyDim() %>%
            select(plantArea,spacingDim,betweenRow,inRow,year,plot,pumpkinID) %>%
            group_by(plantArea,spacingDim,betweenRow,inRow,year,plot) %>%
            summarise("pumpkinCount"=n())
    })
    
    # Data table to feed the diameter quantity line graphs
    pumpkinsQtyDiaTab <- reactive({
        # Generate summary table from previous user selections for graphing
        pumpkinsQtyDim() %>%
            select(plantArea,spacingDim,betweenRow,inRow,year,plot,diameterRoundUp,pumpkinID,diameter,volumeEllipsoid,weight) %>%
            group_by(plantArea,spacingDim,betweenRow,inRow,year,plot,diameterRoundUp) %>% 
            summarise("pumpkinCount"=n(),
                      "Avg Diameter (Inches)"=round(mean(diameter),1),
                      "Avg Volume (cu. in.)"=round(mean(volumeEllipsoid),1),
                      "Avg Weight (lbs)"=round(mean(weight),1))
    })
    
    # Data table to feed the diameter quantity line graphs
    pumpkinsQtyDiaColor <- reactive({
        # Generate summary table from previous user selections for graphing
        pumpkinsQtyDim() %>%
            select(plantArea,spacingDim,betweenRow,inRow,year,plot,diameterRoundUp,color,pumpkinID) %>%
            group_by(plantArea,spacingDim,betweenRow,inRow,year,plot,diameterRoundUp,color) %>% 
            summarise("pumpkinCount"=n())
    })
    
    ###
    ### Diameter DISTR Charts
    ###

    output$exSpaceQtyDia <- renderPlotly({
        withProgress(message = 'Generating Charts...', value = 0,{
            incProgress(0, detail = "for Diameter")
        # Return preferred plot
        if(input$exSpaceQtyPlot=="averagePlot"){
            getPumpkins <- pumpkinsQtyDiaTab() %>%
                select(qtyGroup(),diameterRoundUp,pumpkinCount) %>%
                group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
                summarise("pumpkinAvgCount"=mean(pumpkinCount))
            names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
            diaCount <- ggplot(getPumpkins,aes(x=diameterRoundUp,y=plotAverageCount,group=qtyGroup(),color=eval(parse(text=qtyGroup())))) +
                geom_line(size=1) +
                geom_point() +
                labs(title="Average Pumpkin Count per RoundUp Diameter",
                     x="Pumpkin Diameter Rounded Up (inches)",
                     y="Average Pumpkin Count - Harvest Area")
            #diaCount + guides(fill=guide_legend(title=qtyGroup())) # This is not working as expected to change legend title...???
            y <- ggplotly(diaCount)
            incProgress(1/4, detail = "for Diameter")
            y
        } else{
            if(input$exSpaceQtyPlot=="scaledAveragePlot"){
                scaledPumpkins <- pumpkinsQtyDiaTab()
                # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
                getPumpkins <- scaledPumpkins %>%
                    select(qtyGroup(),diameterRoundUp,scaledPumpkinCount) %>%
                    group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
                    summarise("pumpkinAvgCount"=mean(scaledPumpkinCount))
                names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                
                diaCount <- ggplot(getPumpkins,aes(x=diameterRoundUp,y=plotScaledAverageCount,group=qtyGroup(),color=eval(parse(text=qtyGroup())))) +
                    geom_line(size=1) +
                    geom_point() +
                    labs(title="Scaled Average Pumpkin Count per Plot per RoundUp Diameter",
                         x="Pumpkin Diameter Rounded Up (inches)",
                         y="Average Pumpkin Count - Scaled Harvest Area")
                #diaCount + guides(fill=guide_legend(title=qtyGroup())) # This is not working as expected to change legend title...???
                y <- ggplotly(diaCount)
                incProgress(1/4, detail = "for Diameter")
                y
            } else{
                if(input$exSpaceQtyPlot=="scaledAverageAcre"){
                    scaledPumpkins <- pumpkinsQtyDiaTab()
                    # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                    # Scale both to an acre with 43,560 sq ft in an acre (43560/400=108.9)
                    scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2*108.9,pumpkinCount*108.9))
                    getPumpkins <- scaledPumpkins %>%
                        select(qtyGroup(),diameterRoundUp,scaledPumpkinCount) %>%
                        group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
                        summarise("pumpkinAvgCount"=mean(scaledPumpkinCount))
                    names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","acreScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                    
                    diaCount <- ggplot(getPumpkins,aes(x=diameterRoundUp,y=acreScaledAverageCount,group=qtyGroup(),color=eval(parse(text=qtyGroup())))) +
                        geom_line(size=1) +
                        geom_point() +
                        labs(title="Scaled Average Pumpkin Count per Acre per RoundUp Diameter",
                             x="Pumpkin Diameter Rounded Up (inches)",
                             y="Average Pumpkin Count - Scaled Area")
                    #diaCount + guides(fill=guide_legend(title=qtyGroup())) # This is not working as expected to change legend title...???
                    y <- ggplotly(diaCount)
                    incProgress(1/4, detail = "for Diameter")
                    y
                    } else{
                        if(input$exSpaceQtyPlot=="totalPlot"){
                            getPumpkins <- pumpkinsQtyDiaTab() %>%
                                select(qtyGroup(),diameterRoundUp,pumpkinCount) %>%
                                group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
                                summarise("pumpkinAvgCount"=sum(pumpkinCount))
                            names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                            diaCount <- ggplot(getPumpkins,aes(x=diameterRoundUp,y=plotSumCount,group=qtyGroup(),color=eval(parse(text=qtyGroup())))) +
                                geom_line(size=1) +
                                geom_point() +
                                labs(title="Total Pumpkin Count per RoundUp Diameter",
                                     x="Pumpkin Diameter Rounded Up (inches)",
                                     y="Total Pumpkin Count - Harvest Area")
                            #diaCount + guides(fill=guide_legend(title=qtyGroup())) # This is not working as expected to change legend title...???
                            y <- ggplotly(diaCount)
                            incProgress(1/4, detail = "for Diameter")
                            y
                        } else{
                            if(input$exSpaceQtyPlot=="scaledTotalPlot"){
                                scaledPumpkins <- pumpkinsQtyDiaTab()
                                # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                                scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
                                getPumpkins <- scaledPumpkins %>%
                                    select(qtyGroup(),diameterRoundUp,scaledPumpkinCount) %>%
                                    group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
                                    summarise("pumpkinAvgCount"=sum(scaledPumpkinCount))
                                names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotScaledSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                                
                                diaCount <- ggplot(getPumpkins,aes(x=diameterRoundUp,y=plotScaledSumCount,group=qtyGroup(),color=eval(parse(text=qtyGroup())))) +
                                    geom_line(size=1) +
                                    geom_point() +
                                    labs(title="Scaled Total Pumpkin Count per Plot per RoundUp Diameter",
                                         x="Pumpkin Diameter Rounded Up (inches)",
                                         y="Total Pumpkin Count - Scaled Harvest Area")
                                #diaCount + guides(fill=guide_legend(title=qtyGroup())) # This is not working as expected to change legend title...???
                                y <- ggplotly(diaCount)
                                incProgress(1/4, detail = "for Diameter")
                                y
                            } else{}}}}}
        }) # END Progress Bar
    })
    
    ###
    ### BAR Charts
    ###
    output$exSpaceQtyBar <- renderPlotly({
        withProgress(message = 'Generating Charts...', value = 0,{
            incProgress(1/4, detail = "for Count")
        # Return preferred plot
        if(input$exSpaceQtyPlot=="averagePlot"){
            getPumpkins <- pumpkinsQtyBarTab() %>%
                select(qtyGroup(),pumpkinCount) %>%
                group_by(eval(parse(text=qtyGroup()))) %>% # Use the parse to create row for every value of group by
                summarise("pumpkinCountAvg"=mean(pumpkinCount))
            names(getPumpkins) <- c(qtyGroup(),"plotAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
            barCount <- ggplot(getPumpkins, aes(x=eval(parse(text=qtyGroup())),y=plotAverageCount,fill=eval(parse(text=qtyGroup())))) +
                geom_col() +
                theme(legend.position="none") +
                labs(title="Average Pumpkin Count per Plot",
                     x=qtyGroup(),
                     y="Average Pumpkin Count - Harvest Area")
            y <- ggplotly(barCount)
            incProgress(2/4, detail = "for Count")
            y
        } else{
            if(input$exSpaceQtyPlot=="scaledAveragePlot"){
                scaledPumpkins <- pumpkinsQtyBarTab()
                # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
                getPumpkins <- scaledPumpkins %>%
                    select(qtyGroup(),scaledPumpkinCount) %>%
                    group_by(eval(parse(text=qtyGroup()))) %>% # Use the parse to create row for every value of group by
                    summarise("pumpkinCountAvg"=mean(scaledPumpkinCount))
                names(getPumpkins) <- c(qtyGroup(),"plotScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                barCount <- ggplot(getPumpkins, aes(x=eval(parse(text=qtyGroup())),y=plotScaledAverageCount,fill=eval(parse(text=qtyGroup())))) +
                    geom_col() +
                    theme(legend.position="none") +
                    labs(title="Scaled Average Pumpkin Count per Plot",
                         x=qtyGroup(),
                         y="Average Pumpkin Count - Scaled Harvest Area")
                y <- ggplotly(barCount)
                incProgress(2/4, detail = "for Count")
                y
            } else{
                if(input$exSpaceQtyPlot=="scaledAverageAcre"){
                    scaledPumpkins <- pumpkinsQtyBarTab()
                    # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                    # Scale both to an acre with 43,560 sq ft in an acre (43560/400=108.9)
                    scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2*108.9,pumpkinCount*108.9))
                    getPumpkins <- scaledPumpkins %>%
                        select(qtyGroup(),scaledPumpkinCount) %>%
                        group_by(eval(parse(text=qtyGroup()))) %>% # Use the parse to create row for every value of group by
                        summarise("pumpkinCountAvg"=mean(scaledPumpkinCount))
                    names(getPumpkins) <- c(qtyGroup(),"acreScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                    barCount <- ggplot(getPumpkins, aes(x=eval(parse(text=qtyGroup())),y=acreScaledAverageCount,fill=eval(parse(text=qtyGroup())))) +
                        geom_col() +
                        theme(legend.position="none") +
                        labs(title="Scaled Average Pumpkin Count per Acre",
                             x=qtyGroup(),
                             y="Average Pumpkin Count - Scaled Acre")
                    y <- ggplotly(barCount)
                    incProgress(2/4, detail = "for Count")
                    y
                } else{
                    if(input$exSpaceQtyPlot=="totalPlot"){
                        getPumpkins <- pumpkinsQtyBarTab()
                        getPumpkins <- getPumpkins %>%
                            select(qtyGroup(),pumpkinCount) %>%
                            group_by(eval(parse(text=qtyGroup()))) %>% # Use the parse to create row for every value of group by
                            summarise("pumpkinCountSum"=sum(pumpkinCount))
                        names(getPumpkins) <- c(qtyGroup(),"plotSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                        barCount <- ggplot(getPumpkins, aes(x=eval(parse(text=qtyGroup())),y=plotSumCount,fill=eval(parse(text=qtyGroup())))) +
                            geom_col() +
                            theme(legend.position="none") +
                            labs(title="Total Pumpkin Count",
                                 x=qtyGroup(),
                                 y="Total Pumpkin Count - Harvest Area")
                        y <- ggplotly(barCount)
                        incProgress(2/4, detail = "for Count")
                        y
                    } else{
                        if(input$exSpaceQtyPlot=="scaledTotalPlot"){
                            scaledPumpkins <- pumpkinsQtyBarTab()
                            # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                            scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
                            getPumpkins <- scaledPumpkins %>%
                                select(qtyGroup(),scaledPumpkinCount) %>%
                                group_by(eval(parse(text=qtyGroup()))) %>% # Use the parse to create row for every value of group by
                                summarise("pumpkinCountSum"=sum(scaledPumpkinCount))
                            names(getPumpkins) <- c(qtyGroup(),"plotScaledSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                            barCount <- ggplot(getPumpkins, aes(x=eval(parse(text=qtyGroup())),y=plotScaledSumCount,fill=eval(parse(text=qtyGroup())))) +
                                geom_col() +
                                theme(legend.position="none") +
                                labs(title="Scaled Total Pumpkin Count",
                                     x=qtyGroup(),
                                     y="Total Pumpkin Count - Scaled Harvest Area")
                            y <- ggplotly(barCount)
                            incProgress(2/4, detail = "for Count")
                            y
                        } else{}}}}}
    }) # END Progress Bar
    })
    
    # # Data table matching diameter quantity line graphs that feeds the associated data table
    # pumpkinsQtyDisplayTab <- reactive({
    #         # Return preferred plot
    #         if(input$exSpaceQtyPlot=="averagePlot"){
    #             getPumpkins <- pumpkinsQtyDiaTab() %>%
    #                 select(qtyGroup(),diameterRoundUp,pumpkinCount) %>%
    #                 group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
    #                 summarise("pumpkinAvgCount"=mean(pumpkinCount))
    #             names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
    #             getPumpkins[3] <- round(getPumpkins[3],1)
    #             getPumpkins
    #         } else{
    #             if(input$exSpaceQtyPlot=="scaledAveragePlot"){
    #                 scaledPumpkins <- pumpkinsQtyDiaTab()
    #                 # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
    #                 scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
    #                 getPumpkins <- scaledPumpkins %>%
    #                     select(qtyGroup(),diameterRoundUp,scaledPumpkinCount) %>%
    #                     group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
    #                     summarise("pumpkinAvgCount"=mean(scaledPumpkinCount))
    #                 names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
    #                 getPumpkins[3] <- round(getPumpkins[3],1)
    #                 getPumpkins
    #                 } else{
    #                 if(input$exSpaceQtyPlot=="scaledAverageAcre"){
    #                     scaledPumpkins <- pumpkinsQtyDiaTab()
    #                     # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
    #                     # Scale both to an acre with 43,560 sq ft in an acre (43560/400=108.9)
    #                     scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2*108.9,pumpkinCount*108.9))
    #                     getPumpkins <- scaledPumpkins %>%
    #                         select(qtyGroup(),diameterRoundUp,scaledPumpkinCount) %>%
    #                         group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
    #                         summarise("pumpkinAvgCount"=mean(scaledPumpkinCount))
    #                     names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","acreScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
    #                     getPumpkins[3] <- round(getPumpkins[3],1)
    #                     getPumpkins
    #                 } else{
    #                     if(input$exSpaceQtyPlot=="totalPlot"){
    #                         getPumpkins <- pumpkinsQtyDiaTab() %>%
    #                             select(qtyGroup(),diameterRoundUp,pumpkinCount) %>%
    #                             group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
    #                             summarise("pumpkinAvgCount"=sum(pumpkinCount))
    #                         names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
    #                         getPumpkins
    #                     } else{
    #                         if(input$exSpaceQtyPlot=="scaledTotalPlot"){
    #                             scaledPumpkins <- pumpkinsQtyDiaTab()
    #                             # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
    #                             scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
    #                             getPumpkins <- scaledPumpkins %>%
    #                                 select(qtyGroup(),diameterRoundUp,scaledPumpkinCount) %>%
    #                                 group_by(eval(parse(text=qtyGroup())),diameterRoundUp) %>% # Use the parse to create row for every value of group by
    #                                 summarise("pumpkinAvgCount"=sum(scaledPumpkinCount))
    #                             names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","plotScaledSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
    #                             getPumpkins
    #                         } else{}}}}}
    # })
    
    # # Reactive table of values by user group selection
    # output$exSpaceQtyTable <- function() {
    #         # Return preferred selection
    #         getPumpkins <- pumpkinsQtyDisplayTab()
    #         names(getPumpkins)[1:3] <- c(qtyGroup(),"Diameter Round Up (inches)","Pumpkin Count")
    #         getPumpkins %>%
    #         knitr::kable("html") %>%
    #         kable_styling("striped",full_width=TRUE,position="left",fixed_thead = TRUE)
    # }
    
    # Data table matching diameter quantity line graphs that feeds the associated data table
    pumpkinsQtyDisplayTab <- reactive({
        # Return preferred plot
        if(input$exSpaceQtyPlot=="averagePlot"){
            getPumpkins <- pumpkinsQtyDiaColor() %>%
                select(qtyGroup(),diameterRoundUp,color,pumpkinCount) %>%
                group_by(eval(parse(text=qtyGroup())),diameterRoundUp,color) %>% # Use the parse to create row for every value of group by
                summarise("pumpkinAvgCount"=mean(pumpkinCount))
            names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","color","plotAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
            getPumpkins[4] <- round(getPumpkins[4],1)
            getPumpkins
        } else{
            if(input$exSpaceQtyPlot=="scaledAveragePlot"){
                scaledPumpkins <- pumpkinsQtyDiaColor()
                # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
                getPumpkins <- scaledPumpkins %>%
                    select(qtyGroup(),diameterRoundUp,color,scaledPumpkinCount) %>%
                    group_by(eval(parse(text=qtyGroup())),diameterRoundUp,color) %>% # Use the parse to create row for every value of group by
                    summarise("pumpkinAvgCount"=mean(scaledPumpkinCount))
                names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","color","plotScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                getPumpkins[4] <- round(getPumpkins[4],1)
                getPumpkins
            } else{
                if(input$exSpaceQtyPlot=="scaledAverageAcre"){
                    scaledPumpkins <- pumpkinsQtyDiaColor()
                    # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                    # Scale both to an acre with 43,560 sq ft in an acre (43560/400=108.9)
                    scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2*108.9,pumpkinCount*108.9))
                    getPumpkins <- scaledPumpkins %>%
                        select(qtyGroup(),diameterRoundUp,color,scaledPumpkinCount) %>%
                        group_by(eval(parse(text=qtyGroup())),diameterRoundUp,color) %>% # Use the parse to create row for every value of group by
                        summarise("pumpkinAvgCount"=mean(scaledPumpkinCount))
                    names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","color","acreScaledAverageCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                    getPumpkins[4] <- round(getPumpkins[4],1)
                    getPumpkins
                } else{
                    if(input$exSpaceQtyPlot=="totalPlot"){
                        getPumpkins <- pumpkinsQtyDiaColor() %>%
                            select(qtyGroup(),diameterRoundUp,color,pumpkinCount) %>%
                            group_by(eval(parse(text=qtyGroup())),diameterRoundUp,color) %>% # Use the parse to create row for every value of group by
                            summarise("pumpkinAvgCount"=sum(pumpkinCount))
                        names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","color","plotSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                        getPumpkins
                    } else{
                        if(input$exSpaceQtyPlot=="scaledTotalPlot"){
                            scaledPumpkins <- pumpkinsQtyDiaColor()
                            # Scale 5' betweenRow plots to equal harvest area of 400 sq ft to match that of 10' betweenRow plots
                            scaledPumpkins <- scaledPumpkins %>% mutate(scaledPumpkinCount=ifelse(betweenRow=="5",pumpkinCount*2,pumpkinCount))
                            getPumpkins <- scaledPumpkins %>%
                                select(qtyGroup(),diameterRoundUp,color,scaledPumpkinCount) %>%
                                group_by(eval(parse(text=qtyGroup())),diameterRoundUp,color) %>% # Use the parse to create row for every value of group by
                                summarise("pumpkinAvgCount"=sum(scaledPumpkinCount))
                            names(getPumpkins) <- c(qtyGroup(),"diameterRoundUp","color","plotScaledSumCount") # Have to change column names else it will show eval(parse(text=qtyGroup())) as col name
                            getPumpkins
                        } else{}}}}}
    })
    
    # Stacked bar chart of pumpkin count by color by diameter round up
    output$exSpaceQtyDiaColor <- renderPlotly({
        getPumpkins <- pumpkinsQtyDisplayTab()
        if(input$exSpaceQtyPlot=="averagePlot"){
            stackColor <- ggplot(getPumpkins, aes(x=diameterRoundUp,y=plotAverageCount,fill=color)) +
                geom_bar(position="stack", stat="identity") +
                scale_fill_manual(values = colorFinderStack()) +
                labs(title="Pumpkin Color Distribution",
                     fill="Color",
                     x="Pumpkin Diameter Rounded Up (inches)",
                     y="Average Pumpkin Count - Harvest Area")
            y <- ggplotly(stackColor)
            y
        } else{
            if(input$exSpaceQtyPlot=="scaledAveragePlot"){
                stackColor <- ggplot(getPumpkins, aes(x=diameterRoundUp,y=plotScaledAverageCount,fill=color)) +
                    geom_bar(position="stack", stat="identity") +
                    scale_fill_manual(values = colorFinderStack()) +
                    labs(title="Pumpkin Color Distribution",
                         fill="Color",
                         x="Pumpkin Diameter Rounded Up (inches)",
                         y="Average Pumpkin Count - Scaled Harvest Area")
                y <- ggplotly(stackColor)
                y
            } else{
                if(input$exSpaceQtyPlot=="scaledAverageAcre"){
                    stackColor <- ggplot(getPumpkins, aes(x=diameterRoundUp,y=acreScaledAverageCount,fill=color)) +
                        geom_bar(position="stack", stat="identity") +
                        scale_fill_manual(values = colorFinderStack()) +
                        labs(title="Pumpkin Color Distribution",
                             fill="Color",
                             x="Pumpkin Diameter Rounded Up (inches)",
                             y="Average Pumpkin Count - Scaled Area")
                    y <- ggplotly(stackColor)
                    y
                } else{
                    if(input$exSpaceQtyPlot=="totalPlot"){
                        stackColor <- ggplot(getPumpkins, aes(x=diameterRoundUp,y=plotSumCount,fill=color)) +
                            geom_bar(position="stack", stat="identity") +
                            scale_fill_manual(values = colorFinderStack()) +
                            labs(title="Pumpkin Color Distribution",
                                 fill="Color",
                                 x="Pumpkin Diameter Rounded Up (inches)",
                                 y="Total Pumpkin Count - Harvest Area")
                        y <- ggplotly(stackColor)
                        y
                    } else{
                        if(input$exSpaceQtyPlot=="scaledTotalPlot"){
                            stackColor <- ggplot(getPumpkins, aes(x=diameterRoundUp,y=plotScaledSumCount,fill=color)) +
                                geom_bar(position="stack", stat="identity") +
                                scale_fill_manual(values = colorFinderStack()) +
                                labs(title="Pumpkin Color Distribution",
                                     fill="Color",
                                     x="Pumpkin Diameter Rounded Up (inches)",
                                     y="Total Pumpkin Count - Scaled Harvest Area")
                            y <- ggplotly(stackColor)
                            y
                        } else{}}}}}
    })
    
    # 100pct stack chart of pumpkin count by color
    output$exSpaceQtyColor <- renderPlotly({
        getPumpkins <- pumpkinsQtyDim()
        stackColor <- ggplot(getPumpkins, aes(x=eval(parse(text=qtyGroup())),fill=color)) +
            geom_bar(stat = "count",
                     position = "fill") +
            scale_fill_manual(values = colorFinderStack()) +
            scale_y_continuous(labels = scales::percent_format(scale = 100)) +
            theme(legend.position="none") +
            labs(title = paste0("Pumpkin Color Distribution"),
                 x=qtyGroup(),
                 y="Pumpkin Count %",
                 fill="Pumpkin Color")
        y <- ggplotly(stackColor)
        y
    })
    
    ############# SPACING CHARTS FOR METRICS ###############
    
    # Custom formula for density plots
    plot_multi_density <- function(df, feature, label_column) {
        plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
            geom_density(alpha=0.7) +
            labs(x=feature, y = "Density")
        plt + guides(fill=guide_legend(title=label_column))
    }
        
    # Custom formula for box plots
    plot_multi_box <- function(df, feature, label_column) {
        plt <- ggplot(df, aes(x=eval(parse(text=label_column)),y=eval(parse(text=feature)),fill=eval(parse(text=label_column)))) +
            geom_boxplot(varwidth = TRUE, alpha=0.2) +
            theme(legend.position="none") +
            labs(x=label_column,
                 y=feature)
        plt
    }
    
    # Custom formula for scatter plots
    plot_multi_scatter <- function(df, feature, label_column) {
        plt <- ggplot(df, aes(x=diameter,y=eval(parse(text=feature)),color=eval(parse(text=label_column)))) +
            geom_point() +
            labs(x="Pumpkin Diameter (Inches)",
                 y=feature,
                 color=label_column)
        plt
    }
    
    # Custom formula for histograms
    plot_multi_histogram <- function(df, feature, label_column) {
        plt <- ggplot(df, aes(x=eval(parse(text=feature)),fill=eval(parse(text=label_column)))) +
            geom_histogram(position="identity", alpha=0.2) +
            labs(x=feature,
                 y="Pumpkin Count")
        plt + guides(fill=guide_legend(title=label_column))
    }
    
    # REACTIVE chain for drop down selection values of spacing dimension
    dimChoices <- reactive({
        if(input$exSpaceMetArea=="All"){
            dimChoice <- c("All","10 x 1","10 x 2","10 x 3","10 x 4","5 x 2","5 x 4","5 x 6","5 x 8")}
        if(input$exSpaceMetArea=="10"){
            dimChoice <- c("All","10 x 1","5 x 2")}
        if(input$exSpaceMetArea=="20"){
            dimChoice <- c("All","10 x 2","5 x 4")}
        if(input$exSpaceMetArea=="30"){
            dimChoice <- c("All","10 x 3","5 x 6")}
        if(input$exSpaceMetArea=="40"){
            dimChoice <- c("All","10 x 4","5 x 8")}
        return(dimChoice)
    })
    output$exSpaceMetDim = renderUI({
        selectInput(inputId = "exSpaceMetDimReact", label = "Spacing Dimension", dimChoices())
    })
    
    # REACTIVE variables that react to user input
    # Solo reaction for group by
    metGroup <- reactive({
        # Apply user selected grouping
        metricGroupSelect <- input$exSpaceMetGroup
    })
    # REACTIVE chain for plant area and spacing dimension
    pumpkinsMetArea <- reactive({
        # Filter on user selected plant area
        if (input$exSpaceMetArea=="All") {
            pumpkinsMet <- pumpkins()
        } else{pumpkinsMet <- pumpkins() %>% filter(plantArea == input$exSpaceMetArea)}
    })
    pumpkinsMetDim <- reactive({
        # Filter on user selected spacing dimension
        if (input$exSpaceMetDimReact=="All") {
            pumpkinsMet <- pumpkinsMetArea()
        } else{pumpkinsMet <- pumpkinsMetArea() %>% filter(spacingDim == input$exSpaceMetDimReact)}
    })
    
    # Chart of volume
    output$exSpaceMetVol <- renderPlotly({
        withProgress(message = 'Generating Charts...', value = 0,{
            incProgress(0, detail = "for Volume")
        getPumpkins <- pumpkinsMetDim()
        # Return preferred plot
        if(input$exSpaceMetPlot=="density"){
        spaceMetVol <- plot_multi_density(getPumpkins, 'volumeEllipsoid', metGroup())
        spaceMetVol <- spaceMetVol + labs(title = paste0("Volume"),
                                          x = "Pumpkin Volume (Cubic Inches)")
        y <- ggplotly(spaceMetVol)
        incProgress(1/4, detail = "for Volume")
        y} else{
            if(input$exSpaceMetPlot=="boxplot"){
            spaceMetVol <- plot_multi_box(getPumpkins, 'volumeEllipsoid', metGroup())
            spaceMetVol <- spaceMetVol + labs(title = paste0("Volume"),
                                              y = "Pumpkin Volume (Cubic Inches)")
            y <- ggplotly(spaceMetVol)
            incProgress(1/4, detail = "for Volume")
            y
            } else{
                if(input$exSpaceMetPlot=="scatter"){
                    spaceMetVol <- plot_multi_scatter(getPumpkins, 'volumeEllipsoid', metGroup())
                    spaceMetVol <- spaceMetVol + labs(title = paste0("Volume by Diameter"),
                                                      y = "Pumpkin Volume (Cubic Inches)")
                    y <- ggplotly(spaceMetVol)
                    incProgress(1/4, detail = "for Volume")
                    y
                } else{
                    if(input$exSpaceMetPlot=="histogram"){
                        spaceMetVol <- plot_multi_histogram(getPumpkins, 'volumeEllipsoid', metGroup())
                        spaceMetVol <- spaceMetVol + labs(title = paste0("Volume"),
                                                          x = "Pumpkin Volume (Cubic Inches)")
                        y <- ggplotly(spaceMetVol)
                        incProgress(1/4, detail = "for Volume")
                        y
                        } else{}}}}
        }) # END Progress Bar
    })
    
    # Chart of weight
    output$exSpaceMetWei <- renderPlotly({
        withProgress(message = 'Generating Charts...', value = 0,{
            incProgress(0, detail = "for Weight")
            getPumpkins <- pumpkinsMetDim()
            # Return preferred plot
            if(input$exSpaceMetPlot=="density"){
                spaceMetWei <- plot_multi_density(getPumpkins, 'weight', metGroup())
                spaceMetWei <- spaceMetWei + labs(title = paste0("Weight"),
                                                  x = "Pumpkin Weight (lbs)")
                y <- ggplotly(spaceMetWei)
                incProgress(1/4, detail = "for Weight")
                y} else{
                    if(input$exSpaceMetPlot=="boxplot"){
                        spaceMetWei <- plot_multi_box(getPumpkins, 'weight', metGroup())
                        spaceMetWei <- spaceMetWei + labs(title = paste0("Weight"),
                                                          y = "Pumpkin Weight (lbs)")
                        y <- ggplotly(spaceMetWei)
                        incProgress(1/4, detail = "for Weight")
                        y
                    } else{
                        if(input$exSpaceMetPlot=="scatter"){
                            spaceMetWei <- plot_multi_scatter(getPumpkins, 'weight', metGroup())
                            spaceMetWei <- spaceMetWei + labs(title = paste0("Weight by Diameter"),
                                                              y = "Pumpkin Weight (lbs)")
                            y <- ggplotly(spaceMetWei)
                            incProgress(1/4, detail = "for Weight")
                            y
                        } else{
                            if(input$exSpaceMetPlot=="histogram"){
                                spaceMetWei <- plot_multi_histogram(getPumpkins, 'weight', metGroup())
                                spaceMetWei <- spaceMetWei + labs(title = paste0("Weight"),
                                                                  x = "Pumpkin Weight (lbs)")
                                y <- ggplotly(spaceMetWei)
                                incProgress(1/4, detail = "for Weight")
                                y
                                } else{}}}}
        }) # END Progress Bar
    })
    
    # Chart of diameter
    output$exSpaceMetDia <- renderPlotly({
        withProgress(message = 'Generating Charts...', value = 0,{
            incProgress(0, detail = "for Diameter")
            getPumpkins <- pumpkinsMetDim()
            # Return preferred plot
            if(input$exSpaceMetPlot=="density"){
                spaceMetDia <- plot_multi_density(getPumpkins, 'diameter', metGroup())
                spaceMetDia <- spaceMetDia + labs(title = paste0("Diameter"),
                                                  x = "Pumpkin Diameter (inches)")
                y <- ggplotly(spaceMetDia)
                incProgress(1/4, detail = "for Diameter")
                y} else{
                    if(input$exSpaceMetPlot=="boxplot"){
                        spaceMetDia <- plot_multi_box(getPumpkins, 'diameter', metGroup())
                        spaceMetDia <- spaceMetDia + labs(title = paste0("Diameter"),
                                                          y = "Pumpkin Diameter (Inches)")
                        y <- ggplotly(spaceMetDia)
                        incProgress(1/4, detail = "for Diameter")
                        y
                    } else{
                        if(input$exSpaceMetPlot=="scatter"){
                            spaceMetDia <- plot_multi_scatter(getPumpkins, 'diameter', metGroup())
                            spaceMetDia <- spaceMetDia + labs(title = paste0("Diameter by Diameter"),
                                                              y = "Pumpkin Diameter (Inches)")
                            y <- ggplotly(spaceMetDia)
                            incProgress(1/4, detail = "for Diameter")
                            y
                        } else{
                            if(input$exSpaceMetPlot=="histogram"){
                                spaceMetDia <- plot_multi_histogram(getPumpkins, 'diameter', metGroup())
                                spaceMetDia <- spaceMetDia + labs(title = paste0("Diameter"),
                                                                  x = "Pumpkin Diameter (Inches)")
                                y <- ggplotly(spaceMetDia)
                                incProgress(1/4, detail = "for Diameter")
                                y
                                } else{}}}}
        }) # END Progress Bar
    })
    
    # Chart of length
    output$exSpaceMetLen <- renderPlotly({
        withProgress(message = 'Generating Charts...', value = 0,{
            incProgress(0, detail = "for Length")
            getPumpkins <- pumpkinsMetDim()
            # Return preferred plot
            if(input$exSpaceMetPlot=="density"){
                spaceMetLen <- plot_multi_density(getPumpkins, 'length', metGroup())
                spaceMetLen <- spaceMetLen + labs(title = paste0("Length"),
                                                  x = "Pumpkin Length (inches)")
                y <- ggplotly(spaceMetLen)
                incProgress(1/4, detail = "for Length")
                y} else{
                    if(input$exSpaceMetPlot=="boxplot"){
                        spaceMetLen <- plot_multi_box(getPumpkins, 'length', metGroup())
                        spaceMetLen <- spaceMetLen + labs(title = paste0("Length"),
                                                          y = "Pumpkin Length (Inches)")
                        y <- ggplotly(spaceMetLen)
                        incProgress(1/4, detail = "for Length")
                        y
                    } else{
                        if(input$exSpaceMetPlot=="scatter"){
                            spaceMetLen <- plot_multi_scatter(getPumpkins, 'length', metGroup())
                            spaceMetLen <- spaceMetLen +
                                           geom_abline(slope=1,intercept=0) +
                                           geom_text(x=10, y=14, label="Tall Shaped",color="black") +
                                           geom_text(x=14, y=10, label="Short Shaped",color="black") +
                                           labs(title = paste0("Length by Diameter"),
                                                y = "Pumpkin Length (Inches)")
                            y <- ggplotly(spaceMetLen)
                            incProgress(1/4, detail = "for Length")
                            y
                        } else{
                            if(input$exSpaceMetPlot=="histogram"){
                                spaceMetLen <- plot_multi_histogram(getPumpkins, 'length', metGroup())
                                spaceMetLen <- spaceMetLen + labs(title = paste0("Length"),
                                                                  x = "Pumpkin Length (Inches)")
                                y <- ggplotly(spaceMetLen)
                                incProgress(1/4, detail = "for Length")
                                y
                                } else{}}}}
        }) # END Progress Bar
    })
    
    # TABLE
    
    output$tablePumpkinExplore <- DT::renderDataTable({
        getPumpkins <- pumpkins()
        customPrintName <- paste0(input$studySelection," Study Pumpkin Summary")
        customFileName <- paste0(input$studySelection," Study Pumpkin Summary")
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
    
    #####
    ##### ANALYSIS - SPACING
    #####
    
    # REACTIVE variables that react to user input
    # Solo reaction for group by
    userMetric <- reactive({
        # Apply user selected metric
        metricSelect <- input$aSpaceResponse
    })
    
    # Listen to any changed selections of pumpkin color to display correct image
    # NOTE: shinyapps.io requires file paths that are case sensitive (.jpg vs .JPG matters)
    observeEvent(input$aSpacePredictor,{
        if(input$aSpacePredictor == "plantArea"){
            output$aSpaceResponseImage <- renderUI({
                tags$img(src="images/spacing/drone/plantArea.JPG",width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("Plant Area - Total Area for Each Plant")
                )
            })
        }
        if(input$aSpacePredictor == "spacingDim"){
            output$aSpaceResponseImage <- renderUI({
                tags$img(src="images/spacing/drone/spacingDim.JPG",width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("Spacing Dimension - The Between Row by In Row Spacing")
                )
            })
        }
        if(input$aSpacePredictor == "betweenRow"){
            output$aSpaceResponseImage <- renderUI({
                tags$img(src="images/spacing/drone/betweenRow.JPG",width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("Between Row - The Distance Between Each Row")
                )
            })
        }
        if(input$aSpacePredictor == "inRow"){
            output$aSpaceResponseImage <- renderUI({
                tags$img(src="images/spacing/drone/inRow.JPG",width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("In Row - The Distance Between Plants on Each Row")
                )
            })
        }
    })
    
    # Value box render
    output$aSpaceAnovaArea <- renderValueBox({
        getPumpkins <- pumpkins()
        twoAnovaInteract <- aov(getPumpkins[[input$aSpaceResponse]] ~ plantArea * betweenRow,data=getPumpkins)
        twoSum <- summary(twoAnovaInteract)
        if(twoSum[[1]][["Pr(>F)"]][[1]] < 0.05){
            valueBox(value=tags$div(HTML(paste0(tags$h5("Pr(>F) = ",format(round(twoSum[[1]][["Pr(>F)"]][[1]],5),scientific=FALSE))))), # tags$h5 helps shrink the size of the number value in the box
                     subtitle=tags$div(HTML(paste0(tags$b("Significant"),tags$br(),tags$h4("Plant Area")))),
                     icon = icon("check-circle"),
                     color="green"
            )
        } else{
            valueBox(value=tags$div(HTML(paste0(tags$h5("Pr(>F) = ",format(round(twoSum[[1]][["Pr(>F)"]][[1]],5),scientific=FALSE))))), # tags$h5 helps shrink the size of the number value in the box
                     subtitle=tags$div(HTML(paste0(tags$b("Not Significant"),tags$br(),tags$h4("Plant Area")))),
                     icon = icon("times-circle"),
                     color="red"
            )
        }
    })
    output$aSpaceAnovaBetween <- renderValueBox({
        getPumpkins <- pumpkins()
        twoAnovaInteract <- aov(getPumpkins[[input$aSpaceResponse]] ~ plantArea * betweenRow,data=getPumpkins)
        twoSum <- summary(twoAnovaInteract)
        if(twoSum[[1]][["Pr(>F)"]][[2]] < 0.05){
            valueBox(value=tags$div(HTML(paste0(tags$h5("Pr(>F) = ",format(round(twoSum[[1]][["Pr(>F)"]][[2]],5),scientific=FALSE))))), # tags$h5 helps shrink the size of the number value in the box
                     subtitle=tags$div(HTML(paste0(tags$b("Significant"),tags$br(),tags$h4("Between Row")))),
                     icon = icon("check-circle"),
                     color="green"
            )
        } else{
            valueBox(value=tags$div(HTML(paste0(tags$h5("Pr(>F) = ",format(round(twoSum[[1]][["Pr(>F)"]][[2]],5),scientific=FALSE))))), # tags$h5 helps shrink the size of the number value in the box
                     subtitle=tags$div(HTML(paste0(tags$b("Not Significant"),tags$br(),tags$h4("Between Row")))),
                     icon = icon("times-circle"),
                     color="red"
            )
        }
    })
    output$aSpaceAnovaInteract <- renderValueBox({
        getPumpkins <- pumpkins()
        twoAnovaInteract <- aov(getPumpkins[[input$aSpaceResponse]] ~ plantArea * betweenRow,data=getPumpkins)
        twoSum <- summary(twoAnovaInteract)
        if(twoSum[[1]][["Pr(>F)"]][[3]] < 0.05){
            valueBox(value=tags$div(HTML(paste0(tags$h5("Pr(>F) = ",format(round(twoSum[[1]][["Pr(>F)"]][[3]],5),scientific=FALSE))))), # tags$h5 helps shrink the size of the number value in the box
                     subtitle=tags$div(HTML(paste0(tags$b("Significant"),tags$br(),tags$h4("Interaction")))),
                     icon = icon("check-circle"),
                     color="green"
            )
        } else{
            valueBox(value=tags$div(HTML(paste0(tags$h5("Pr(>F) = ",format(round(twoSum[[1]][["Pr(>F)"]][[3]],5),scientific=FALSE))))), # tags$h5 helps shrink the size of the number value in the box
                     subtitle=tags$div(HTML(paste0(tags$b("Not Significant"),tags$br(),tags$h4("Interaction")))),
                     icon = icon("times-circle"),
                     color="red"
            )
        }
    })
    
    # A two-way ANOVA with interaction
    output$aSpaceAnovaTable <- renderPrint({
        getPumpkins <- pumpkins()
        twoAnovaInteract <- aov(getPumpkins[[input$aSpaceResponse]] ~ plantArea * betweenRow,data=getPumpkins)
        print(twoAnovaInteract)
        print(summary(twoAnovaInteract))
        #cat("Coefficients"); cat("\n")
        #print(twoAnovaInteract$coefficients)
    })
    # Corresponding coefficients table
    output$aSpaceCoefTable <- function() {
        getPumpkins <- pumpkins()
        twoAnovaInteract <- aov(getPumpkins[[input$aSpaceResponse]] ~ plantArea * betweenRow,data=getPumpkins)
        coef <- as.tibble(cbind(names(twoAnovaInteract$coefficients),round(twoAnovaInteract$coefficients,2)))
        names(coef) <- c("Term","Coefficient")
        coef[1,1] <- "Mean (Intercept)"
        coef %>%
            knitr::kable("html") %>%
            kable_styling("striped",full_width=TRUE,position="left",fixed_thead = TRUE)
    }
    # Corresponding Tukey
    output$aSpaceTukeyTable <- renderPrint({
        getPumpkins <- pumpkins()
        twoAnovaInteract <- aov(getPumpkins[[input$aSpaceResponse]] ~ plantArea * betweenRow,data=getPumpkins)
        print(TukeyHSD(twoAnovaInteract))
    })
    # Corresponding interaction plot
    output$aSpaceIntPlot <- renderPlotly({
        yLabel <- ifelse(userMetric()=="volumeEllipsoid","Average Pumpkin Volume (cubic inches)",
                         ifelse(userMetric()=="diameter","Average Pumpkin Diameter (inches)",
                                ifelse(userMetric()=="length","Average Pumpkin Length (inches)",
                                       ifelse(userMetric()=="weight","Average Pumpkin Weight (lbs)",
                                              userMetric()))))
        getPumpkins <- pumpkins()
        getPumpkins <- getPumpkins %>% group_by(plantArea,betweenRow) %>% summarise(mean(eval(parse(text=userMetric()))))
        names(getPumpkins) <- c("plantArea","betweenRow",userMetric())
        intPlot <- ggplot(getPumpkins, aes(x=plantArea,y=eval(parse(text=userMetric())),color=betweenRow)) +
            geom_line(aes(group=betweenRow)) +
            geom_point() +
            labs(x="Plant Area (square feet)",
                 y=yLabel,
                 color="" # Hides the ggplot legend title as blank so plotly legend title below will show alone
                 )
        y <- ggplotly(intPlot) %>% layout(legend=list(orientation="v",traceorder="reversed",title=list(text="Between Row (feet)"))) # orientation="h" legend position on top, traceorder changes sort of legend values
        y
    })
    
    # Spacing analysis - explanation of results
    output$aSpaceSigResult <- renderUI({
        tags$div("The ",
                 tags$b(tags$span(style="color:green", "green")),
                 " and ",
                 tags$b(tags$span(style="color:red", "red")),
                 " boxes indicate if Plant Area, Between Row, or their Interaction together have a significant effect on the selected response mean of pumpkin volume, diameter, length, or weight. Significance is determined by the corresponding p-value associated with the F statistic - Pr(>F) as being < 0.05 in value. The null hypothesis of Plant Area, Between Row, or their Interaction having no effect on the reseponse mean is evaluated by this p-value. If significant, the null hypothesis is rejected.",
                 tags$h4("Two-Way ANOVA Result"),
                 "Explain two-way ANOVA output.",
                 tags$h4("Interaction Plot"),
                 "Explain interaction plot.",
                 tags$h4("Coefficients"),
                 "Explain coefficient values.",
                 tags$h4("Tukey Results"),
                 "Explain Tukey and corresponding p.",
                 )
    })
#}
    
    ###################### RENDER NITROGEN OUTPUT ################################################
    
    #####
    ##### INFORMATION - NITROGEN
    #####
    
    output$infoNitDown <- renderUI({
        tags$div(tags$br(),
                 HTML(paste("All of the prepared data from the nitrogen level study can be downloaded in the preferred 
                            file format using the below links:")),
                 tags$br(),
                 tags$br(),
                 a(href="files/R/pumpkinData.RData", "Download R", download=NA, target="_blank"),
                 tags$i("(Object Name: nitrogenData)"),
                 tags$br(),
                 a(href="files/SAS/nitrogendata.sas7bdat", "Download SAS", download=NA, target="_blank"),
                 tags$br(),
                 a(href="files/Excel/nitrogenData.xlsx", "Download Excel", download=NA, target="_blank")
        )
    })
    
    ###################### RENDER LEAF OUTPUT ################################################
    
    #####
    ##### INFORMATION - LEAF
    #####
    
    output$infoLeafDown <- renderUI({
        tags$div(tags$br(),
                 HTML(paste("All of the prepared data from the leaf study can be downloaded in the preferred 
                            file format using the below links:")),
                 tags$br(),
                 tags$br(),
                 a(href="files/R/pumpkinData.RData", "Download R", download=NA, target="_blank"),
                 tags$i("(Object Name: leafData)"),
                 tags$br(),
                 a(href="files/SAS/leafdata.sas7bdat", "Download SAS", download=NA, target="_blank"),
                 tags$br(),
                 a(href="files/Excel/leafData.xlsx", "Download Excel", download=NA, target="_blank")
        )
    })
} # END OF function
