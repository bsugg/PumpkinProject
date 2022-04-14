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

# Call data generated from save function within the "README.RMD" file
load("pumpkinData.RData")

# Utilize shinyjs package for javascript code to collapse boxes on command
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

function(input, output, session) {
    
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
                             menuSubItem("Charts for Quantity",tabName = "exploreSpaceBasic"),
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
    
    ###################### REACTIVE ELEMENTS #####################################
    
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
    
    ###################### RENDER SPACING OUTPUT ################################################
    
    #####
    ##### UI ACTIONS
    #####
    
    ###
    ### SIDEBAR
    ###
    
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
                 tags$b("Background:"),
                 tags$br(),
                 HTML(paste("Pumpkins are an emerging specialty crop in North Carolina. In 2019 the state produced 713,000 cwt of pumpkins on 3,100 acres, ranking 6th in the United States where overall national production is steadily increasing ")),
                 tags$a(href="https://usda.library.cornell.edu/concern/publications/02870v86p?locale=en","(USDA Feb 16, 2022)",target="_blank"),
                 HTML(paste(". With the rise in production there is an increasing need for providing clear guidelines to North Carolina farmers on how plant density impacts overall yield.")),
                 tags$br(),
                 tags$br(),
                 tags$b("Why is plant density important?"),
                 tags$ul(
                          tags$li("As plant density increases, yields will generally increase"), 
                          tags$ul(tags$li("Decrease vegetative growth, increase reproductive growth")), 
                          tags$li("Increased plant density aids in weed maintenance"),
                          tags$li("Fuller canopies protect fruit from sun damage")
                 ),
                 tags$b("Objectives of Study:"),
                 tags$ul(
                     tags$li("Understand the impact of spacing on pumpkin production and fruit size"), 
                     tags$li("Find the pumpkin spacing which produces the highest yield"),
                     tags$li("Compare 10 ft and 5 ft between-row spacing")
                 ),
                 tags$b("Methods:"),
                 tags$br(),
                 HTML(paste("The Spacing Study was planted for two seasons in 2020 and 2021 at the NC State Extension Upper Mountain Research Station under the care of NC Extension Agents. Crops were planted in June and harvested in September during both years after a 12 week growing period.")),
                 tags$ul(
                     tags$li("Four different plant areas were studied, each with a corresponding 10 ft and 5 ft between-row spacing distance"), 
                     tags$ul(tags$li(HTML(paste("10 ft", tags$sup(2),", 20 ft", tags$sup(2),", 30 ft", tags$sup(2),", and 40 ft", tags$sup(2), sep = "")))),
                     tags$li("Variety: Kratos pumpkins (20-30 lbs average weight)"),
                     tags$li("Fruit was evaluated for size, weight, and maturity (orange or green in color)")
                 ),
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
        pumpkinsAreaTable <- pumpkinsAreaTable %>% select(spacingDim,plot,pumpkinID,diameter,volumeEllipsoid,weight,standCount,standCountIdeal,standCountIdealPct) %>%
            group_by(spacingDim,plot) %>% summarise("Pumpkin Count"=n(),
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
                     tags$caption("Growth by Spacing Dimensions: 10 x 1 (Left), 5 x 2 (Right)")
            )
        } else{
            if(input$infoSpaceSelectArea=="20"){
                tags$img(src="images/spacing/drone/plantSpace20.JPG",
                         width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("Growth by Spacing Dimensions: 10 x 2 (Left), 5 x 4 (Right)")
                )
            } else{
                if(input$infoSpaceSelectArea=="30"){
                    tags$img(src="images/spacing/drone/plantSpace30.JPG",
                             width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                             tags$caption("Growth by Spacing Dimensions: 10 x 3 (Left), 5 x 6 (Right)")
                    )
                } else{
                    if(input$infoSpaceSelectArea=="40"){
                        tags$img(src="images/spacing/drone/plantSpace40.JPG",
                                 width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                                 tags$caption("Growth by Spacing Dimensions: 10 x 4 (Left), 5 x 8 (Right)")
                        )
                    } else{}}}}
    })
    output$infoSpaceHarvestImage <- renderUI({
        if(input$infoSpaceSelectArea=="10"){
            tags$img(src="images/spacing/drone/plantSpaceHarvest10.JPG",
                     width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                     tags$caption("Harvest by Spacing Dimensions: 10 x 1 (Left), 5 x 2 (Right)")
            )
        } else{
            if(input$infoSpaceSelectArea=="20"){
                tags$img(src="images/spacing/drone/plantSpaceHarvest20.JPG",
                         width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                         tags$caption("Harvest by Spacing Dimensions: 10 x 2 (Left), 5 x 4 (Right)")
                )
            } else{
                if(input$infoSpaceSelectArea=="30"){
                    tags$img(src="images/spacing/drone/plantSpaceHarvest30.JPG",
                             width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                             tags$caption("Harvest by Spacing Dimensions: 10 x 3 (Left), 5 x 6 (Right)")
                    )
                } else{
                    if(input$infoSpaceSelectArea=="40"){
                        tags$img(src="images/spacing/drone/plantSpaceHarvest40.JPG",
                                 width="100%",style="display: block; margin-left: auto; margin-right: auto;",
                                 tags$caption("Harvest by Spacing Dimensions: 10 x 4 (Left), 5 x 8 (Right)")
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
    
    # 100pct stack chart color finder

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
    
    # 100pct stack chart of pumpkin count by color by spacing dimension
    output$exSpaceStackColorDim <- renderPlotly({
        getPumpkins <- pumpkins()
        stackColorDim <- ggplot(getPumpkins, aes(x=spacingDim,fill=color)) +
            geom_bar(stat = "count",
                     position = "fill") +
            scale_fill_manual(values = colorFinderStack()) +
            scale_y_continuous(labels = scales::percent_format(scale = 100)) +
            theme(legend.position="none") +
            labs(x="Spacing Dimension (Feet)",
                 y="Pumpkin Count %",
                 fill="Pumpkin Color")
        y <- ggplotly(stackColorDim)
        y
    })
    
    # 100pct stack chart of pumpkin count by color by plant area
    output$exSpaceStackColorArea <- renderPlotly({
        getPumpkins <- pumpkins()
        stackColorArea <- ggplot(getPumpkins, aes(x=plantArea,fill=color)) +
            geom_bar(stat = "count",
                     position = "fill") +
            scale_fill_manual(values = colorFinderStack()) +
            scale_y_continuous(labels = scales::percent_format(scale = 100)) +
            theme(legend.position="none") +
            labs(x="Plant Area (Square Feet)",
                 y="Pumpkin Count %",
                 fill="Pumpkin Color")
        y <- ggplotly(stackColorArea)
        y
    })
    
    # 100pct stack chart of pumpkin count by color by between row
    output$exSpaceStackColorBR <- renderPlotly({
        getPumpkins <- pumpkins()
        stackColorBR <- ggplot(getPumpkins, aes(x=betweenRow,fill=color)) +
            geom_bar(stat = "count",
                     position = "fill") +
            scale_fill_manual(values = colorFinderStack()) +
            scale_y_continuous(labels = scales::percent_format(scale = 100)) +
            theme(legend.position="none") +
            labs(x="Between Row Distance (Feet)",
                 y="Pumpkin Count %",
                 fill="Pumpkin Color")
        y <- ggplotly(stackColorBR)
        y
    })
    
    # Boxplot of volume by spacing dimension
    output$exSpaceBoxVolDim <- renderPlotly({
        getPumpkins <- pumpkins()
        boxVolDim <- ggplot(getPumpkins, aes(x=spacingDim,y=volumeEllipsoid,fill=spacingDim)) +
            geom_boxplot(varwidth = TRUE, alpha=0.2) +
            theme(legend.position="none") +
            labs(x="Spacing Dimension (Feet)",
                 y="Pumpkin Volume (Cubic Inches)")
        y <- ggplotly(boxVolDim)
        y
    })
    
    # Boxplot of volume by plant area
    output$exSpaceBoxVolArea <- renderPlotly({
        getPumpkins <- pumpkins()
        boxVolArea <- ggplot(getPumpkins, aes(x=plantArea,y=volumeEllipsoid,fill=plantArea)) +
            geom_boxplot(varwidth = TRUE, alpha=0.2) +
            theme(legend.position="none") +
            labs(x="Plant Area (Square Feet)",
                 y="Pumpkin Volume (Cubic Inches)")
        y <- ggplotly(boxVolArea)
        y
    })
    
    # Boxplot of volume by between row
    output$exSpaceBoxVolBR <- renderPlotly({
        getPumpkins <- pumpkins()
        boxVolBR <- ggplot(getPumpkins, aes(x=betweenRow,y=volumeEllipsoid,fill=betweenRow)) +
            geom_boxplot(varwidth = TRUE, alpha=0.2) +
            theme(legend.position="none") +
            labs(x="Between Row Distance (Feet)",
                 y="Pumpkin Volume (Cubic Inches)")
        y <- ggplotly(boxVolBR)
        y
    })
    
    # Histogram of diameter by spacing dimension
    output$exSpaceHistDiaDim <- renderPlotly({
        getPumpkins <- pumpkins()
        histDiaDim <- ggplot(getPumpkins, aes(x=diameter, fill=spacingDim)) +
            geom_histogram(position="identity", alpha=0.2) +
            labs(x="Pumpkin Diameter (Inches)",
                 y="Pumpkin Count")
        y <- ggplotly(histDiaDim)
        y
    })
    
    # Histogram of diameter by plant area
    output$exSpaceHistDiaArea <- renderPlotly({
        getPumpkins <- pumpkins()
        histDiaArea <- ggplot(getPumpkins, aes(x=diameter, fill=plantArea)) +
            geom_histogram(position="identity", alpha=0.2) +
            labs(x="Pumpkin Diameter (Inches)",
                 y="Pumpkin Count")
        y <- ggplotly(histDiaArea)
        y
    })
    
    # Histogram of diameter by between row
    output$exSpaceHistDiaBR <- renderPlotly({
        getPumpkins <- pumpkins()
        histDiaBR <- ggplot(getPumpkins, aes(x=diameter, fill=betweenRow)) +
            geom_histogram(position="identity", alpha=0.2) +
            labs(x="Pumpkin Diameter (Inches)",
                 y="Pumpkin Count")
        y <- ggplotly(histDiaBR)
        y
    })
    
    # Bar chart of pumpkin count by spacing dimension
    output$exSpaceBarCountDim <- renderPlotly({
        getPumpkins <- pumpkins()
        barCountDim <- ggplot(getPumpkins, aes(x=spacingDim,fill=spacingDim)) +
            geom_bar(stat = "count") +
            theme(legend.position="none") +
            labs(x="Spacing Dimension (Feet)",
                 y="Pumpkin Count")
        y <- ggplotly(barCountDim)
        y
    })
    
    # Bar chart of pumpkin count by plant area
    output$exSpaceBarCountArea <- renderPlotly({
        getPumpkins <- pumpkins()
        barCountArea <- ggplot(getPumpkins, aes(x=plantArea,fill=plantArea)) +
            geom_bar(stat = "count") +
            theme(legend.position="none") +
            labs(x="Plant Area (Square Feet)",
                 y="Pumpkin Count")
        y <- ggplotly(barCountArea)
        y
    })
    
    # Bar chart of pumpkin count by between row
    output$exSpaceBarCountBR <- renderPlotly({
        getPumpkins <- pumpkins()
        barCountBR <- ggplot(getPumpkins, aes(x=betweenRow,fill=betweenRow)) +
            geom_bar(stat = "count") +
            theme(legend.position="none") +
            labs(x="Between Row Distance (Feet)",
                 y="Pumpkin Count")
        y <- ggplotly(barCountBR)
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
    
    ###################### RENDER NITROGEN OUTPUT ################################################
    
    #####
    ##### UI ACTIONS
    #####
    
    ###
    ### SIDEBAR
    ###
    
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
    ##### UI ACTIONS
    #####
    
    ###
    ### SIDEBAR
    ###
    
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
