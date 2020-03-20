library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(readr)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)
library(googleVis)

# Any JS functions
jsCode = 'shinyjs.clear_warning = function(){document.getElementById("note_save_confirm").innerHTML = "";}'

# Loading Spinner Settings
options(spinner.color="#ffd700", spinner.type = 2, spinner.color.background = 'white' )


# Header
header <- dashboardHeader(disable = TRUE)

# Sidebar
sidebar <- dashboardSidebar(disable=FALSE,width='190px',
                            # Custom CSS to hide the default logout panel
                            tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
                            tags$head(tags$style(HTML('.box.box-solid.box-primary>.box-header{ background-color: #D2D6DE; }'))),
                            tags$head(tags$style(HTML('.box.box-solid.box-primary{ border: 1px solid #D2D6DE;'))),
                            tags$head(tags$style(HTML('.box.box-primary{ border-top-color: #D2D6DE;}'))),
                            tags$head(tags$style(HTML('.box.box-solid.box-primary>.box-header>.box-title{ color: #242D32;}'))),
                            
                            tags$head(tags$style(HTML('.content-wrapper{ background-color: #c8c9c7; }'))),
                            tags$head(tags$style(HTML('.content-wrapper{ background-color: #ECF1F5; }'))),
                            ###
                            tags$head(tags$style(HTML('.irs-bar{ background: #ffd700; border-top: #ffd700; border-bottom: #ffd700;}'))),
                            tags$head(tags$style(HTML('.irs-bar-edge{ background: #ffd700; border: #ffd700;}'))),
                            tags$head(tags$style(HTML('.irs-single{ background: #888B8D;}'))),
                            tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active{ border-top-color: #888B8D;}'))),
                            
                            # Hide error messages while calculating predictions/plots
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            # Add icons
                            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/icon?family=Material+Icons"),
                            tags$link(rel = 'stylesheet', href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
                            #Lato Font
                            tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Lato'),
                            # JavaScript
                            tags$head(tags$script(src="script.js")),
                            # CSS
                            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style_sheet.css")),
                            # Extend JS
                            useShinyjs(),
                            extendShinyjs(text = jsCode, functions = 'clear_warning'),
                            
                            tags$head(tags$style(HTML('.skin-black .wrapper {background-color: 	#ECF1F5;}'))),
                            
                            #### ####
                            
                            sidebarMenu(id = 'tabs', 
                                        selectInput("partition", "Area", 
                                                    choices = c("Republic of Ireland", "Island of Ireland"),
                                                    selected = "Island of Ireland"),
                                        menuItem("Time", icon = icon("line-chart"), tabName = "time"),
                                        menuItem("Map and reports", icon = icon("list"), tabName = "nphet"),
                                        menuItem("Compare", icon = icon("globe"), tabName = "compare"),
                                        # menuItem("Map", icon = icon("map"), tabName = "map"),
                                        menuItem("Daily Data", icon = icon("eye"), tabName = "data")
                            )
                            
                            
                            
)

# Body
body <- dashboardBody(
  
    # tabs
    tabItems(
  
        # # Map Tab #
        # tabItem("map",   
        #         h3(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
        #                   "HSE Coronavirus information", 
        #                   target="_blank")),
        #         h3(paste0("Data from Ireland ("),
        #            tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/", 
        #                   "Department of Health", target="_blank"),
        #            paste0("), Northern Ireland ("),
        #            tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
        #                   "NHS", target="_blank"),
        #            paste0(") and WHO")),
        #         fluidRow(
        #           box(title = "Map of cases using Belfast, Cork, Dublin, Letterkenny and Galway as locations",
        #               footer = "Note: no locations for 121 cases announced March 12/13/18th",
        #               width = 12,
        #               leafletOutput("map", width = "70%", height = 600)
        #             )
        #           ),
        #         h3(paste0("Developed by Dr. Andrew Simpkin "),
        #            tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1"),
        #            paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
        #            tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer"))
        # 
        # ),
        
        # Time series Tab #
        tabItem("time", 
                h3(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
                                   "HSE Coronavirus information", 
                                   target="_blank")),
                h3(paste0("Data (18/03/2020) from Ireland ("),
                   tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/", 
                          "Department of Health", target="_blank"),
                   paste0("), Northern Ireland ("),
                   tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                          "NHS", target="_blank"),
                   paste0(") and WHO")),
                valueBoxOutput("CasesBox"),
                valueBoxOutput("MortBox"),
                
                fluidRow(
                    box(width = 12, title = "Cumulative and new cases per day", 
                        plotlyOutput("cumulcases", width = "80%", height = 400)
                        # ,
                        # plotlyOutput("cumularea", width = "80%", height = 400)
                        # ,
                        # plotlyOutput("cumulgender", width = "80%", height = 400)
                    )
                ),
                h3(paste0("Developed by Dr. Andrew Simpkin "),
                            tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1", target="_blank"),
                            paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
                            tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer", target="_blank"))
        ),
        
        # Country comparison Tab #
        tabItem("compare", 
                h3(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
                          "HSE Coronavirus information", 
                          target="_blank")),
                h3(paste0("Data from Ireland ("),
                   tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/", 
                          "Department of Health", target="_blank"),
                   paste0("), Northern Ireland ("),
                   tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                          "NHS", target="_blank"),
                   paste0(") and WHO")),
                h3("Number of cases from other countries are scaled to reflect the Irish population. For example: ROI+NI (6.712 million people) is about 11% of Italy's population (60.48m), so 100 cases in Italy is equivalent to 11 cases in Ireland"),
                fluidRow(
                  column(width=6,
                         selectInput("place", "Country to compare", 
                                     choices = c("france", "germany", "italy", "spain", "uk"), 
                                     selected = "france")),
                  column(width=6,
                         sliderInput("days","Decide the time difference",min = -5,max=30,value=0)
                  )
                ),
                fluidRow(
                  box(width = 12, 
                      title = "Compare cases with other countries scaled by population, with time shifted", 
                      plotlyOutput("irelandcompare", width = "90%", height = 500) 
                  )
                ),
                h3(paste0("Developed by Dr. Andrew Simpkin "),
                   tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1"),
                   paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
                   tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer"))
        ),
        
        # Data view Tab #
        tabItem("data",
                h3(paste0("Data from Ireland ("),
                   tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/", 
                          "Department of Health", target="_blank"),
                   paste0("), Northern Ireland ("),
                   tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                                 "NHS", target="_blank"),
                   paste0(") and WHO")),
                fluidRow(box(width = 12, title = "Raw data",
                             column(10, DTOutput("dattable"))
                )),
                # fluidRow(box(width = 12, title = "Total cases by Area (Missing area for 121 cases)",
                #              column(10, DTOutput("areatable"))
                # )),
                # fluidRow(box(width = 12, title = "Total cases by Gender (ROI only and 47 with no gender information)",
                #              column(10, DTOutput("gendertable"))
                # )),
                h3(paste0("Developed by Dr. Andrew Simpkin "),
                   tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1"),
                   paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
                   tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer"))
              ),
        
        
        # Weekly Tab
        tabItem("nphet",
                h3(paste0("Data as of midnight on 17/03/2020 from Ireland"),
                   tags$a(href="https://www.gov.ie/en/publication/cd0cea-an-analysis-of-the-266-cases-of-covid-19-in-ireland-as-of-march-16-2/", 
                          "(National Public Health Emergency Team)", target="_blank")),
                valueBoxOutput("HospBox"),
                valueBoxOutput("ICUBox"),
                valueBoxOutput("CFRBox"),
                fluidRow(box(title = "Map of cases by county (ROI only)", width = 12,
                             leafletOutput("map", width = "70%", height = 600)
                             )),
                fluidRow(box(width = 12, title = "Weekly area data",
                             column(10, DTOutput("weekarea"))
                )),
                fluidRow(box(title = "Cases by Age Group", width = 12,
                             plotlyOutput("ageplot", width = "70%", height = 600)
                )),
                fluidRow(box(width = 12, title = "Weekly age data",
                             column(10, DTOutput("weekage"))
                )),
                fluidRow(box(title = "Cases by Transmission type", width = 12,
                             plotlyOutput("transplot", width = "70%", height = 600)
                )),
                fluidRow(box(width = 12, title = "Weekly transmission data",
                             column(10, DTOutput("weektrans"))
                )),
                h3(paste0("Developed by Dr. Andrew Simpkin "),
                   tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1"),
                   paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
                   tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer"))
        )
    ) # end of tabItems
    
)# end of body



# Define server logic required to draw a leaflet
server <- function(input, output) {
    
    # read in all data
    dataRaw <- reactive({
      dat1 <- read.csv("data/corona_island.csv") %>% 
        mutate(pop = case_when(country=="ireland"~6.712,
                               country=="france"~66.99,
                               country=="germany"~82.79,
                               country=="italy"~60.48,
                               country=="spain"~46.66,
                               country=="uk"~66.44)) 

      dat2 <- read.csv("data/corona_island.csv") %>% 
        mutate(pop = case_when(country=="ireland"~4.88,
                               country=="france"~66.99,
                               country=="germany"~82.79,
                               country=="italy"~60.48,
                               country=="spain"~46.66,
                               country=="uk"~66.44))%>%
          filter(area!="north")
      
      if(input$partition == "Island of Ireland") return(dat1)
      if(input$partition == "Republic of Ireland") return(dat2)
      
    })
    
    # gender data
    dataGender <- reactive({
      read.csv("data/corona_island_gender.csv")
    })
    
    # irish data only
    dataIreland <- reactive({
      dataRaw() %>% filter(country == "ireland")
    })

    # Country comparison data
    dataCountry <- reactive({
      dataRaw() %>% filter(country=="ireland" | country == input$place)
    })
    
    # irish area data
    dataArea <- reactive({        
    dataIreland() %>% 
            mutate(long = case_when(area=="east" ~ -6.2690,
                                    area=="west" ~ -9.0650,
                                    area=="south" ~ -8.5110,
                                    area=="north" ~ -5.954,
                                    area=="northwest" ~ -7.7328),
                   lat = case_when(area=="east" ~ 53.3593,
                                   area=="west" ~ 53.2760,
                                   area=="south" ~ 51.8830,
                                   area=="north" ~ 54.588,
                                   area=="northwest" ~ 54.9598))  %>% 
        group_by(area) %>% summarise(ncases=sum(ncase,na.rm=TRUE),
                                             ndeaths=sum(ndeath,na.rm=TRUE),
                                             long = mean(long,na.rm=TRUE),
                                             lat = mean(lat, na.rm=TRUE)) %>%
            na.omit() 
    })

    
 
    ## Interactive Map ###########################################
    
    # # Create the map
    # output$map <- renderLeaflet({
    #     dataArea() %>%
    #         leaflet() %>%
    #         addTiles(
    #             urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    #         ) %>%
    #         setView(lng = -8, lat = 53.5, zoom = 7) %>%
    #         clearShapes() %>%
    #         addCircleMarkers(lng= ~long, 
    #                          lat= ~lat, 
    #                          layerId = ~area,
    #                          radius = ~ncases/2)
    # })
    # 
    # # Show a popup at the given location
    # showAreaPopup <- function(t, lat, long) {
    #     selectedArea <- dataArea()[dataArea()$area == t,]
    #     content <- as.character(tagList(
    #         tags$h4("Number of cases:", as.integer(selectedArea$ncases)),
    #         tags$h4("Number of deaths:", as.integer(selectedArea$ndeaths))
    #     ))
    #     leafletProxy("map") %>% 
    #         addPopups(long, lat, content, layerId = t)
    # }
    # 
    # # When map is clicked, show a popup with city info
    # observe({
    #     leafletProxy("map") %>% 
    #         clearPopups()
    #     event <- input$map_marker_click
    #     if (is.null(event))
    #         return()
    #     
    #     isolate({
    #         showAreaPopup(event$id, event$lat, event$lng)
    #     })
    # })
    
    ################### Info boxes ##########
    ## Info box
    output$CasesBox <- renderValueBox({
      dat <- dataIreland()
      valueBox(paste0(sum(dat$ncase,na.rm=TRUE)), "Confirmed cases",
        color = "purple"
      )
    })
    
    ## mort box
    output$MortBox <- renderValueBox({
      dat <- dataIreland()
      valueBox(
        paste0(sum(dat$ndeath,na.rm=TRUE)), "Total deaths",
        color = "red"
      )
    })

    
    ################### Time series plots ##############
    #### Plotly cases per day
    output$cumulcases <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))  %>%
        mutate(Growth_Percentage = round(((Total_cases/c(NA,Total_cases[1:(length(Total_cases)-1)]))-1)*100,0)) %>%
        ggplot(aes(x=date,y=ccases,label=Date,label1=Total_cases,label2=New_cases, label3=Growth_Percentage)) + 
        geom_line() + geom_point() + geom_col(aes(x=date,y=ncases)) +
        theme(legend.position="none") + labs(y="Cases")
      ggplotly(g, tooltip = c("Date","Total_cases","New_cases", "Growth_Percentage"))
      
    })
    
    # #### Plotly cases per day by area
    output$cumularea <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,area) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        group_by(area) %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ccases,color=area,label=Date,label1=Total_cases,label2=New_cases, label3=area)) + 
        geom_line() + geom_point() + labs(y="Cases", title = "Cumulative cases by area")
      ggplotly(g, tooltip = c("Date", "area", "Total_cases","New_cases"))
      
    })

    
    # #### Plotly cases per day by gender
    output$cumulgender <- renderPlotly({
      
      g = dataGender() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,gender) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        group_by(gender) %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ccases,color=gender,label=Date,label1=Total_cases,label2=New_cases)) + 
        geom_line() + geom_point() + labs(y="Cases", title = "Cumulative cases by gender (ROI only)")
      ggplotly(g, tooltip = c("Date", "gender", "Total_cases","New_cases"))
      
    })    

    ################### Comparison plots ############
    ##### Plotly Ireland v other countries
    output$irelandcompare <- renderPlotly({
      
      
      dat <- dataCountry()
      dat <- dat %>% mutate(date = as.Date(date,format = "%d/%m/%Y"), country = as.character(country)) 
      prop = mean(dat$pop[dat$country=="ireland"])/mean(dat$pop[dat$country==input$place])
      dat$ncase[dat$country==input$place] <- round(dat$ncase[dat$country==input$place]*prop,0)
      dat$date[dat$country==input$place] <- dat$date[dat$country==input$place]+input$days
      
      diffdat = dat %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y"))%>%
        filter(country=="ireland"|country==input$place) %>%
        group_by(country, date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date), Country = country[1]) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))
      
      ireland <- diffdat %>% filter(country == "ireland")
      comp <- diffdat %>% filter(country == input$place)
      comp <- merge(ireland,comp,by="date") %>%
        mutate(diff = abs(Total_cases.x - Total_cases.y)) %>%
        select(date,diff)
      
      
      dat$country[dat$country==input$place] <- paste(input$place, "scaled to ireland +",input$days,"days")
      
      g = dat %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))%>%
        group_by(country, date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date), Country = country[1]) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ccases,color=country,fill=country,label=Date,label1=Country,label2=Total_cases)) +
        geom_line() + geom_point() + labs(y="Cases (scaled to Ireland)") + theme_bw() + 
        ggtitle(paste0("Mean daily difference is ",round(mean(comp$diff,na.rm=TRUE),0), " cases"))
      ggplotly(g, tooltip = c("Country","Date","Total_cases","New_cases"))
      
    })
    
    
    
    #################### Data Table  ###################
    output$dattable <- renderDT({
      
      dataRaw() %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))
      
    })
    
    output$areatable <- renderDT({
      
      dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,area) %>%
        summarise(New_cases = sum(ncase)) %>%
        na.omit() %>%
        group_by(area) %>%
        mutate(Total_cases = cumsum(New_cases)) %>%
        filter(date == max(date), area!="unknown") %>%
        select(date,area,Total_cases) %>%
        group_by(date) %>%
        mutate(Total_cases= Total_cases,
               Percentage = round(100*Total_cases/sum(Total_cases),0))
      
    })
    
    output$gendertable <- renderDT({
      
      dataGender() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,gender) %>%
        summarise(New_cases = sum(ncase)) %>%
        na.omit() %>%
        group_by(gender) %>%
        mutate(Total_cases= cumsum(New_cases)) %>%
        filter(date == max(date), gender != "unknown") %>%
        select(date,gender,Total_cases) %>%
        group_by(date) %>%
        mutate(Total_cases= Total_cases,
               Percentage = round(100*Total_cases/sum(Total_cases),0))
      
    })
    
    
    ######################### weekly tab
    dataCounty <- reactive({
      read.csv("data/corona_county.csv")
    })
    dataAge <- reactive({
      read.csv("data/corona_age.csv") %>%
        mutate(age_group = factor(age_group, levels(age_group)[c(1,2,7,3:6,8,9)]))
    })
    dataTrans <- reactive({
      read.csv("data/corona_trans.csv")
    })
    dataStats <- reactive({
      read.csv("data/corona_stats.csv") %>%
      mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date))
        
    })
    
    ## info boxes
    output$HospBox <- renderValueBox({
      valueBox(paste0(dataStats()$ncase[dataStats()$type=="Total number hospitalised"], "(",
                      dataStats()$Percentage[dataStats()$type=="Total number hospitalised"], ")"), "Hospitalised",
               color = "blue"
      )
    })
    output$ICUBox <- renderValueBox({
      valueBox(paste0(dataStats()$ncase[dataStats()$type=="Total number admitted to ICU"], "(",
                      dataStats()$Percentage[dataStats()$type=="Total number admitted to ICU"], ")"), "in Intensive Care",
               color = "purple"
      )
    })
    output$CFRBox <- renderValueBox({
      valueBox(paste0(dataStats()$ncase[dataStats()$type=="Case fatality rate"], "(",
                      dataStats()$Percentage[dataStats()$type=="Case fatality rate"], ")"), "Case fatality rate",
               color = "red"
      )
    })
    
    ## Plot weekly data
    
    ### Map
    output$map <-  renderLeaflet({
      
      labs <- lapply(seq(nrow(dataCounty())), function(i) {
        paste0( '<p>', dataCounty()[i, "ncase"],' cases </p><p>') 
      })
      
        dataCounty()  %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date)) %>%
        mutate(long = case_when(county == "Carlow" ~ -6.9261098,
                                county == "Cavan" ~ -7.3605599,
                                county == "Clare" ~ -8.9811,
                                county == "Donegal" ~ -8.1041,
                                county == "Kildare" ~ -6.9144402,
                                county == "Kilkenny" ~ -7.2448,
                                county == "Laois" ~ -7.3323,
                                county == "Leitrim" ~ -8.0020,
                                county == "Longford" ~ -7.7933,
                                county == "Louth" ~ -6.4889,
                                county == "Mayo" ~ -9.4289,
                                county == "Meath" ~ -6.6564,
                                county == "Monaghan" ~ -6.9683,
                                county == "Offaly" ~ -7.7122,
                                county == "Roscommon" ~ -8.1891,
                                county == "Sligo" ~ -8.4761,
                                county == "Tipperary" ~ -8.1619,
                                county == "Wexford" ~ -6.4633,
                                county == "Kerry" ~ -9.5669,
                                county == "Waterford" ~ -7.1101,
                                county == "Westmeath" ~ -7.4653,
                                county == "Wicklow" ~ -6.0446,
                                county == "Galway" ~ -9.0568,
                                county == "Limerick" ~ -8.6267,
                                county == "Cork" ~ -8.4756,
                                county == "Dublin" ~ -6.2603),
               lat = case_when(county == "Carlow" ~ 52.8408318,
                               county == "Cavan" ~ 53.9908295,
                               county == "Clare" ~ 52.9045,
                               county == "Donegal" ~ 54.6549,
                               county == "Kildare" ~ 53.1561089,
                               county == "Kilkenny" ~ 52.6541,
                               county == "Laois" ~ 52.9943,
                               county == "Leitrim" ~ 54.1247,
                               county == "Longford" ~ 53.7276,
                               county == "Louth" ~ 53.9252,
                               county == "Mayo" ~ 54.0153,
                               county == "Meath" ~ 53.6055,
                               county == "Monaghan" ~ 54.2492,
                               county == "Offaly" ~ 53.2357,
                               county == "Roscommon" ~ 53.6276,
                               county == "Sligo" ~ 54.2766,
                               county == "Tipperary" ~ 52.4738,
                               county == "Wexford" ~ 52.3369,
                               county == "Kerry" ~ 52.1545,
                               county == "Waterford" ~ 52.2593,
                               county == "Westmeath" ~ 53.5345,
                               county == "Wicklow" ~ 52.9808,
                               county == "Galway" ~ 53.2707,
                               county == "Limerick" ~ 52.6638,
                               county == "Cork" ~ 51.8985,
                               county == "Dublin" ~ 53.3498)) %>%
        mutate(ncases = as.character(ncase),
               ncases = ifelse(ncases == "< = 5","5",ncases)) %>%
        mutate(ncases = as.numeric(ncases)) %>%
          leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          setView(lng = -8, lat = 53.5, zoom = 8) %>%
          clearShapes() %>%
          addCircleMarkers(lng= ~long, 
                           lat= ~lat, 
                           layerId = ~county,
                           radius = ~ncases/2,
                           label = lapply(labs, htmltools::HTML))
      })
    
    ## age plot
    output$ageplot <- renderPlotly({
      g = dataAge() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date)) %>%
        ggplot(aes(x=age_group,y=ncase,fill=age_group)) + 
        geom_col() + labs(y="Cases",x="Age group") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(g, tooltip = c("age_group","ncase"))
    })
    
    ## age plot
    output$transplot <- renderPlotly({
      g = dataTrans() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date)) %>%
        ggplot(aes(x=transmission,y=ncase,fill=transmission)) + 
        geom_col() + labs(y="Cases",x="Transmission type") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(g, tooltip = c("transmission","ncase"))
    })
    
    
    ## Table weekly data
    ### county
    output$weekarea <- renderDT({

      dataCounty() %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))
      
    })
    
    ### age
    output$weekage <- renderDT({
      
      dataAge() %>% mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(Percentage = round(100*ncase/sum(ncase),0))
      
    })
    
    ### transmission
    output$weektrans <- renderDT({
      
      dataTrans() %>% mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>% mutate(Percentage = round(100*ncase/sum(ncase),0))
      
    })
}

# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)

