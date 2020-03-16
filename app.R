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
sidebar <- dashboardSidebar(disable=FALSE,width='150px',
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
                                        menuItem("Time", icon = icon("line-chart"), tabName = "time"),
                                        menuItem("Compare", icon = icon("globe"), tabName = "compare"),
                                        menuItem("Map", icon = icon("map"), tabName = "map"),
                                        menuItem("Data", icon = icon("eye"), tabName = "data")
                            )
                            
                            
                            
)

# Body
body <- dashboardBody(
  
    # tabs
    tabItems(
  
        # Map Tab #
        tabItem("map",    
                fluidRow(
                  box(title = "Map of cases on the island of Ireland using Belfast, Cork, Dublin, Letterkenny and Galway as locations", 
                      footer = "Note: no locations for 47 cases announced March 12/13th",
                      width = 12,
                      leafletOutput("map", width = "70%", height = 600)
                    )
                  )

        ),
        
        # Time series Tab #
        tabItem("time",  
                fluidRow(h5("Data from Ireland (Dept. of Health) and NI (NHS) combined")),
                fluidRow(
                  box(width = 12, 
                      infoBoxOutput("CasesBox"),
                      infoBoxOutput("MortBox"))
                  ),     
                
                fluidRow(
                    box(width = 12, title = "Cumulative and new cases on island of Ireland per day", 
                        plotlyOutput("cumulcases", width = "80%", height = 400)
                    )
                )
        ),
        
        # Country comparison Tab #
        tabItem("compare", 
                
                fluidRow(
                  column(width=6,
                         selectInput("place", "Country to compare", 
                                     choices = c("france", "germany", "italy", "spain", "uk"), 
                                     selected = "italy")),
                  column(width=6,
                         sliderInput("days","Decide the time difference",min = -5,max=30,value=13)
                  )
                ),
                fluidRow(
                  box(width = 12, 
                      title = "Compare (island of) Ireland cases with other countries scaled by population, with time shifted", 
                      plotlyOutput("irelandcompare", width = "90%", height = 500),
                      h5("Data from Dept. of Health (Ireland), NHS (NI) and WHO")
                  )
                )
        ),
        
        # Data view Tab #
        tabItem("data",
                fluidRow(box(width = 12, title = "Data from Department of Health (Ireland), NHS (NI) and WHO",
                             column(10, DTOutput("dattable"))
                )
              )
        )
    ) # end of tabItems
    
)# end of body



# Define server logic required to draw a leaflet
server <- function(input, output) {
    
    # read in all data
    dataRaw <- reactive({
      read.csv("data/corona_island.csv") %>% 
        mutate(pop = case_when(country=="ireland"~6.712,
                               country=="france"~66.99,
                               country=="germany"~82.79,
                               country=="italy"~60.48,
                               country=="spain"~46.66,
                               country=="uk"~66.44))
      
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
    
    # Create the map
    output$map <- renderLeaflet({
        dataArea() %>%
            leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = -8, lat = 53.5, zoom = 7) %>%
            clearShapes() %>%
            addCircleMarkers(lng= ~long, 
                             lat= ~lat, 
                             layerId = ~area,
                             radius = ~ncases)
    })
    
    # Show a popup at the given location
    showAreaPopup <- function(t, lat, long) {
        selectedArea <- dataArea()[dataArea()$area == t,]
        content <- as.character(tagList(
            tags$h4("Number of cases:", as.integer(selectedArea$ncases)),
            tags$h4("Number of deaths:", as.integer(selectedArea$ndeaths))
        ))
        leafletProxy("map") %>% 
            addPopups(long, lat, content, layerId = t)
    }
    
    # When map is clicked, show a popup with city info
    observe({
        leafletProxy("map") %>% 
            clearPopups()
        event <- input$map_marker_click
        if (is.null(event))
            return()
        
        isolate({
            showAreaPopup(event$id, event$lat, event$lng)
        })
    })
    
    ################### Info boxes ##########
    ## Info box
    output$CasesBox <- renderInfoBox({
      dat <- dataIreland()
      infoBox(
        "Number of cases", paste0(sum(dat$ncase,na.rm=TRUE)), icon = icon("list"),
        color = "purple"
      )
    })
    
    ## mort box
    output$MortBox <- renderInfoBox({
      dat <- dataIreland()
      infoBox(
        "Number of deaths", paste0(sum(dat$ndeath,na.rm=TRUE)), icon = icon("list"),
        color = "red"
      )
    })
    
    
    ## mort box
    output$RecovBox <- renderInfoBox({
      dat <- dataIreland()
      infoBox(
        "Recovered", paste0(sum(dat$nrecov,na.rm=TRUE)), icon = icon("list"),
        color = "green"
      )
    })
    
    
    
    #################### Data Table  ###################
    output$dattable <- renderDT({
      
      dataRaw() %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))
    
      })
    
    ################### Time series plots ##############
    #### Plotly cases per day
    output$cumulcases <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ccases,label=Date,label1=Total_cases,label2=New_cases)) + 
        geom_line() + geom_point() + geom_col(aes(x=date,y=ncases)) +
        theme(legend.position="none") + labs(y="Cases")
      ggplotly(g, tooltip = c("Date","Total_cases","New_cases"))
      
    })
    
    # #### Plotly cases per day
    # output$timeseries <- renderPlotly({
    # 
    #         g = dataIreland() %>%
    #           mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
    #           group_by(date) %>%
    #           summarise(ncases = sum(ncase)) %>%
    #           na.omit() %>%
    #           ggplot(aes(x=date,y=ncases)) + 
    #           geom_line() + geom_point() + theme(legend.position="none") + labs(y="New cases")
    #     ggplotly(g)
    #     
    # })
    

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
        geom_line() + geom_point() + labs(y="Cases") + theme_bw() + 
        ggtitle(paste0("Mean daily difference is ",round(mean(comp$diff,na.rm=TRUE),0), " cases"))
      ggplotly(g, tooltip = c("Country","Date","Total_cases","New_cases"))
      
    })    
    
}

# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)

