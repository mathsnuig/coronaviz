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
sidebar <- dashboardSidebar(disable=FALSE,
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
                            
                            tags$a(href='https://www.nuigalway.ie/science/school-of-maths/',
                                           tags$img(src='logo.jpg',height='50',width='150')),
                            

                            sidebarMenu(id = 'tabs', 
                                        menuItem("Map", icon = icon("map"), tabName = "map"),
                                        menuItem("Time series", icon = icon("line-chart"), tabName = "time"),
                                        menuItem("Data store", icon = icon("eye"), tabName = "data")
                            )
                            
                            
                            
)

# Body
body <- dashboardBody(
    
  
    # Motion Tab #
    tabItems(
  
        # Map Tab #
        tabItem("map", 
                fluidRow(
                  box(width = 12, 
                      infoBoxOutput("CasesBox"),
                      infoBoxOutput("MortBox"),
                      infoBoxOutput("RecovBox")
                  ),                
                fluidRow(
                  box(title = "Map using Mater, Galway and Cork Hospitals as locations", 
                      width = 12,
                      leafletOutput("map", width = "70%", height = 750)
                    )
                  )

                    
                )
        ),
        
        # Time series Tab #
        tabItem("time", 
                fluidRow(
                    box(width = 12, title = "Cumulative cases", 
                        plotlyOutput("cumulcases", width = "70%", height = 500)
                    )
                ),
                fluidRow(
                  box(width = 12, title = "Number of new cases per day, by gender", 
                      plotlyOutput("timeseries", width = "70%", height = 500)
                  )
                )
        ),
        
        # Data view Tab #
        tabItem("data",
                fluidRow(box(width = 12, title = "Data table",
                             column(10, DTOutput("dattable"))
                )
                )
        )
    ) # end of tabItems
    
)# end of body



# Define server logic required to draw a leaflet
server <- function(input, output) {
    
    # read in and clean data
    dataRaw <- reactive({
      read.csv("data/corona_island.csv")
    })
    
    # read in and clean data
    dataArea <- reactive({        
    dataRaw() %>% 
            mutate(long = case_when(area=="east" ~ -6.2690,
                                    area=="west" ~ -9.0650,
                                    area=="south" ~ -8.5110,
                                    area=="north" ~ -5.954),
                   lat = case_when(area=="east" ~ 53.3593,
                                   area=="west" ~ 53.2760,
                                   area=="south" ~ 51.8830,
                                   area=="north" ~ 54.588))  %>% 
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
            setView(lng = -8, lat = 53, zoom = 7) %>%
            clearShapes() %>%
            addCircleMarkers(lng= ~long, 
                             lat= ~lat, 
                             layerId = ~area,
                             radius = ~ncases*3)
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
    
    
    ## Info box
    output$CasesBox <- renderInfoBox({
      dat <- dataRaw()
      infoBox(
        "Number of cases", paste0(sum(dat$ncase,na.rm=TRUE)), icon = icon("list"),
        color = "purple"
      )
    })
    
    ## mort box
    output$MortBox <- renderInfoBox({
      dat <- dataRaw()
      infoBox(
        "Number of deaths", paste0(sum(dat$ndeath,na.rm=TRUE)), icon = icon("list"),
        color = "red"
      )
    })
    
    
    ## mort box
    output$RecovBox <- renderInfoBox({
      dat <- dataRaw()
      infoBox(
        "Recovered", paste0(sum(dat$nrecov,na.rm=TRUE)), icon = icon("list"),
        color = "green"
      )
    })
    
    #### Data Table
    output$dattable <- renderDT({
      
      dataRaw() %>% select(-age)
    
      })
    
    
    #### Plotly cases per day
    output$cumulcases <- renderPlotly({
      
      g = dataRaw() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase)) %>%
        na.omit() %>%
        mutate(ncases = cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ncases)) + 
        geom_line() + geom_point() + theme(legend.position="none") + labs(y="Total cases")
      ggplotly(g)
      
    })
    
    #### Plotly cases per day
    output$timeseries <- renderPlotly({
  
            g = dataRaw() %>%
              mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
              mutate_at(vars(gender), funs(as.character(.))) %>%
              bind_rows(mutate(., gender = "All")) %>%
              group_by(date,gender) %>%
              summarise(ncases = sum(ncase)) %>%
              na.omit() %>%
              ggplot(aes(x=date,y=ncases,color=gender)) + 
              geom_line() + geom_point() + theme(legend.position="none") + labs(y="New cases")
        ggplotly(g)
        
    })
    
    
}

# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)

