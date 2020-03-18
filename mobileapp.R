library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyMobile)
library(readr)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)
library(googleVis)

                            
           
ui = f7Page(
  title = "Coronavirus Ireland",
  f7TabLayout(
      navbar = f7Navbar(
        title = "COVID19 Ireland",
        f7Toggle("partition", "Include NI", checked = TRUE, color = NULL),
        # enable both panels
        left_panel = FALSE,
        right_panel = FALSE
      ),
    # f7Tabs is a special toolbar with included navigation
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      # Map tab
      f7Tab(
        tabName = "map",
        icon = f7Icon("map"),
        active = TRUE,
        # Tab 1 content
        h4(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
                  "HSE Coronavirus information.", 
                  target="_blank"),
           "Data from Ireland (",
           tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/",
                  "Department of Health", target="_blank"),
           paste0("), Northern Ireland ("),
           tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                  "NHS", target="_blank"),
           paste0(") and WHO")),
        infoBoxOutput("CasesBox"),
        infoBoxOutput("MortBox"),
        h4("Map of cases using Belfast, Cork, Dublin, Letterkenny and Galway as locations
           Note: no locations for 47 cases announced March 12/13th"),
        leafletOutput("map", width = "90%", height = 600),
        h4(paste0("Developed by Dr. Andrew Simpkin (NUI Galway)"),
                  tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1"),
           paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
                  tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer"))
        
      ),
      # Time tab
      f7Tab(
        tabName = "time",
        icon = f7Icon("graph_square"),
        active = FALSE,
        # Tab 2 content
        h4(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
                  "HSE Coronavirus information", 
                  target="_blank"),
           paste0(". Data from Ireland ("),
           tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/", 
                  "Department of Health", target="_blank"),
           paste0("), Northern Ireland ("),
           tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                  "NHS", target="_blank"),
           paste0(") and WHO")),
        plotlyOutput("cumulcases", width = "90%", height = 400),
        plotlyOutput("cumularea", width = "90%", height = 400),
        plotlyOutput("cumulgender", width = "90%", height = 400)
        
      ),
      # Compare tab
      f7Tab(
        tabName = "compare",
        icon = f7Icon("calendar"),
        active = FALSE,
        # Tab 3 content
        h4("Compare (island of) Ireland trajectory with other countries (scaled by population). 
           Number of cases from other countries are scaled to reflect the Irish population. 
           For example, ROI+NI (6.712 million people) is about 11% of Italy's population (60.48m), so 100 cases in Italy is equivalent to 11 cases in Ireland"),
        f7SmartSelect("place", "Country to compare", 
                      choices = c("france", "germany", "italy", "spain", "uk"), 
                      selected = "italy"),
        h4("Decide how many days behind/ahead Ireland is to your selected country"),
        f7Slider("days","Decide the time difference",min = -5,max=30,value=13,scale = TRUE,step = 1),
        plotlyOutput("irelandcompare", width = "90%", height = 500)
      ),
      # Data tab
      f7Tab(
        tabName = "data",
        icon = f7Icon("list_number"),
        active = FALSE,
        # Tab 4 content
        h4(tags$caption("Raw data")),
        DTOutput("dattable", width = "80%"),
        h4(tags$caption("Total cases by Area (Missing area for 47 cases)")),
        DTOutput("areatable", width = "80%"),
        h4(tags$caption("Total cases by Gender (ROI only and 47 with no gender information)")),
        DTOutput("gendertable", width = "80%")
      )
    )
  )
)
 
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
    
    if(input$partition == TRUE) return(dat1)
    if(input$partition == FALSE) return(dat2)
    
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
        filter(area != "unknown") %>%
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
      labs <- lapply(seq(nrow(dataArea())), function(i) {
        paste0( '<p>', dataArea()[i, "ncases"],' cases </p><p>', 
                dataArea()[i, "ndeaths"], ' deaths </p>' ) 
      })
      
        dataArea() %>%
            leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = -8, lat = 53.5, zoom = 6) %>%
            clearShapes() %>%
            addCircleMarkers(lng= ~long, 
                             lat= ~lat, 
                             layerId = ~area,
                             radius = ~ncases/2,
                             label = lapply(labs, htmltools::HTML))
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
        "Confirmed cases", 
        paste0(sum(dat$ncase,na.rm=TRUE)), 
        color = "purple",
        icon = icon("list")
      )
    })
    
    ## mort box
    output$MortBox <- renderInfoBox({
      dat <- dataIreland()
      infoBox(
        "Total deaths", 
        paste0(sum(dat$ndeath,na.rm=TRUE)), 
        color = "red",
        icon = icon("list")
      )
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
        theme(legend.position="none") + labs(y="Cases", title = "New and cumulative cases")
      ggplotly(g, tooltip = c("Date","Total_cases","New_cases"))
      
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
    
}

# Run the application 
shinyApp(
    ui = ui,
    server = server
)

