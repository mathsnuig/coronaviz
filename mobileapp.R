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
      # # Map tab
      # f7Tab(
      #   tabName = "map",
      #   icon = f7Icon("map"),
      #   active = TRUE,
      #   # Tab 1 content
      #   h4(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
      #             "HSE Coronavirus information.", 
      #             target="_blank"),
      #      "Data from Ireland (",
      #      tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/",
      #             "Department of Health", target="_blank"),
      #      paste0("), Northern Ireland ("),
      #      tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
      #             "NHS", target="_blank"),
      #      paste0(") and WHO")),
      #   infoBoxOutput("CasesBox"),
      #   infoBoxOutput("MortBox"),
      #   h4("Map of cases using Belfast, Cork, Dublin, Letterkenny and Galway as locations
      #      Note: no locations for 121 cases announced March 12/13/18th"),
      #   leafletOutput("map", width = "90%", height = 600),
      #   h4(paste0("Developed by Dr. Andrew Simpkin"),
      #             tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1"),
      #      paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
      #             tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer"))
      #   
      # ),
      # Weekly tab
      f7Tab(
        tabName = "map",
        icon = f7Icon("calendar"),
        active = FALSE,
        # Tab 5 content
        h3(paste0("Report data (midnight 19/03/20) from Republic of Ireland only"),
           tags$a(href="https://www.gov.ie/en/publication/cd0cea-an-analysis-of-the-266-cases-of-covid-19-in-ireland-as-of-march-16-2/", 
                  "(National Public Health Emergency Team)", target="_blank")),
        valueBoxOutput("HospBox"),
        valueBoxOutput("ICUBox"),
        valueBoxOutput("CFRBox"),
        leafletOutput("map", width = "90%", height = 600),
        h4(tags$caption("County data")),
        DTOutput("weekarea", width = "80%"),
        plotlyOutput("ageplot", width = "70%"),
        h4(tags$caption("Cumulative cases by age group")),
        plotlyOutput("cumulage", width = "70%", height = 600),
        h4(tags$caption("Age data")),
        DTOutput("weekage", width = "80%"),
        plotlyOutput("transplot", width = "70%"),
        h4(tags$caption("Cumulative cases by transmission type")),
        plotlyOutput("cumultrans", width = "70%", height = 600),
        h4(tags$caption("Transmission data")),
        DTOutput("weektrans", width = "80%")
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
           paste0(". Data (21/03/2020) from Ireland ("),
           tags$a(href="https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/", 
                  "Department of Health", target="_blank"),
           paste0("), Northern Ireland ("),
           tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                  "NHS", target="_blank"),
           paste0(") and WHO")),
        infoBoxOutput("CasesBox"),
        infoBoxOutput("MortBox"),
        fluidRow(),
        valueBoxOutput("GrowthBox"),
        #valueBoxOutput("DoublingBox"),
        h4("New and cumulative cases per day"),
        plotlyOutput("cumulcases", width = "90%", height = 400),
        h4("Cumulative cases on the log10 scale"),
        plotlyOutput("cumullog", width = "90%", height = 400)
        # ,
        # plotlyOutput("cumularea", width = "90%", height = 400),
        # plotlyOutput("cumulgender", width = "90%", height = 400)
        
      ),
      # Compare tab
      f7Tab(
        tabName = "compare",
        icon = f7Icon("layers"),
        active = FALSE,
        # Tab 3 content
        h4("Compare (island of) Ireland trajectory with other countries (scaled by population). 
           Number of cases from other countries are scaled to reflect the Irish population. 
           For example, ROI+NI (6.712 million people) is about 11% of Italy's population (60.48m), so 100 cases in Italy is equivalent to 11 cases in Ireland"),
        f7SmartSelect("place", "Country to compare", 
                      choices = c("belgium", "denmark", "france", "germany", "italy", 
                                  "netherlands", "norway", "portugal", "spain", "uk"), 
                      selected = "france"),
        h4("Decide how many days behind/ahead Ireland is to your selected country"),
        f7Slider("days","Decide the time difference",min = -5,max=30,value=0,scale = TRUE,step = 1),
        h4("Compare cumulative growth"),
        plotlyOutput("irelandcompare", width = "90%", height = 500),
        h4("Compare cumulative growth on the log10 scale"),
        plotlyOutput("logcompare", width = "90%", height = 500)
      ),
      # Data tab
      f7Tab(
        tabName = "data",
        icon = f7Icon("list_number"),
        active = FALSE,
        # Tab 4 content
        h4(tags$caption("Raw data")),
        DTOutput("dattable", width = "80%")
        # h4(tags$caption("Total cases by Area (Missing area for 121 cases)")),
        # DTOutput("areatable", width = "80%"),
        # h4(tags$caption("Total cases by Gender (ROI only and 47 with no gender information)")),
        # DTOutput("gendertable", width = "80%")
      )

    )
  )
)
 
# Define server logic required to draw a leaflet
server <- function(input, output) {
    
  # read in all data
  dataRaw <- reactive({
    dat1 <- read.csv("data/corona_island.csv") %>% 
      mutate(pop = case_when(country=="ireland"~6.804,
                             country=="belgium"~11.4,
                             country=="denmark"~5.786,
                             country=="france"~66.99,
                             country=="germany"~82.79,
                             country=="italy"~60.48,
                             country=="netherlands"~17.18,
                             country=="norway"~5.368,
                             country=="portugal"~10.29,
                             country=="spain"~46.66,
                             country=="uk"~66.44)) 
    
    dat2 <- read.csv("data/corona_island.csv") %>% 
      mutate(pop = case_when(country=="ireland"~4.922,
                             country=="belgium"~11.575,
                             country=="denmark"~5.786,
                             country=="france"~65.233,
                             country=="germany"~83.710,
                             country=="italy"~60.486,
                             country=="netherlands"~17.124,
                             country=="norway"~5.409,
                             country=="portugal"~10.204,
                             country=="spain"~46.749,
                             country=="uk"~67.787)) %>%
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
    
    dataGrowth <- reactive({
      dataRaw() %>%
        filter(country=="ireland") %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        mutate(Growth = round(((Total_cases/c(NA,Total_cases[1:(length(Total_cases)-1)]))-1)*100,0)) %>%
        mutate(median = median(Growth,na.rm=TRUE))
    })
    
    output$GrowthBox <- renderValueBox({
      
      dat <- dataGrowth()
      
      valueBox(paste0(dat$median[1],"%"), "Median growth rate", color = "blue")
      
    })
    
    output$DoublingBox <- renderValueBox({
      
      dat <- dataGrowth()
      
      valueBox(round(70/dat$median[1],2), "Estimated days for number of cases to double", color = "maroon")
      
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
        theme(legend.position="none") + labs(y="Cases")
      ggplotly(g, tooltip = c("Date","Total_cases","New_cases"))
      
    })
    
    #### Plotly cases per day
    output$cumullog <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))  %>%
        mutate(Growth_Percentage = round(((Total_cases/c(NA,Total_cases[1:(length(Total_cases)-1)]))-1)*100,0)) %>%
        ggplot(aes(x=date,y=ccases,label=Date,label1=Total_cases,label2=New_cases, label3=Growth_Percentage)) + 
        geom_line() + geom_point() + 
        theme(legend.position="none") + labs(y="Cases (axis shows log10 scale)") +
        scale_y_continuous(trans='log10')
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
    
    output$logcompare <- renderPlotly({
      
      
      dat <- dataCountry()
      dat <- dat %>% mutate(date = as.Date(date,format = "%d/%m/%Y"), country = as.character(country)) 
      prop = mean(dat$pop[dat$country=="ireland"])/mean(dat$pop[dat$country==input$place])
      dat$ncase[dat$country==input$place] <- round(dat$ncase[dat$country==input$place]*prop,0)
      dat$date[dat$country==input$place] <- dat$date[dat$country==input$place]+input$days
      dat$country[dat$country==input$place] <- paste(input$place, "scaled to ireland +",input$days,"days")
      
      g = dat %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))%>%
        group_by(country, date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date), Country = country[1]) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ccases,color=country,fill=country,label=Date,label1=Country,label2=Total_cases)) +
        geom_line() + geom_point() + labs(y="Cases (scaled to Ireland)") + theme_bw() + scale_y_continuous(trans = "log10")
      ggplotly(g, tooltip = c("Country","Date","Total_cases","New_cases"))
      
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
      valueBox(paste0(dataStats()$Percentage[dataStats()$type=="Case fatality rate"]), "Case fatality rate",
               color = "red"
      )
    })
    
    ## Plot weekly data
    
    ### map
    output$map <-  renderLeaflet({
      
      labs <- lapply(seq(nrow(dataCounty())), function(i) {
        paste0( '<p>', dataCounty()[i, "ncase"],' cases in ', dataCounty()[i, "county"], '</p><p>') 
      })
      
      counties <- dataCounty()  %>%
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
        mutate(ncases = as.numeric(ncases),
               logcases = log(ncases+1))
      
      pal <- colorNumeric('Reds', counties$logcases)
      
      counties %>%
        leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -8, lat = 53.5, zoom = 7) %>%
        clearShapes() %>%
        addCircleMarkers(lng= ~long, 
                         lat= ~lat, 
                         layerId = ~county,
                         radius = ~5*log(ncases),
                         color = ~pal(logcases),
                         label = lapply(labs, htmltools::HTML))
    })

    ## age plot
    output$ageplot <- renderPlotly({
      g = dataAge() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        ggplot(aes(x=age_group,y=ncase,fill=age_group)) + 
        geom_col() + labs(y="Cases",x="Age group") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(g, tooltip = c("age_group","ncase"))
    })
    
    # age spaghetti chart
    output$cumulage <- renderPlotly({
      
      g = dataAge() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,age_group) %>%
        summarise(Total_cases = sum(ncase)) %>%
        ggplot(aes(x=date,y=Total_cases,color=age_group)) + 
        geom_line() + geom_point() + labs(y="Cases")
      ggplotly(g, tooltip = c("date", "age_group", "Total_cases"))
      
    })
    
    ## transimission barplot
    output$transplot <- renderPlotly({
      g = dataTrans() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date)) %>%
        ggplot(aes(x=transmission,y=ncase,fill=transmission)) + 
        geom_col() + labs(y="Cases",x="Transmission type") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(g)
    })
    
    # trans spaghetti chart
    output$cumultrans <- renderPlotly({
      
      g = dataTrans() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,transmission) %>%
        summarise(Total_cases = sum(ncase)) %>%
        ggplot(aes(x=date,y=Total_cases,color=transmission)) + 
        geom_line() + geom_point() + labs(y="Cases")
      ggplotly(g, tooltip = c("date", "transmission", "Total_cases"))
      
    })       
    
    
    ## Table weekly data
    ### county
    output$weekarea <- renderDT({
      
      dataCounty() %>% mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% arrange(desc(date))
      
    })
    
    ### age
    output$weekage <- renderDT({
      
      dataAge() %>% mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>% mutate(Percentage = round(100*ncase/sum(ncase),0)) %>% arrange(desc(date))
      
    })
    
    ### transmission
    output$weektrans <- renderDT({
      
      dataTrans() %>% mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(Percentage = round(100*ncase/sum(ncase),0)) %>% arrange(desc(date))
      
    })
    
}

# Run the application 
shinyApp(
    ui = ui,
    server = server
)

