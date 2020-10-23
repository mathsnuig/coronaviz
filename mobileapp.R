library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(readr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)
library(plotly)
library(DT)
library(rvest)
library(httr)
library(readxl)
library(zoo)
library(tidyr)
library(wesanderson)

# Data dates
daily_date = "22/10/2020"
lag_date = "20/10/2020"
maxdays = 300

# use round away from zero form of rounding (sometimes called banker's rounding)
# what many of us learnt in school!
# check out the "round" package to find out more than you ever wanted to know about the complexities of rounding
round2 <- function(x, n = 0) (trunc((abs(x) * 10 ^ n) + 0.5) / 10 ^ n) * sign(x)

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
                                                    selected = "Republic of Ireland"),
                                        menuItem("Time", icon = icon("line-chart"), tabName = "time")
                            )
                            
                            
                            
)

# Body
body <- dashboardBody(
  
    # title
    titlePanel("Covid-19 Ireland"),
  
    # tabs
    tabItems(
        
        # Time series Tab #
        tabItem("time", 
                h3(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
                                   "HSE Coronavirus information.", 
                                   target="_blank")),
                h3(paste0("Data (",daily_date,") from Ireland's "),
                   tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                          "HSE Health Protection Surveillance Centre", target="_blank")),
                valueBoxOutput("CasesBox"),
                valueBoxOutput("FourteenBox"),
                valueBoxOutput("SevenBox"),
                fluidRow(),
                valueBoxOutput("MortBox"),
                valueBoxOutput("HospBox"),
                valueBoxOutput("ICUBox"),
                fluidRow(
                  box(
                    title = "Recent new cases and seven day rolling average", 
                    width = 12,
                    plotlyOutput("recentplot", width = "90%", height = 400),
                    sliderInput("recent", "Choose how many days to display", value = 30, min = 7, max = maxdays)
                  )),
                h3(paste0("County comparison data (midnight ",lag_date,") from Ireland's "),
                   tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                          "HSE Health Protection Surveillance Centre", target="_blank")),
                
                dateRangeInput("countydates", "Date range for county comparisons", start = as.Date(lag_date, format = "%d/%m/%Y")-14, end = as.Date(lag_date, format = "%d/%m/%Y")),
                
                fluidRow(box(title = "Map of cases by county (ROI only)", width = 12,
                             leafletOutput("map", width = "90%", height = 600)
                )),
                fluidRow(box(title = "County incidence per 100,000 league table (ROI only)", width = 12,
                             DTOutput("countytable", width = "90%")
                )),
                fluidRow(box(title = "County incidence per 100,000 over time (ROI only)", width = 12,
                             plotlyOutput("newcounty", width = "90%")
                )),
                
                h3(paste0("Country comparison data (midnight ",daily_date,") from "),
                   tags$a(href="https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases",
                          "ECDC", target="_blank")),
                
                fluidRow(
                          column(width=6,
                                 uiOutput("country_choice")),
                          column(width=6,
                                 checkboxInput("log","Compare on log scale",value = FALSE)
                          )
                        ),
                fluidRow(
                          tabBox(
                          title = "Compare cases",
                          width = 12,
                          selected = "Cases per million",
                          tabPanel("Cases per million", plotOutput("casecompare", width = "90%", height = 500))
                          ,
                          tabPanel("Animation (slow but it'll get there)", imageOutput("caseanim", width = "90%", height = 500))
                          )
                        ),
                fluidRow(
                          tabBox(
                            title = "Compare deaths",
                            width = 12,
                            selected = "Deaths per million",
                          tabPanel("Deaths per million", plotOutput("deathcompare", width = "90%", height = 500))
                          )
                        ),
                
                
                
                h3(paste0("Developed by Dr. Andrew Simpkin "),
                   tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1", target="_blank"),
                   paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
                   tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer", target="_blank"))
        ) # end of tab
    ) # end of tabItems
)# end of body



# Define server logic required to draw a leaflet
server <- function(input, output) {
    
    # read in all data
    dataRaw <- reactive({
      dat1 <- read.csv("data/corona_ireland.csv") %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(pop = case_when(country=="ireland"~6804000)) 

      dat2 <- read.csv("data/corona_ireland.csv") %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(pop = case_when(country=="ireland"~4922000)) %>%
          filter(area!="north") 

      if(input$partition == "Island of Ireland") return(dat1)
      if(input$partition == "Republic of Ireland") return(dat2)
      
    })
    
    # irish data only
    dataIreland <- reactive({
      dataRaw() %>% filter(country == "ireland")
    })
    
    # icu data
    dataICU <- reactive({
      read.csv("data/corona_icu.csv") %>%
        mutate(datetime = as.POSIXct(paste(date, time), format="%d/%m/%Y %H:%M")) %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y"))
    })

    # Country comparison data
    dataCountry <- reactive({
      # Website and filename for European Centre for Disease Prevention and Control Covid-19 data
      todaysdate = format(as.Date(Sys.time()-(16*60*60)), "%Y-%m-%d") # Updates ECDC around 16:00 each day
      ecdpcurl = paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", todaysdate, ".xlsx", sep = "")
      ecdpcfn = rev(strsplit(ecdpcurl, "/")[[1]])[1]
      
      # Download the dataset from the website to a local temporary file
      GET(ecdpcurl, authenticate(":", ":", type = "ntlm"), write_disk(ecdpcfn, overwrite = TRUE))
      
      # Read dataset
      ecdpcdata = read_excel(ecdpcfn)
      
      # Date object
      # Have checked that Day/Month/Year is same as DateRep
      ecdpcdata$dateRep = as.Date(ecdpcdata$dateRep, format = "%d/%m/%Y")
      
      # Factors and replace "_" with " "
      ecdpcdata$countriesAndTerritories = gsub("_",  " ", ecdpcdata$countriesAndTerritories)
      ecdpcdata$countriesAndTerritories = factor(ecdpcdata$countriesAndTerritories)
      
      ecdpcdata$geoId = factor(ecdpcdata$geoId)
      ecdpcdata$countryterritoryCode = factor(ecdpcdata$countryterritoryCode)
      
      
      # Cumulative count (dates are reverse order in dataset)
      ecdpcdata$TotalCases = rev(cumsum(rev(ecdpcdata$cases)))
      ecdpcdata$TotalDeaths = rev(cumsum(rev(ecdpcdata$deaths)))
      
      
      # Firstly group by date, region and gender and put in date order
      ecdpcdata = ecdpcdata %>%
        mutate(country = tolower(countriesAndTerritories), 
               gender = "unknown", area = "unknown", pop = popData2019) %>%
        filter(country != "ireland") %>%
        select(date = dateRep, ncase = cases, ndeath = deaths, gender, area, country, pop)
      
      # Make into data.frame and start from date of first case/death
      ecdpcdata = data.frame(ecdpcdata)
      ecdpcdata = ecdpcdata[1:max(which((ecdpcdata$ncase + ecdpcdata$ndeath) > 0)),]
      
      ecdpcdata <- ecdpcdata %>% filter(!(ncase == 0 & ndeath == 0))
      
      dataRaw() %>% 
        select(-c(type,death)) %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        rbind(ecdpcdata)
    })
    
    
    # create dataset with growth per day for use in growth rate calc and doubling time
    dataGrowth <- reactive({
      dataRaw() %>%
        filter(country=="ireland") %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(New_cases = sum(ncase), New_deaths = sum(ndeath)) %>%
        na.omit() %>%
        mutate(Total_cases= cumsum(New_cases), Total_deaths = cumsum(New_deaths)) %>%
        mutate(Growth = round2(((Total_cases/c(NA,Total_cases[1:(length(Total_cases)-1)]))-1)*100,1),
               Growth_death = round2(((Total_deaths/c(NA,Total_deaths[1:(length(Total_deaths)-1)]))-1)*100,1)) %>%
        mutate(median = median(Growth,na.rm=TRUE)) %>%
        filter(Total_cases > 100)
    })

    ## detailed data read in
    dataCounty <- reactive({
      read.csv("data/corona_county.csv") %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y"),
               ncases = as.character(ncase),
               ncases = ifelse(ncases == "< = 5","5",ncases),
               ncases = as.numeric(ncases),
               pop = case_when(county == "Carlow" ~ 56932,
                               county == "Cavan" ~ 76176,
                               county == "Clare" ~ 118817,
                               county == "Donegal" ~159192,
                               county == "Kildare" ~ 222504,
                               county == "Kilkenny" ~ 99232,
                               county == "Laois" ~ 84697,
                               county == "Leitrim" ~32044,
                               county == "Longford" ~ 40873,
                               county == "Louth" ~ 128884,
                               county == "Mayo" ~ 130507,
                               county == "Meath" ~ 195044,
                               county == "Monaghan" ~61386,
                               county == "Offaly" ~ 77961,
                               county == "Roscommon" ~ 64544,
                               county == "Sligo" ~ 65535,
                               county == "Tipperary" ~ 159553,
                               county == "Wexford" ~ 149722,
                               county == "Kerry" ~ 147707,
                               county == "Waterford" ~ 116176,
                               county == "Westmeath" ~ 88770,
                               county == "Wicklow" ~ 142425,
                               county == "Galway" ~ 258058,
                               county == "Limerick" ~ 194899,
                               county == "Cork" ~ 542868,
                               county == "Dublin" ~ 1347359),
               long = case_when(county == "Carlow" ~ -6.9261098,
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
                               county == "Dublin" ~ 53.3498),
               province = case_when(county=="Carlow"|county=="Dublin"|
                                      county=="Kildare"|county=="Kilkenny"|
                                      county=="Laois"|county=="Longford"|
                                      county=="Louth"|county=="Meath"|
                                      county=="Offaly"|county=="Westmeath"|
                                      county=="Wexford"|county=="Wicklow" ~ "Leinster",
                                    county=="Clare"|county=="Cork"|
                                      county=="Kerry"|county=="Limerick"|
                                      county=="Tipperary"|county=="Waterford" ~ "Munster",
                                    county=="Galway"|county=="Leitrim"|
                                      county=="Mayo"|county=="Roscommon"|
                                      county=="Sligo" ~ "Connacht",
                                    county=="Donegal"|county=="Cavan"|
                                      county=="Monaghan" ~ "Ulster (ROI)"),
               Total_cases_per100k = 1e5*ncases/pop)
    })
    dataStats <- reactive({
      read.csv("data/corona_stats.csv") %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y"))
    })
    
    ## country selector
    output$country_choice <- renderUI({
      selectInput("place", "Country to compare (best to choose all before animation)", 
                  choices = unique(dataCountry()$country), 
                  selected = c("ireland","united kingdom", "united states of america"),
                  multiple = TRUE)
    })

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
    
    ## 14 day incidence box
    output$FourteenBox <- renderValueBox({
      dat <- dataIreland() %>%
        filter(date > as.Date(daily_date,format="%d/%m/%Y")-14)
      valueBox(paste0(round(1e5*sum(dat$ncase,na.rm=TRUE)/dat$pop[1],1)), "cases per 100,000 over the past two weeks",
               color = "red"
      )
    })     
    
    ## hosp box
    output$HospBox <- renderValueBox({
      dat <- dataICU() %>% filter(date == max(date))
      valueBox(
        paste0(dat$ncase[dat$type=="hospitalised"]), "patients in hospital",
        color = "purple"
      )
    })
 
    ## 7 day incidence box
    output$SevenBox <- renderValueBox({
      dat <- dataIreland() %>%
        filter(date > as.Date(daily_date,format="%d/%m/%Y")-7)
      valueBox(paste0(round(1e5*sum(dat$ncase,na.rm=TRUE)/dat$pop[1],1)), "cases per 100,000 over the past week",
               color = "purple"
      )
    })  
    
    ## icu box
    output$ICUBox <- renderValueBox({
      dat <- dataICU() %>% filter(date == max(date))
      valueBox(
        paste0(dat$ncase[dat$type=="icu_confirmed"]), "patients in intensive care",
        color = "red"
      )
    })
 

    

    ################### Time series plots ##############
    output$recentplot <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        filter(date > as.Date(daily_date,format="%d/%m/%Y")-input$recent) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases),
               roll = rollmean(New_cases, k=7, na.pad = TRUE, align = "center"))  %>%
        ggplot(aes(x=Date,y=New_cases,label=Date,label1=Total_cases)) + 
        geom_point() + geom_line(aes(y=roll)) +
        theme(legend.position="none") + theme_bw() + labs(y="Daily New Cases")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date", "New_cases"))
      
    })


    
    
    #################### County data tab ############

    ### Table
    output$countytable <- renderDT({
      counties <- dataCounty() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date >= as.Date(input$countydates[1],format = "%d/%m/%Y")) %>%
        filter(date <= as.Date(input$countydates[2],format = "%d/%m/%Y")) %>%
        group_by(county) %>% 
        mutate(New_cases = c(NA,diff(ncases)),
               New_cases = ifelse(New_cases < 0, 0, New_cases),
               Cases = max(ncases)-min(ncases)) %>%
        mutate(Incidence = round(1e5*Cases/pop,0),
               Pop = pop,
               Days = as.Date(input$countydates[2],format = "%d/%m/%Y") - 
                 as.Date(input$countydates[1],format = "%d/%m/%Y")) %>%
        filter(date == as.Date(input$countydates[2],format = "%d/%m/%Y")) %>%
        select(date, county, Incidence, Days) %>%
        arrange(desc(Incidence))
      datatable(counties, escape = FALSE, options = list(pageLength = 26))
    })
    
    ### Map
    output$map <-  renderLeaflet({
      counties <- dataCounty() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date >= as.Date(input$countydates[1],format = "%d/%m/%Y")) %>%
        filter(date <= as.Date(input$countydates[2],format = "%d/%m/%Y")) %>%
        group_by(county) %>% 
        mutate(New_cases = c(NA,diff(ncases)),
               New_cases = ifelse(New_cases < 0, 0, New_cases),
               Period_cases = max(ncases)-min(ncases)) %>%
        mutate(Period_cases_per100k = round(1e5*Period_cases/pop,0),
               Period_length = as.Date(input$countydates[2],format = "%d/%m/%Y") - 
                 as.Date(input$countydates[1],format = "%d/%m/%Y"))
               
               
      labs <- lapply(seq(nrow(counties)), function(i) {
        paste0( '<p>', counties[i, "Period_cases_per100k"],' cases/100k during this time in ', 
                dataCounty()[i, "county"], '</p><p>') 
      })

      cutoff <- quantile(counties$Period_cases_per100k, probs = c(0.25,0.5,0.85))             
      
       counties <- counties  %>%
         mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
         mutate(case_groups = case_when(Period_cases_per100k < cutoff[1] ~ "1",
                                        Period_cases_per100k >=  cutoff[1] & 
                                          Period_cases_per100k < cutoff[2] ~ "2",
                                        Period_cases_per100k >= cutoff[2] & 
                                          Period_cases_per100k < cutoff[3] ~ "3",
                                        Period_cases_per100k >= cutoff[3] ~ "4"))
       
       pal <- colorFactor('YlOrRd', counties$case_groups)
       
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
                           radius = ~6*log(Period_cases_per100k+1),
                           color = ~pal(case_groups),
                           label = lapply(labs, htmltools::HTML),
                           fillOpacity = 0.9)
      })
    
    output$newcounty <- renderPlotly({
      ndays = as.Date(input$countydates[2],format = "%d/%m/%Y") - 
              as.Date(input$countydates[1],format = "%d/%m/%Y")
      g = dataCounty() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        group_by(county) %>% 
        mutate(New_cases = c(rep(NA,ndays), diff(ncases,lag=ndays)),
               New_cases = ifelse(New_cases < 0, 0, New_cases)) %>%
        mutate(Incidence_per100k = round(1e5*New_cases/pop,0)) %>%
        ggplot(aes(x=date, y=Incidence_per100k, color = county, label1 = New_cases)) + 
        geom_line() + theme_bw() + 
        labs(y="Incidence per 100,000", title = paste0(ndays," day incidence per 100,000"))+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "14 days")
      ggplotly(g, tooltip = c("date","county","Incidence_per100k","New_cases"))
      
    })
  
    
    
    
    
    ################### Country comparison tabs ############
    
    ## compare countries on the per 1m of pop scale
    output$casecompare <- renderPlot({
      
      dataCountry() %>% 
        filter(country %in% input$place) %>% 
        mutate(pop = pop/1e6) %>%
        group_by(country, date) %>%
        summarise(Actual_new_cases = sum(ncase), Date = min(date), Country = country[1], Pop = mean(pop)) %>%
        na.omit() %>%
        mutate(New_cases_per_million = round(Actual_new_cases / Pop, 0)) %>%
        filter(New_cases_per_million > 0.1) %>%
        mutate(Days = as.numeric(Date - min(Date)),
               roll = rollmean(New_cases_per_million, k=7, na.pad = TRUE, align = "right")) %>%
        ggplot(aes(x = Days, y = New_cases_per_million, color = country, fill = country, label = Date,
                   label1 = Country, label2 = New_cases_per_million , label3 = Actual_new_cases)) +
        geom_line(aes(y=roll)) + 
        geom_point() + 
        geom_text(data = . %>% filter(Days == max(Days)) %>% mutate(Days = Days + 2),
                  aes(y=roll,label = country), hjust = 0, size = 4) +
        labs(title = "7-day average (line) and cases (points)", 
             x = "Days since 0.1 daily cases (per million) first recorded", 
             y = ifelse(input$log == TRUE, "Log cases (per million of population)", "Cases (per million of population)")) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") +
        scale_y_continuous(trans = ifelse(input$log == TRUE, "log10", "identity")) +
        lims(x=c(0,maxdays))
      
    })
    
    ## gganimate cases
    output$caseanim <- renderImage({
      # as per https://shiny.rstudio.com/articles/progress.html#a-more-complex-progress-example
      # but set max value to pre-determined total frame count
      progress <- shiny::Progress$new(max = 100)
      progress$set(message = "Rendering", value = 0)
      on.exit(progress$close())
      
      updateShinyProgress <- function(detail) {    
        progress$inc(1, detail = detail)
      }
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile1 <- tempfile(fileext='.gif')
      
      # now make the animation
      p = dataCountry() %>% 
        filter(country %in% input$place) %>% 
        mutate(pop = pop/1e6) %>%
        group_by(country, date) %>%
        summarise(Actual_new_cases = sum(ncase), Date = min(date), Country = country[1], Pop = mean(pop)) %>%
        na.omit() %>%
        mutate(New_cases_per_million = round(Actual_new_cases / Pop, 0)) %>%
        filter(New_cases_per_million > 0.1) %>%
        mutate(Days = as.numeric(Date - min(Date)),
               roll = rollmean(New_cases_per_million, k=7, na.pad = TRUE, align = "right")) %>%
        ggplot(aes(x = Days, y = New_cases_per_million, color = country, fill = country)) +
        geom_line(aes(y=roll)) + 
        geom_text(data = . %>% mutate(Days = Days + 2),
                  aes(y=roll,label = country), hjust = 0, size = 4) +
        labs(title = "7-day rolling average", 
             x = "Days since 0.1 daily cases (per million) first recorded", 
             y = ifelse(input$log == TRUE, "Log cases (per million of population)", "Cases (per million of population)")) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") + 
        transition_reveal(Days) +
        scale_y_continuous(trans = ifelse(input$log == TRUE, "log10", "identity")) +
        lims(x=c(0,maxdays))
      
      anim_save("outfile1.gif", animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile1.gif",
           contentType = 'image/gif'
      )}, deleteFile = TRUE)
    
    
    ## compare countries on the per 1m of pop scale
    output$deathcompare <- renderPlot({
      
      dataCountry() %>% 
        filter(country %in% input$place) %>% 
        mutate(pop = pop/1e6) %>%
        group_by(country, date) %>%
        summarise(Actual_new_deaths = sum(ndeath), Date = min(date), Country = country[1], Pop = mean(pop)) %>%
        na.omit() %>%
        mutate(New_deaths_per_million = round(Actual_new_deaths / Pop, 0)) %>%
        filter(New_deaths_per_million > 0.1) %>%
        mutate(Days = as.numeric(Date - min(Date)),
               roll = round(rollmean(New_deaths_per_million, k=7, na.pad = TRUE, align = "right"),0)) %>%
        ggplot(aes(x = Days, y = New_deaths_per_million, color = country, fill = country)) +
        geom_line(aes(y=roll)) + 
        geom_point() + 
        geom_text(data = . %>% filter(Days == max(Days)) %>% mutate(Days = Days + 2),
                  aes(y=roll,label = country), hjust = 0, size = 4) +
        labs(title = "7-day average (line) and deaths (points)", 
             x = "Days since 0.1 daily deaths (per million) first recorded", 
             y = ifelse(input$log == TRUE, "Log deaths (per million of population)", "Deaths (per million of population)")) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") +
        scale_y_continuous(trans = ifelse(input$log == TRUE, "log10", "identity")) +
        lims(x=c(0,maxdays))
      
    })    

}

# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)

