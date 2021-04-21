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
library(RCurl)
library(lubridate)

# Dates used in selections
newdata = getURL("https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.geojson") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("features", "properties") %>%
  mutate(date = as.Date(Date),
         date2 = as.Date(StatisticsProfileDate))
# latest date
daily_date = as.Date(max(newdata$date))
# date for detailed statistics
lag_date = as.Date(max(newdata$date2))
# x-axis length to give space for country/county name in plots
maxdays = as.numeric(as.Date(max(newdata$date)) - as.Date(min(newdata$date)))+36

# use round away from zero form of rounding (sometimes called banker's rounding)
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
                h3(paste0("Data (", daily_date, ") from Ireland's "),
                   tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                          "HSE Health Protection Surveillance Centre", target="_blank")),
                valueBoxOutput("CasesBox"),
                valueBoxOutput("FourteenBox"),
                #valueBoxOutput("SevenBox"),
                valueBoxOutput("VaccineBox"),
                fluidRow(),
                valueBoxOutput("MortBox"),
                valueBoxOutput("HospBox"),
                valueBoxOutput("ICUBox"),
                fluidRow(
                  box(
                    title = "Vaccination roll-out", 
                    width = 12,
                    plotlyOutput("dailyvaxplot", width = "90%", height = 400)
                  )),
                fluidRow(
                  box(
                    title = "Recent cases and seven day rolling average", 
                    width = 12,
                    plotlyOutput("recentplot", width = "90%", height = 400),
                    sliderInput("recent", "Choose how many days to display", value = 30, min = 7, max = maxdays)
                  )),
                fluidRow(
                  box(
                    title = "Recent deaths and seven day rolling average", 
                    width = 12,
                    plotlyOutput("recentdeaths", width = "90%", height = 400),
                    sliderInput("recent2", "Choose how many days to display", value = 30, min = 7, max = maxdays)
                  )),
                fluidRow(
                  box(
                    title = "Recent hospitalisations and ICU", 
                    width = 12,
                    plotlyOutput("dailyicu", width = "90%", height = 400),
                    sliderInput("recent3", "Choose how many days to display", value = 30, min = 7, max = maxdays)
                  )),
                h3(paste0("County comparison data from Ireland's "),
                   tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                          "HSE Health Protection Surveillance Centre", target="_blank")),
                
                dateRangeInput("countydates", "Date range for county comparisons", 
                               start = as.Date(lag_date)-14, 
                               end = as.Date(lag_date)),
                
                fluidRow(box(title = "Map of cases by county (ROI only)", width = 12,
                             leafletOutput("map", width = "90%", height = 600)
                )),
                fluidRow(box(title = "County incidence per 100,000 league table (ROI only)", width = 12,
                             DTOutput("countytable", width = "90%")
                )),
                selectInput("county", "Counties to compare",
                            choices = c("Carlow","Cavan","Clare","Cork","Donegal","Dublin",
                                        "Galway","Kerry","Kildare","Kilkenny","Laois","Leitrim","Limerick",
                                        "Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon",
                                        "Sligo","Tipperary","Waterford","Westmeath","Wexford","Wicklow"),
                            selected = c("Dublin","Kildare"),
                            multiple = TRUE),
                fluidRow(box(title = "County incidence per 100,000 over time (ROI only)", width = 12,
                             plotlyOutput("newcounty", width = "90%")
                )),
                
                h3(paste0("Country comparison data from "),
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
                          title = "Compare countries",
                          width = 12,
                          selected = "Cases per million",
                          tabPanel("Cases per million", plotOutput("casecompare", width = "90%", height = 500)),
                          tabPanel("Deaths per million", plotOutput("deathcompare", width = "90%", height = 500)),
                          tabPanel("Vaccinations per hundred", plotOutput("vaxcompare", width = "90%", height = 500))
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
    getURL("https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.geojson") %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("features", "properties") %>%
      mutate(date = as.Date(Date),
             pop = 4922000,
             ncase = ConfirmedCovidCases,
             ndeath = ConfirmedCovidDeaths)
    
  })
    
  # icu data
  dataICU <- reactive({
    
    icu_data <- httr::GET("https://services-eu1.arcgis.com/z6bHNio59iTqqSUY/arcgis/rest/services/ICUBISHistoricTimelinePublicView/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json") %>%
      httr::content() %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("features", "attributes") %>%
      mutate(datetime = as_datetime(extract/1000),
             date = as.Date(datetime),
             type="icu") %>%
      rename(ncase = ncovidconf) %>%
      mutate(new = c(NA,diff(ncase))) %>%
      select(date,datetime,ncase,type,new)
    
    httr::GET("https://services-eu1.arcgis.com/z6bHNio59iTqqSUY/arcgis/rest/services/Covid19AcuteHospitalHistoricSummaryOpenData/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json") %>%
      httr::content() %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("features", "attributes")%>%
      mutate(datetime = as_datetime(Date/1000),
             date = as.Date(datetime),
             type="hospitalised") %>%
      rename(ncase = SUM_number_of_confirmed_covid_1) %>%
      mutate(new = c(NA,diff(ncase))) %>%
      select(date,datetime,ncase,type,new) %>%
      rbind(icu_data)
  })

    # Country comparison data
    dataCountry <- reactive({
      
      read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
               col_types = list(col_character(),col_character(),col_character(),
                                col_date(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_character(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_double(), col_double(),
                                col_double(), col_character())) %>%
        mutate(date = as.Date(date),
               country = as.factor(tolower(location)))
      
    })
    
    ## country selector
    output$country_choice <- renderUI({
      selectInput("place", "Country to compare (best to choose all before animation)", 
                  choices = unique(dataCountry()$country), 
                  selected = c("ireland","united kingdom", "united states"),
                  multiple = TRUE)
    })  
    

    ## detailed data read in
    dataCounty <- reactive({
      read_csv("http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv") %>%
        mutate(date = as.Date(TimeStamp),
               ncases = ConfirmedCovidCases,
               pop = PopulationCensus16,
               province = case_when(CountyName=="Carlow"|CountyName=="Dublin"|
                                      CountyName=="Kildare"|CountyName=="Kilkenny"|
                                      CountyName=="Laois"|CountyName=="Longford"|
                                      CountyName=="Louth"|CountyName=="Meath"|
                                      CountyName=="Offaly"|CountyName=="Westmeath"|
                                      CountyName=="Wexford"|CountyName=="Wicklow" ~ "Leinster",
                                    CountyName=="Clare"|CountyName=="Cork"|
                                      CountyName=="Kerry"|CountyName=="Limerick"|
                                      CountyName=="Tipperary"|CountyName=="Waterford" ~ "Munster",
                                    CountyName=="Galway"|CountyName=="Leitrim"|
                                      CountyName=="Mayo"|CountyName=="Roscommon"|
                                      CountyName=="Sligo" ~ "Connacht",
                                    CountyName=="Donegal"|CountyName=="Cavan"|
                                      CountyName=="Monaghan" ~ "Ulster (ROI)"),
               Total_cases_per100k = 1e5*ncases/pop)
    })
    
    ## vaccinations (owid missing march 3rd, added manually)
    dataVac <- reactive({
      dataCountry() %>% 
        filter(country == "ireland" & date > "2021-01-01") %>%
        select(date, total_vaccinations, people_fully_vaccinated, people_vaccinated) %>%
        mutate(people_vaccinated = ifelse(is.na(people_vaccinated),total_vaccinations,people_vaccinated)) %>%
        filter(!is.na(total_vaccinations)) %>%
        add_row(date = as.Date("2021-03-03"), total_vaccinations = 474645,
                people_fully_vaccinated = 146047, people_vaccinated = 328598) %>%
        add_row(date = as.Date("2021-04-04"), total_vaccinations = 936087,
                people_fully_vaccinated = 272676, people_vaccinated = 663411) %>%
        arrange(date)
    })
    
    output$vaxplot <- renderPlotly({
      d = dataVac() %>%
        mutate(rate = c(NA,diff(people_vaccinated)/as.numeric(diff(date)))) %>%
        filter(date == max(date)) %>% 
        mutate(time = round((4922000 - people_vaccinated)/rate,0))
      #,
      #        req_rate = round((4922000 - total_vaccinations)/as.numeric(input$vaxdate-Sys.Date()),0))
      
      g = dataVac() %>%
        mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
        ggplot(aes(x=date)) + theme_bw() +
        geom_line(aes(y=people_vaccinated), color="darkblue") + 
        geom_point(aes(y=people_vaccinated), color="darkblue") + 
        geom_line(aes(y=people_fully_vaccinated), color = "darkgreen") + 
        geom_point(aes(y=people_fully_vaccinated), color = "darkgreen") + 
        labs(x="Date",y="People vaccinated",
             title=paste0("Current rate is ", round(d$rate,0), " per day"))
      #title = paste0(d$req_rate, " per day are required to meet your chosen deadline. Current rate is ", round(d$rate,0), " per day."))
      
      # g = dataVac() %>%
      #   mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
      #   ggplot(aes(date, total_vaccinations)) + geom_line() + geom_point() +
      #   labs(x="Date",y="Vaccinations") +
      #   theme_bw()
      
      ggplotly(g)
    })
    
    # daily vax
    output$dailyvaxplot <- renderPlotly({
      d = dataVac() %>%
        mutate(Weekly = c(rep(NA,7),diff(total_vaccinations, lag = 7)),
               Weekly_first = c(rep(NA,7),diff(people_vaccinated, lag = 7)),
               Weekly_second = c(rep(NA,7),diff(people_fully_vaccinated, lag = 7)),
               First_dose = c(NA,diff(people_vaccinated)),
               Second_dose = c(NA,diff(people_fully_vaccinated))) %>%
        filter(date > "2021-02-06") %>%
        gather(type, count, First_dose:Second_dose, factor_key=TRUE) 
      
      
      weeks_left = as.numeric(as.Date("2021-06-30") - last(dataVac()$date))/7 # weeks left to end of June
      weekly_doses = round((3e6 - last(dataVac()$people_vaccinated))/weeks_left,0) # weekly vax to hit first dose target
      
      g = d %>% 
        ggplot(aes(x=date, y = count, fill = type)) + 
        geom_col() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust = 1)) +
        labs(x = "Date", y = "Daily total vaccinations",
             title = paste0(last(d$Weekly_first), " first doses in the last 7 days",
                            '<br>',
                            weekly_doses, " weekly first doses needed for June target",
                            '<br>',
                            "     ")) + 
        scale_fill_manual(values=wes_palette("GrandBudapest1", n=2))

      
      ggplotly(g)
    })

    ################### Info boxes ##########
    ## Info box
    output$CasesBox <- renderValueBox({
      dat <- dataRaw()
      valueBox(paste0(max(dat$TotalConfirmedCovidCases,na.rm=TRUE), "  cases"),
               paste0(dat$ConfirmedCovidCases[length(dat$ConfirmedCovidCases)], " new cases"),
               color = "purple"
      )
    })
    
    ## mort box
    output$MortBox <- renderValueBox({
      dat <- dataRaw()
      valueBox(
        paste0(max(dat$TotalCovidDeaths,na.rm=TRUE), " deaths"),
        paste0(dat$ndeath[length(dat$ndeath)], " new death(s)"),
        color = "red"
      )
    })
    
    ## 14 day incidence box
    output$FourteenBox <- renderValueBox({
      dat <- dataRaw() %>%
        filter(date > max(dataRaw()$date)-14)
      valueBox(paste0(round(1e5*sum(dat$ncase,na.rm=TRUE)/dat$pop[1],1)), "cases per 100,000 over the past two weeks",
               color = "red"
      )
    })     
    
    ## hosp box
    output$HospBox <- renderValueBox({
      dat <- dataICU() %>% filter(date == max(date))
      valueBox(
        paste0(dat$ncase[dat$type=="hospitalised"], " in hospital (ROI)"),
        paste0(ifelse(dat$new[dat$type=="hospitalised"]>=0,"+",""), dat$new[dat$type=="hospitalised"], " since last update"),
        color = "purple"
      )
    })
 
    ## 7 day incidence box
    output$SevenBox <- renderValueBox({
      dat <- dataRaw() %>%
        filter(date > max(dataRaw()$date)-7)
      valueBox(paste0(round(1e5*sum(dat$ncase,na.rm=TRUE)/dat$pop[1],1)), "cases per 100,000 over the past week",
               color = "purple"
      )
    })  
    
    ## icu box
    output$ICUBox <- renderValueBox({
      dat <- dataICU() %>% filter(date == max(date))
      if(length(dat$ncase[dat$type=="icu"])==0){
        dat <- dataICU() %>% filter(date == max(date)-1)
      }
      valueBox(
        paste0(dat$ncase[dat$type=="icu"], " in ICU (ROI)"),
        paste0(ifelse(dat$new[dat$type=="icu"]>=0,"+",""), dat$new[dat$type=="icu"], " since last update"),
        color = "red"
      )
    })
 
    ## vaccine total box
    output$VaccineBox <- renderValueBox({
      valueBox(
        paste0(max(dataVac()$people_vaccinated), " vaccinations"),
        paste0(round(100*max(dataVac()$people_vaccinated)/4922000,2), "%", " of population (ROI)"),
        color = "blue"
      )
    }) 
    

    ################### Time series plots ##############
    output$recentplot <- renderPlotly({
      
      g = dataRaw() %>%
        filter(date > max(dataRaw()$date)-input$recent) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases),
               roll = rollmean(New_cases, k=7, na.pad = TRUE, align = "center"))  %>%
        ggplot(aes(x=Date,y=New_cases,label=Date,label1=Total_cases)) + 
        geom_point() + geom_line(aes(y=roll)) +
        theme(legend.position="none") + theme_bw() + labs(y="Daily New Cases")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date", "New_cases"))
      
    })
    
    # recent deaths
    output$recentdeaths <- renderPlotly({
      
      g = dataRaw() %>%
        filter(date > max(dataRaw()$date)-input$recent2) %>%
        group_by(date) %>%
        summarise(ndeaths = sum(ndeath), New_deaths = sum(ndeath), Date = min(date)) %>%
        na.omit() %>%
        mutate(cdeaths = cumsum(ndeaths), Total_deaths= cumsum(ndeaths),
               roll = rollmean(New_deaths, k=7, na.pad = TRUE, align = "center"))  %>%
        ggplot(aes(x=Date,y=New_deaths,label=Date,label1=Total_deaths)) + 
        geom_point() + geom_line(aes(y=roll)) +
        theme(legend.position="none") + theme_bw() + labs(y="Daily New Deaths")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date", "New_deaths"))
      
    })

    ### ICU trends
    output$dailyicu <- renderPlotly({
      g = dataICU() %>% 
        select(-new) %>%
        spread(type, ncase) %>% 
        gather(type, ncase, -c(datetime,date)) %>%
        na.omit() %>%
        filter(date > max(dataICU()$date)-input$recent3) %>%
        ggplot(aes(x=date, y=ncase, color = type)) + geom_line() + 
        scale_color_manual(values=wes_palette("GrandBudapest1", n=2)) +
        labs(y="Patients") + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days", date_labels = "%Y-%m-%d")
    })
    
    
    #################### County data tab ############

    ### Table
    output$countytable <- renderDT({
      counties <- dataCounty() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date >= as.Date(input$countydates[1],format = "%d/%m/%Y")) %>%
        filter(date <= as.Date(input$countydates[2],format = "%d/%m/%Y")) %>%
        group_by(CountyName) %>% 
        mutate(New_cases = c(NA,diff(ncases)),
               New_cases = ifelse(New_cases < 0, 0, New_cases),
               Cases = max(ncases)-min(ncases)) %>%
        mutate(Incidence = round(1e5*Cases/pop,0),
               Pop = pop,
               Days = as.Date(input$countydates[2],format = "%d/%m/%Y") - 
                 as.Date(input$countydates[1],format = "%d/%m/%Y")) %>%
        filter(date == as.Date(input$countydates[2],format = "%d/%m/%Y")) %>%
        select(date, CountyName, Incidence, Days) %>%
        arrange(desc(Incidence))
      datatable(counties, escape = FALSE, options = list(pageLength = 26))
    })
    
    ### Map
    output$map <-  renderLeaflet({
      counties <- dataCounty() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date >= as.Date(input$countydates[1],format = "%d/%m/%Y")) %>%
        filter(date <= as.Date(input$countydates[2],format = "%d/%m/%Y")) %>%
        group_by(CountyName) %>% 
        mutate(New_cases = c(NA,diff(ncases)),
               New_cases = ifelse(New_cases < 0, 0, New_cases),
               Period_cases = max(ncases)-min(ncases)) %>%
        mutate(Period_cases_per100k = round(1e5*Period_cases/pop,0),
               Period_length = as.Date(input$countydates[2],format = "%d/%m/%Y") - 
                 as.Date(input$countydates[1],format = "%d/%m/%Y"))
               
               
      labs <- lapply(seq(nrow(counties)), function(i) {
        paste0( '<p>', counties[i, "Period_cases_per100k"],' cases/100k during this time in ', 
                dataCounty()[i, "CountyName"], '</p><p>') 
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
            # urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            # attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          setView(lng = -8, lat = 53.5, zoom = 7) %>%
          clearShapes() %>%
          addCircleMarkers(lng= ~Long, 
                           lat= ~Lat, 
                           layerId = ~CountyName,
                           radius = ~5*log(Period_cases_per100k+1),
                           color = ~pal(case_groups),
                           label = lapply(labs, htmltools::HTML),
                           fillOpacity = 0.9)
      })
    
    output$newcounty <- renderPlotly({
      ndays = as.Date(input$countydates[2],format = "%d/%m/%Y") - 
              as.Date(input$countydates[1],format = "%d/%m/%Y")
      g = dataCounty() %>%
        filter(CountyName %in% input$county) %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        group_by(CountyName) %>% 
        mutate(New_cases = c(rep(NA,ndays), diff(ncases,lag=ndays)),
               New_cases = ifelse(New_cases < 0, 0, New_cases)) %>%
        mutate(Incidence_per100k = round(1e5*New_cases/pop,0)) %>%
        ggplot(aes(x=date, y=Incidence_per100k, color = CountyName, label1 = New_cases)) + 
        geom_line() + theme_bw() + 
        labs(y="Incidence per 100,000", title = paste0(ndays," day incidence per 100,000"))+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "14 days")
      ggplotly(g, tooltip = c("date","CountyName","Incidence_per100k","New_cases"))
      
    })
  
    

    
    
    ################### Country comparison tabs ############
    
    ## compare countries on the per 1m of pop scale
    output$casecompare <- renderPlot({
      
      dataCountry() %>% 
        filter(country %in% input$place) %>% 
        ggplot(aes(x = date, y = new_cases_smoothed_per_million, color = country, fill = country, label = date,
                   label1 = country, label2 = new_cases_per_million , label3 = new_cases)) +
        geom_line() + 
        geom_text(data = . %>% filter(date == max(date)) %>% mutate(date = date + 2),
                  aes(y=new_cases_smoothed_per_million,label = country), hjust = 0, size = 4) +
        labs(title = "Daily cases", 
             y = ifelse(input$log == TRUE, "Log cases (per million of population)", "Cases (per million of population)")) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") +
        scale_y_continuous(trans = ifelse(input$log == TRUE, "log10", "identity")) +
        scale_x_date(limits=c(as.Date("2020-03-01"),Sys.Date()+30))
      
      
    })
    
    ## compare countries on the per 1m of pop scale
    output$deathcompare <- renderPlot({
      
      dataCountry() %>% 
        filter(country %in% input$place) %>% 
        ggplot(aes(x = date, y = new_deaths_smoothed_per_million, color = country, fill = country, label = date,
                   label1 = country, label2 = new_deaths_per_million , label3 = new_cases)) +
        geom_line() + 
        geom_text(data = . %>% filter(date == max(date)) %>% mutate(date = date + 2),
                  aes(y=new_deaths_smoothed_per_million,label = country), hjust = 0, size = 4) +
        labs(title = "Daily deaths", 
             y = ifelse(input$log == TRUE, "Log deaths (per million of population)", "Deaths (per million of population)")) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") +
        scale_y_continuous(trans = ifelse(input$log == TRUE, "log10", "identity")) +
        scale_x_date(limits=c(as.Date("2020-03-01"),Sys.Date()+30))
      
    })      
    
    ## compare countries vaccines on the per 100 scale
    output$vaxcompare <- renderPlot({
      
      dataCountry() %>% 
        filter(country %in% input$place) %>% 
        select(date, country, total_vaccinations, total_vaccinations_per_hundred) %>%
        na.omit() %>% 
        ggplot(aes(x = date, y = total_vaccinations_per_hundred, color = country, 
                   label = total_vaccinations)) +
        geom_line() + 
        geom_text(data = . %>% 
                    group_by(country) %>% 
                    summarise(date = max(date), 
                              total_vaccinations_per_hundred = max(total_vaccinations_per_hundred)) %>%
                    mutate(date = date + 1),
                  aes(y=total_vaccinations_per_hundred,label = country), hjust = 0, size = 4) +
        labs(title = "Total vaccinations per hundred", 
             y = "Vaccinations (per hundred people)") + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") +
        scale_x_date(limits=c(as.Date("2020-12-20"),Sys.Date()+10))
      
    })   

}

# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)

