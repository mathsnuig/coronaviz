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
library(prettyunits)
library(dplyr)
library(plotly)
library(DT)
library(googleVis)
library(rvest)
library(httr)
library(readxl)
library(zoo)
library(tidyr)
library(RCurl)
library(wesanderson)
library(lubridate)



# Dates used in selections
newdata = getURL("https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.geojson") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("features", "properties") %>%
  mutate(date = as.Date(Date),
         date2 = as.Date(StatisticsProfileDate))
# latest date
daily_date = as.Date(max(newdata$date))
# date for detailed
lag_date = as.Date(max(newdata$date2))
# x-axis length to give space for country/county name in plots
maxdays = as.numeric(as.Date(max(newdata$date)) - as.Date(min(newdata$date)))+24

# use round away from zero form of rounding (sometimes called banker's rounding)
round2 <- function(x, n = 0) (trunc((abs(x) * 10 ^ n) + 0.5) / 10 ^ n) * sign(x)

# Any JS functions
jsCode = 'shinyjs.clear_warning = function(){document.getElementById("note_save_confirm").innerHTML = "";}'

# Loading Spinner Settings
options(spinner.color="#ffd700", spinner.type = 2, spinner.color.background = 'white', 
        digits = 3, scipen = 99)


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
                            
                            ########
                            sidebarMenu(id = 'tabs', 
                                        selectInput("partition", "Area", 
                                                    choices = c("Republic of Ireland"),
                                                    selected = "Republic of Ireland"),
                                        menuItem("Home", icon = icon("line-chart"), tabName = "time"),
                                        menuItem("Ireland", icon = icon("list"), tabName = "nphet"),
                                        menuItem("Worldwide", icon = icon("globe"), tabName = "compare")
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
                                   target="_blank"),
                   paste0(" If you are on your phone, "),
                   tags$a(href="https://andsim.shinyapps.io/coronamobile", 
                          "try the mobile version here!", 
                          target="_blank") ),
                h3(paste0("Data (",daily_date,") from Ireland ("),
                   tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                          "HSE Health Protection Surveillance Centre", target="_blank"),
                   paste0("), Northern Ireland ("),
                   tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                          "NHS", target="_blank"),
                   paste0(") and "),
                   tags$a(href="https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases",
                          "ECDC", target="_blank")),
                h3(paste0("Please read this "), 
                          tags$a(href="https://www.statslife.org.uk/features/4474-a-statistician-s-guide-to-coronavirus-numbers", 
                                 "advice ", target="_blank"),
                   paste0("before interpreting the data")),
                valueBoxOutput("CasesBox"),
                valueBoxOutput("FourteenBox"),
                valueBoxOutput("VaccineBox"),
                fluidRow(),
                valueBoxOutput("MortBox"),
                valueBoxOutput("HospBox"),
                valueBoxOutput("ICUBox"),
                fluidRow(
                  tabBox(
                    title = "Vaccination roll out", 
                    width = 12,
                    selected = "Daily",
                    tabPanel("Daily",plotlyOutput("dailyvaxplot", width = "90%", height = 400)),
                    tabPanel("Total",plotlyOutput("vaxplot", width = "90%", height = 400))
                    
                  )),
                fluidRow(
                  box(
                    title = "Recent cases and seven day rolling average", 
                    width = 12,
                    plotlyOutput("recentplot", width = "90%", height = 400),
                    sliderInput("recent", "Choose how many days to display", value = 30, min = 7, max = maxdays)
                  )),
                fluidRow(
                  tabBox(
                  title = "Cases",
                  width = 12, 
                  selected = "New Cases",
                  tabPanel("New Cases", plotlyOutput("newcases", width = "90%", height = 400)),
                  tabPanel("Total Cases", plotlyOutput("cumulcases", width = "90%", height = 400)),
                  tabPanel("Log Cases", plotlyOutput("cumullog", width = "90%", height = 400))
                  )),
                fluidRow(
                  tabBox(
                    title = "Deaths",
                    width = 12, 
                    selected = "New Deaths",
                  tabPanel("New Deaths", plotlyOutput("newdeaths", width = "90%", height = 400)),
                  tabPanel("Total Deaths", plotlyOutput("cumuldeaths", width = "90%", height = 400)),
                  tabPanel("Log Deaths", plotlyOutput("cumullogdeaths", width = "90%", height = 400))
                  )),
                fluidRow(
                  tabBox(
                    title = "Further data",
                    width = 12, 
                    selected = "Patients",
                  tabPanel("Patients",
                           h4(paste0("Data taken from "),
                              tags$a(href="https://www.hse.ie/eng/services/news/newsfeatures/covid19-updates/",
                          "HSE daily updates", target="_blank")),
                           plotlyOutput("dailyicu", width = "90%", height = 400),
                           sliderInput("recent2", "Choose how many days to display", value = 30, min = 1, max = maxdays)),
                  tabPanel("Daily data", DTOutput("dattable"))
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
                   tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                          "HSE Health Protection Surveillance Centre", target="_blank"),
                   paste0("), Northern Ireland ("),
                   tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                          "NHS", target="_blank"),
                   paste0(") and "),
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
                   tags$a(href="https://twitter.com/AndrewSimpkin1", "@AndrewSimpkin1"),
                   paste0(", Prof. Derek O'Keeffe (Galway University Hosptial)"),
                   tags$a(href="https://twitter.com/Physicianeer", "@Physicianeer"))
        ),
        
  
        # Detailed Tab
        tabItem("nphet",
                h3(paste0("Data (midnight ",lag_date,") from Ireland"),
                   tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                          "(HSE Health Protection Surveillance Centre)", target="_blank")),
               
                
                    dateRangeInput("countydates", "Date range for incidence", start = as.Date(lag_date, format = "%d/%m/%Y")-14, end = as.Date(lag_date, format = "%d/%m/%Y")),
                    selectInput("county", "Counties to compare",
                              choices = c("Carlow","Cavan","Clare","Cork","Donegal","Dublin",
                                          "Galway","Kerry","Kildare","Kilkenny","Laois","Leitrim","Limerick",
                                          "Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon",
                                          "Sligo","Tipperary","Waterford","Westmeath","Wexford","Wicklow"),
                              selected = c("Dublin","Kildare"),
                              multiple = TRUE),
                fluidRow(
                  tabBox(title = "County data",
                    height = 800,
                    width = 12, 
                    selected = "Map",
                    tabPanel("Map",
                             leafletOutput("map", width = "70%", height = 700)),
                    tabPanel("Incidence table",
                             DTOutput("countytable", width = "70%")),
                    tabPanel("Incidence over time", 
                             plotlyOutput("newcounty", width = "90%", height = 700)),
                    # tabPanel("Cases per 100,000 (by county)", 
                    #          plotlyOutput("cumulcounty", width = "90%", height = 500)),
                    tabPanel("Incidence animation",
                             h5(paste0("This can take a minute to load. Choose your required counties and 
                                       incidence dates outside this tab (e.g. with Map selected), 
                                       otherwise this will try to make a new animation every time 
                                       you select a new county!")),
                             imageOutput("countyanim", width = "90%", height = 800))
                    # ,
                    # tabPanel("Mobility", 
                    #          selectInput("mobility", "Mobility type",
                    #                       choices = c("retail_and_recreation_percent_change_from_baseline",
                    #                                    "grocery_and_pharmacy_percent_change_from_baseline",
                    #                                    "parks_percent_change_from_baseline",    
                    #                                    "transit_stations_percent_change_from_baseline",  
                    #                                    "workplaces_percent_change_from_baseline",    
                    #                                    "residential_percent_change_from_baseline"),
                    #                       selected = "retail_and_recreation_percent_change_from_baseline"),
                    #          plotlyOutput("mob_county", width = "90%", height = 700))
                    # ,
                    # tabPanel("R0", 
                    #          plotlyOutput("r0", width = "90%", height = 700))
                  )
                ),
                fluidRow(
                  tabBox(title = "Hospital and intensive care cases over time", 
                         width = 12,
                         selected = "New patients",
                         tabPanel("New patients", plotlyOutput("newpatients", width = "90%", height = 500)),
                         tabPanel("Total patients", plotlyOutput("patienttime", width = "90%", height = 500)),
                         tabPanel("Percentage of total cases", plotlyOutput("patienttimepercent", width = "90%", height = 500))
                                  
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
    
    # #https://medium.com/@kyleake/wikipedia-data-scraping-with-r-rvest-in-action-3c419db9af2d
    # url <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Northern_Ireland#Confirmed_cases_and_deaths"
    # 
    # dat_ni2020 = url %>%
    #   read_html() %>%
    #   html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
    #   html_table(fill=TRUE)
    # 
    # names(dat_ni2020)[1:3] <- c("date","ncase","ndeath")
    # dat_ni2020 <- dat_ni2020[-1,-c(4:5)]
    # 
    # dat_ni2021 = url %>%
    #   read_html() %>%
    #   html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') %>%
    #   html_table(fill=TRUE)
    # 
    # names(dat_ni2021)[1:3] <- c("date","ncase","ndeath")
    # dat_ni2021 <- dat_ni2021[-c(1,dim(dat_ni2021)[1]),-c(4:5)]
    # 
    # dat_ni <- rbind(dat_ni2020, dat_ni2021) %>%
    #   mutate(date = as.Date(date, "%d %b %Y"))
    # 
    # for(i in 1:dim(dat_ni)[1]){
    #   if(is.na(dat_ni$date[i])){
    #     dat_ni$date[i] <- dat_ni$date[i-1]+1
    #   }
    # }
    # datni <- dat_ni %>%
    #   mutate(Date = NA, ConfirmedCovidCases = as.numeric(ncase), TotalConfirmedCovidCases = cumsum(as.numeric(ncase)), 
    #          ConfirmedCovidDeaths = as.numeric(ndeath), TotalCovidDeaths = cumsum(as.numeric(ndeath)), StatisticsProfileDate = NA, 
    #          CovidCasesConfirmed = NA, HospitalisedCovidCases = NA, RequiringICUCovidCases = NA,      
    #          HealthcareWorkersCovidCases = NA, ClustersNotified = NA, HospitalisedAged5 = NA, 
    #          HospitalisedAged5to14 = NA, HospitalisedAged15to24 = NA, HospitalisedAged25to34 = NA,      
    #          HospitalisedAged35to44 = NA, HospitalisedAged45to54 = NA, HospitalisedAged55to64 = NA,      
    #          HospitalisedAged65up = NA, Male = NA, Female = NA,                      
    #          Unknown = NA, Aged1to4 = NA, Aged5to14 = NA,                   
    #          Aged15to24  = NA, Aged25to34 = NA, Aged35to44 = NA,                  
    #          Aged45to54 = NA, Aged55to64 = NA, Aged65up = NA,                    
    #          Median_Age = NA, CommunityTransmission = NA, CloseContact = NA,                
    #          TravelAbroad = NA, FID = NA, date = date, pop = NA, ncase = as.numeric(ncase), 
    #          ndeath = as.numeric(ndeath)) %>%
    #   select(Date, ConfirmedCovidCases, TotalConfirmedCovidCases, 
    #          ConfirmedCovidDeaths, TotalCovidDeaths, StatisticsProfileDate = date, 
    #          CovidCasesConfirmed , HospitalisedCovidCases , RequiringICUCovidCases ,      
    #          HealthcareWorkersCovidCases , ClustersNotified , HospitalisedAged5 , 
    #          HospitalisedAged5to14 , HospitalisedAged15to24 , HospitalisedAged25to34 ,      
    #          HospitalisedAged35to44 , HospitalisedAged45to54 , HospitalisedAged55to64 ,      
    #          HospitalisedAged65up , Male , Female ,                      
    #          Unknown , Aged1to4 , Aged5to14 ,                   
    #          Aged15to24  , Aged25to34 , Aged35to44 ,                  
    #          Aged45to54 , Aged55to64 , Aged65up ,                    
    #          Median_Age , CommunityTransmission , CloseContact ,                
    #          TravelAbroad , FID , date = date, pop , ncase, 
    #          ndeath)
    # 
    # 
    dat2 <- getURL("https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.geojson") %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("features", "properties") %>%
      mutate(date = as.Date(Date),
             pop = 4922000,
             ncase = ConfirmedCovidCases,
             ndeath = ConfirmedCovidDeaths)
    
    # dat1 <- dat2 %>%
    #   rbind(datni) %>%
    #   group_by(date) %>%
    #   summarise(ConfirmedCovidCases = sum(ConfirmedCovidCases,na.rm=TRUE),
    #             ncase = sum(ConfirmedCovidCases, na.rm=TRUE),
    #             TotalConfirmedCovidCases = sum(TotalConfirmedCovidCases, na.rm=TRUE),
    #             ConfirmedCovidDeaths = sum(ConfirmedCovidDeaths,na.rm=TRUE),
    #             ndeath = sum(ConfirmedCovidDeaths, na.rm=TRUE),
    #             TotalCovidDeaths = sum(TotalCovidDeaths, na.rm=TRUE),
    #             CovidCasesConfirmed = sum(CovidCasesConfirmed, na.rm=TRUE),
    #             HospitalisedCovidCases = sum(HospitalisedCovidCases, na.rm=TRUE),
    #             RequiringICUCovidCases = sum(RequiringICUCovidCases, na.rm=TRUE)) %>%
    #   mutate(pop = 6804000) 
    
    if(input$partition == "Island of Ireland") return(dat1)
    if(input$partition == "Republic of Ireland") return(dat2)
    
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
    
    # cumulative vax
    output$vaxplot <- renderPlotly({
      d = dataVac() %>%
        mutate(rate = c(NA,diff(people_vaccinated)/as.numeric(diff(date)))) %>%
        filter(date == max(date)) %>% 
        mutate(time = round((4922000 - people_vaccinated)/rate,0))
      
      #(0.6*3.7e6 -  - last(dataVac()$people_vaccinated))/days_left # second dose
      
      g = dataVac() %>%
        mutate(date = as.Date(date, format = "%d/%m/%Y"))  %>%
        mutate(First_dose = people_vaccinated,
               Second_dose = people_fully_vaccinated) %>%
        gather(type, count, First_dose:Second_dose, 
               factor_key=TRUE) %>%
        ggplot(aes(x=date, y = count, fill = type)) + theme_bw() +
        geom_col() + 
        scale_fill_manual(values=wes_palette("GrandBudapest1", n=2))  + 
        labs(x="Date",y = "People vaccinated")
      #title=paste0("Current rate is ", round(d$rate,0), " new people vaccinated per day"))
      #title = paste0(d$req_rate, " per day are required to meet your chosen deadline. Current rate is ", round(d$rate,0), " per day."))
      
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
        paste0(max(dataVac()$people_vaccinated), " vaccinated"),
        paste0(round(100*max(dataVac()$people_vaccinated)/4922000,2), "%", " of population vaccinated (ROI)"),
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
    
    
    output$newcases <- renderPlotly({
      
      g = dataRaw() %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))  %>%
        ggplot(aes(x=Date,y=New_cases,label=Date,label1=Total_cases)) + 
        geom_col() +
        theme(legend.position="none") + theme_bw() + labs(y="Daily New Cases")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date", "New_cases"))
      
    })
    
    
    # cumulative cases over time
    output$cumulcases <- renderPlotly({
      
      g = dataRaw() %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))  %>%
        mutate(Growth_Percentage = round2(((Total_cases/c(NA,Total_cases[1:(length(Total_cases)-1)]))-1)*100,0)) %>%
        ggplot(aes(x=Date,y=ccases,label=Date,label1=Total_cases,label2=New_cases, label3=Growth_Percentage)) + 
        geom_line() + geom_point() + geom_col(aes(y=ncases)) +
        theme(legend.position="none") + theme_bw() + labs(y="Cases")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date", "Total_cases","New_cases", "Growth_Percentage"))
      
    })
    
    # Cases per day
    output$cumullog <- renderPlotly({
      
      g = dataRaw() %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))  %>%
        mutate(Growth_Percentage = round2(((Total_cases/c(NA,Total_cases[1:(length(Total_cases)-1)]))-1)*100,0)) %>%
        ggplot(aes(x=Date,y=ccases,label=Date,label1=Total_cases,label2=New_cases, label3=Growth_Percentage)) + 
        geom_line() + geom_point() + geom_col(aes(y=New_cases)) + theme_bw() +
        theme(legend.position="none") + labs(y="Cases (axis shows log10 scale)") +
        scale_y_continuous(trans='log10')+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date","Total_cases","New_cases", "Growth_Percentage"))
      
    })
    
    #### Plotly deaths per day
    output$cumuldeaths <- renderPlotly({
      
      g = dataRaw() %>%
        group_by(date) %>%
        summarise(New_deaths = sum(ndeath), Date = min(date))%>%
        na.omit() %>%
        mutate(Total_deaths= cumsum(New_deaths)) %>%
        ggplot(aes(x=Date,y=Total_deaths,label=Date)) + 
        geom_line() + geom_point() + geom_col(aes(x=Date,y=New_deaths)) + theme_bw() +
        labs(y="Deaths")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date","death","Total_deaths","New_deaths"))
      
    })    
    
    #### Plotly deaths per day
    output$newdeaths <- renderPlotly({
      
      g = dataRaw() %>%
        group_by(date) %>%
        summarise(New = sum(ndeath), Date = min(date))%>%
        na.omit() %>%
        ggplot(aes(x=Date,y=New)) + 
        geom_col() + theme_bw() + labs(y="Daily death notifications")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date","New"))
      
    })  
    
    #### Plotly log deaths per day
    output$cumullogdeaths <- renderPlotly({
      
      g = dataRaw() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ndeaths = sum(ndeath), New_deaths = sum(ndeath), Date = min(date)) %>%
        na.omit() %>%
        mutate(cdeaths = cumsum(ndeaths), Total_deaths= cumsum(ndeaths))  %>%
        ggplot(aes(x=Date,y=cdeaths,label=Date,label1=Total_deaths,label2=New_deaths)) + 
        geom_line() + geom_point() + geom_col(aes(x=Date,y=ndeaths)) + theme_bw() +
        theme(legend.position="none") + labs(y="Deaths (axis shows log10 scale)") +
        scale_y_continuous(trans='log10')+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("Date","Total_deaths","New_deaths"))
      
    }) 
    
    ### ICU trends
    output$dailyicu <- renderPlotly({
      g = dataICU() %>% 
        select(-new) %>%
        spread(type, ncase) %>% 
        gather(type, ncase, -c(datetime,date)) %>%
        na.omit() %>%
        filter(date > max(dataICU()$date)-input$recent2) %>%
        ggplot(aes(x=date, y=ncase, color = type)) + geom_line() + 
        scale_color_manual(values=wes_palette("GrandBudapest1", n=2)) +
        labs(y="Patients") + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days", date_labels = "%Y-%m-%d")
    })
    

    
    
    ################### Comparison tabs ############
    
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
    
    
    #################### Data Tab  ###################
    output$dattable <- renderDT({
      dataRaw()
    })


    #################### Detailed data tab ############
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
    
    # county cumulative
    output$cumulcounty <- renderPlotly({
      
      g = dataCounty() %>%
        filter(county %in% input$county) %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        filter(date >= as.Date(input$countydates[1],format = "%d/%m/%Y")) %>%
        filter(date <= as.Date(input$countydates[2],format = "%d/%m/%Y")) %>%
        group_by(county) %>%
        mutate(new_cases = ncases - min(ncases)) %>%
        group_by(date,county) %>%
        summarise(Total_cases = sum(new_cases), pop = mean(pop)) %>%
        mutate(Cases_per100k = round2(1e5*Total_cases/pop,0)) %>%
        ggplot(aes(x=date,y=Cases_per100k,color=county,group=county)) + 
        geom_line() + theme_bw() + labs(y="Cases per 100,000")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("date", "county", "Cases_per100k", "Total_cases"))
      
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
    
    ## gganimate cases
    output$countyanim <- renderImage({
      # as per https://shiny.rstudio.com/articles/progress.html#a-more-complex-progress-example
      # but set max value to pre-determined total frame count
      progress <- shiny::Progress$new(min = 0, max = 1000)
      progress$set(message = "Loading animation (very slow - but worth it)", value = 0)
      on.exit(progress$close())
      
      updateShinyProgress <- function(detail) {
        progress$inc(10, detail = detail)
      }

      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile1 <- tempfile(fileext='.gif')
      
      # now make the animation
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
        geom_text(aes(y=Incidence_per100k, label = CountyName), hjust = 0, size = 4) +
        geom_line() + theme_bw() + 
        labs(y="Incidence per 100,000", title = paste0(ndays," day incidence per 100,000"))+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "14 days", limits = c(as.Date("12/03/2020",format = "%d/%m/%Y"),
                                                        as.Date(lag_date,format = "%d/%m/%Y")+15)) + 
        transition_reveal(date)
      
      anim_save("outfile1.gif", animate(g)) # New
      
      # Return a list containing the filename
      list(src = "outfile1.gif",
           contentType = 'image/gif',
           width = 700
           # height = 300,
           # alt = "This is alternate text"
      )}, deleteFile = TRUE)
    
    ## patient time raw
    output$patienttime <- renderPlotly({
      
      g = dataRaw() %>% 
        ggplot() + 
        geom_point(aes(x=date,y=HospitalisedCovidCases),color="orange") + 
        geom_line(aes(x=date,y=HospitalisedCovidCases),color="orange") + 
        geom_point(aes(x=date,y=RequiringICUCovidCases),color="red") + 
        geom_line(aes(x=date,y=RequiringICUCovidCases),color="red") + 
        theme_bw() + 
        labs(y="Cumulative Hospitalised and ICU patients")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g)
      
    })
    
    ## patient time percentage
    output$patienttimepercent <- renderPlotly({
      
      g = dataRaw()  %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(ICU_percent = round(100*RequiringICUCovidCases/CovidCasesConfirmed,1),
               Hospitalised_percent = round(100*HospitalisedCovidCases/CovidCasesConfirmed,1)) %>%
        ggplot() + 
        geom_point(aes(x=date,y=Hospitalised_percent), color="orange") + 
        geom_line(aes(x=date,y=Hospitalised_percent), color="orange") + 
        geom_point(aes(x=date,y=ICU_percent),color="red") + 
        geom_line(aes(x=date,y=ICU_percent),color="red") + theme_bw() + 
        labs(y="Hospitalised and ICU patients (% of total cases)")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g)
      
    })
    
    ## new icu, death
    output$newpatients <- renderPlotly({
      
      g = dataRaw() %>% 
        mutate(Cases = c(NA,diff(CovidCasesConfirmed)),
               Hospitalised = c(NA,diff(HospitalisedCovidCases)),
               ICU = c(NA,diff(RequiringICUCovidCases))) %>%
        gather(type, count, Cases:ICU, factor_key=TRUE) %>% 
        filter(type!="Cases") %>%
        ggplot(aes(x=date, y=count, color = type)) + geom_line() + 
        scale_color_manual(values=wes_palette("GrandBudapest1", n=2)) +
        geom_point() + labs(y="New daily counts") + theme_bw()+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g)
      
    })
    
    ## Table county data
    output$weekarea <- renderDT({
      dataCounty() %>% arrange(desc(date))
    })
  
}

# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)