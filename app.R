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
library(wesanderson)

# Data dates
daily_date = "08/07/2020"
lag_date = "06/07/2020"
maxdays = 150

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
                                        menuItem("Time", icon = icon("line-chart"), tabName = "time"),
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
                valueBoxOutput("MortBox"),
                fluidRow(),
                valueBoxOutput("HospBox"),
                valueBoxOutput("ICUBox"),
                fluidRow(
                  tabBox(
                  title = "Cases",
                  width = 12, 
                  selected = "New Cases",
                  tabPanel("New Cases", plotlyOutput("newcases", width = "90%", height = 400)),
                  tabPanel("Total Cases", plotlyOutput("cumulcases", width = "90%", height = 400)),
                  tabPanel("Log Cases", plotlyOutput("cumullog", width = "90%", height = 400)),
                  tabPanel("Growth Rate", plotlyOutput("growth", width = "90%", height = 400))
                  )),
                fluidRow(
                  tabBox(
                    title = "Deaths",
                    width = 12, 
                    selected = "New Deaths",
                  tabPanel("New Deaths", plotlyOutput("newdeaths", width = "90%", height = 400)),
                  tabPanel("Total Deaths", plotlyOutput("cumuldeaths", width = "90%", height = 400)),
                  tabPanel("Log Deaths", plotlyOutput("cumullogdeaths", width = "90%", height = 400)),
                  tabPanel("Growth Rate", plotlyOutput("growthdeath", width = "90%", height = 400))
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
                           selectInput("confirmed", "Include suspected cases", 
                                       choices = c("Confirmed cases only","Include suspected cases"),
                                       selected = "Confirmed cases only"),
                           plotlyOutput("dailyicu", width = "90%", height = 400)),
                  tabPanel("Testing", 
                           plotlyOutput("tests", width = "90%", height = 400),
                           h4("Official testing data are presented in the table below. 
                              Positive percentage is calculated using ROI cases only, 
                              excluding those from the German lab, and assuming that 
                              testing and notification occur in the same week"),
                           DTOutput("testdata", width = "90%")),
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
                #h3("Number of cases from other countries are scaled to reflect the Irish population. For example: ROI+NI (6.712 million people) is about 11% of Italy's population (60.48m), so 100 cases in Italy is equivalent to 11 cases in Ireland"),
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
                  # ,
                  # tabPanel("Animation (slow but it'll get there)", imageOutput("deathanim", width = "90%", height = 500))
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
                fluidRow(box(title = "Map of cases by county (ROI only)", width = 12,
                             leafletOutput("map", width = "70%", height = 600)
                             )),
                fluidRow(
                  selectInput("county", "Counties to compare",
                              choices = c("Carlow","Cavan","Clare","Cork","Donegal","Dublin",
                                          "Galway","Kerry","Kildare","Kilkenny","Laois","Leitrim","Limerick",
                                          "Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon",
                                          "Sligo","Tipperary","Waterford","Westmeath","Wexford","Wicklow"),
                              selected = c("Cavan","Cork","Dublin","Galway","Limerick","Monaghan","Westmeath"),
                              multiple = TRUE), 
                  dateRangeInput("countydates", "Dates to compare", start = "2020-03-15", end = as.Date(lag_date, format = "%d/%m/%Y")),
                  tabBox(title = "Area data over time",
                    width = 12, 
                    selected = "New cases per million (by county)",
                    tabPanel("New cases per million (by county)", 
                             plotlyOutput("newcounty", width = "90%", height = 500)),
                    # tabPanel("New cases animated (slow but it'll get there)",
                    #          imageOutput("animcounty")),
                    tabPanel("Cases per million (by county)", 
                             plotlyOutput("cumulcounty", width = "90%", height = 500)),
                    tabPanel("Cases per million (by province)", plotlyOutput("cumulprovince", width = "90%", height = 500))

                  )
                ),
                fluidRow(box(width = 12, title = "County data",
                             column(10, DTOutput("weekarea"))
                )),
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
      todaysdate = format(as.Date(Sys.time()) - 1, "%Y-%m-%d") # NZ is always ahead!
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
               pop = case_when(county == "Carlow" ~ 24272,
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
                                      county=="Monaghan" ~ "Ulster (ROI)"))
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
    
    ## hosp box
    output$HospBox <- renderValueBox({
      dat <- dataICU() %>% filter(date == max(date))
      valueBox(
        paste0(dat$ncase[dat$type=="hospitalised"]), "patients in hospital",
        color = "navy"
      )
    })
    
    ## mort box
    output$ICUBox <- renderValueBox({
      dat <- dataICU() %>% filter(date == max(date))
      valueBox(
        paste0(dat$ncase[dat$type=="icu_confirmed"]), "patients in intensive care",
        color = "orange"
      )
    })


    ################### Time series plots ##############
    output$newcases <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,type) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))  %>%
        ggplot(aes(x=Date,y=New_cases,fill=type,label=Date,label1=Total_cases)) + 
        geom_col() +
        scale_fill_manual(values=wes_palette("GrandBudapest1", n=4))+
        theme(legend.position="none") + theme_bw() + labs(y="Daily New Cases")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date", "New_cases", "type"))
      
    })
    
    
    # cumulative cases over time
    output$cumulcases <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))  %>%
        mutate(Growth_Percentage = round2(((Total_cases/c(NA,Total_cases[1:(length(Total_cases)-1)]))-1)*100,0)) %>%
        ggplot(aes(x=Date,y=ccases,label=Date,label1=Total_cases,label2=New_cases, label3=Growth_Percentage)) + 
        geom_line() + geom_point() + geom_col(aes(y=ncases)) +
        theme(legend.position="none") + theme_bw() + labs(y="Cases")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date", "Total_cases","New_cases", "Growth_Percentage"))
      
    })
    
    # Cases per day
    output$cumullog <- renderPlotly({
      
      g = dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
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
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date","Total_cases","New_cases", "Growth_Percentage"))
      
    })


    
    # Growth rate (cases)
    output$growth <- renderPlotly({
      
      g = dataGrowth() %>%
        mutate(Date = as.Date(date,format = "%d/%m/%Y")) %>%
        filter(Date >= as.Date("13/03/2020",format = "%d/%m/%Y")) %>%
        ggplot(aes(x=Date,y=Growth,label1=New_cases,label2=Total_cases)) + 
        geom_point() + theme_bw() + geom_line() +
        theme(legend.position="none") + labs(y="Growth in Total Cases per day (%)")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date", "Growth", "Total_cases", "New_cases"))
      
    })
    
    # Growth rate (death)
    output$growthdeath <- renderPlotly({
      
      g = dataGrowth() %>%
        mutate(Date = as.Date(date,format = "%d/%m/%Y"),
               Growth = Growth_death) %>%
        filter(Date >= as.Date("28/03/2020",format = "%d/%m/%Y")) %>%
        ggplot(aes(x=Date,y=Growth,label1=New_deaths,label2=Total_deaths)) + 
        geom_point() + theme_bw() + geom_line() +
        theme(legend.position="none") + labs(y="Growth in Total Deaths per day (%)")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date", "Growth", "Total_deaths", "New_deaths"))
      
    })
    
    #### Plotly deaths per day
    output$cumuldeaths <- renderPlotly({
      
      g = dataIreland() %>%
        group_by(date) %>%
        summarise(New_deaths = sum(ndeath), Date = min(date))%>%
        na.omit() %>%
        mutate(Total_deaths= cumsum(New_deaths)) %>%
        ggplot(aes(x=Date,y=Total_deaths,label=Date)) + 
        geom_line() + geom_point() + geom_col(aes(x=Date,y=New_deaths)) + theme_bw() +
        labs(y="Deaths")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date","death","Total_deaths","New_deaths"))
      
    })    
    
    #### Plotly deaths per day
    output$newdeaths <- renderPlotly({
      
      g = dataIreland() %>%
        group_by(date,death) %>%
        summarise(New = sum(ndeath), Date = min(date))%>%
        na.omit() %>%
        arrange(death,.by_group=TRUE) %>%
        ggplot(aes(x=Date,y=New,fill=death)) + 
        scale_fill_manual(values=wes_palette("GrandBudapest1", n=3))+
        geom_col() + theme_bw() + labs(y="Daily death notifications")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date","death","New"))
      
    })  
    
    #### Plotly log deaths per day
    output$cumullogdeaths <- renderPlotly({
      
      g = dataIreland() %>%
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
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("Date","Total_deaths","New_deaths"))
      
    }) 
    
    ### ICU trends
    output$dailyicu <- renderPlotly({
      lims <- as.POSIXct(strptime(c("29/02/2020 00:00",paste0(daily_date, " 23:59")), format = "%d/%m/%Y %H:%M"))    
      conf <- ifelse(input$confirmed == "Confirmed cases only", "confirmed", "total")
      g = dataICU() %>% 
        spread(type, ncase) %>% 
        mutate(icu_total = icu_confirmed + icu_suspected,
               ventilated_total = ventilated_confirmed + ventilated_suspected) %>%
        gather(type, ncase, -c(datetime,date,time)) %>%
        na.omit() %>%
        filter(type == "hospitalised" | type == paste0("icu_",conf) | type == paste0("ventilated_",conf)) %>%
        ggplot(aes(x=datetime, y=ncase, color = type)) + geom_line() + 
        scale_color_manual(values=wes_palette("GrandBudapest1", n=3)) +
        labs(y="Patients") + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_datetime(date_breaks = "3 days", date_labels = "%Y-%m-%d", limits = lims)
    })
    
    ## testing
    dataTest <- reactive({
      tests <- c(397,1784,6636,
                 17992,30213,42484,
                 90646,111584,153954,
                 214761,258808,295626,
                 325795,348416, 367780,
                 386572, 404989)
      dates <- as.Date(c("02/03/2020","09/03/2020", "17/03/2020", 
                         "23/03/2020", "30/03/2020", "06/04/2020", 
                         "13/04/2020", "20/04/2020", "27/04/2020", 
                         "04/05/2020", "11/05/2020", "18/05/2020",
                         "25/05/2020", "01/06/2020", "08/06/2020",
                         "15/06/2020", "22/06/2020"),format="%d/%m/%Y")
      tests_pw <- c(NA,diff(tests))
      tests_pd <- c(80, 198, 606, 1892, 1746, 1753, 6880, 2991, 6053, 8687, 6292, 
                    5260, 4310, 3232, 2766, 2685, 2631)
      links <- c("https://bit.ly/2wpipR2", "https://bit.ly/2wpipR2", "https://bit.ly/3c83Twp", 
                 "https://bit.ly/2XmjlAE", "https://bit.ly/2UVD7l9", "https://bit.ly/34rdXOu",
                 "https://bit.ly/2XZeiXe", "https://bit.ly/34YHCyu", "https://bit.ly/3aHIk4J",
                 "https://bit.ly/3c8kpwO", "https://bit.ly/2YWB6HJ", "https://bit.ly/2AJObtX",
                 "https://bit.ly/3efuKYi", "https://bit.ly/3gWRqPo", "https://bit.ly/3d7l1CE",
                 "https://bit.ly/3egfYkn", "https://bit.ly/3etmTqC")
      refs <- paste0("<a href='",links,"' target='_blank'>",links,"</a>")
      
      # weekly tests
      dat <- read.csv("data/corona_ireland.csv") %>% 
        mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
        filter(type !="german_lab") %>%
        filter(area!="north")  %>%
        mutate(week = case_when(date > dates[1] & date <= dates[2] ~ 1,
                                date > dates[2] & date <= dates[3] ~ 2,
                                date > dates[3] & date <= dates[4] ~ 3,
                                date > dates[4] & date <= dates[5] ~ 4,
                                date > dates[5] & date <= dates[6] ~ 5,
                                date > dates[6] & date <= dates[7] ~ 6,
                                date > dates[7] & date <= dates[8] ~ 7,
                                date > dates[8] & date <= dates[9] ~ 8,
                                date > dates[9] & date <= dates[10] ~ 9,
                                date > dates[10] & date <= dates[11] ~ 10,
                                date > dates[11] & date <= dates[12] ~ 11,
                                date > dates[12] & date <= dates[13] ~ 12,
                                date > dates[13] & date <= dates[14] ~ 13,
                                date > dates[14] & date <= dates[15] ~ 14,
                                date > dates[15] & date <= dates[16] ~ 15,
                                date > dates[16] & date <= dates[17] ~ 16
                                
        )) %>%
        group_by(week) %>%
        summarise(new_cases = sum(ncase)) %>%
        na.omit() 
      
      data.frame(date = as.Date(dates, format = "%d/%m/%Y"), 
                 Total_tests = tests,
                 Source = refs, 
                 Tests_between_updates = tests_pw,
                 Cases_between_updates = c(NA, as.vector(dat$new_cases)),
                 Positive_percentage = round(100*c(NA, as.vector(dat$new_cases))/tests_pw,1),
                 Daily_tests_between_updates = tests_pd)
    })
    
    # table of data
    output$testdata <- renderDT({
      datatable(dataTest(), escape = FALSE, options = list(pageLength = 25))
    })
    
    # bar plot of positive tests over week
    output$tests <- renderPlotly({
      
      g = ggplot(dataTest(), aes(x=date,y=Positive_percentage,
                                 label1 = Cases_between_updates, 
                                 label2 = Tests_between_updates,
                                 fill=as.factor(1))) + 
        scale_fill_manual(values=wes_palette("GrandBudapest1", n=1))+
        geom_col() + theme_bw() + labs(y="Positive test (%)") + theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days") 
      ggplotly(g, tooltip = c("date", "Cases_between_updates", "Tests_between_updates", "Positive_percentage"))
      
    })
    
    ################### Comparison tabs ############
    
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
        labs(title = "Seven day rolling average (line) and actual (points) cases per million by days since 0.1 cases per million first recorded", 
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
        labs(title = "Seven day rolling average cases per million", 
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
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
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
        labs(title = "Seven day rolling average (line) and actual (points) deaths per million by days since 0.1 deaths per million first recorded", 
             x = "Days since 0.1 daily deaths (per million) first recorded", 
             y = ifelse(input$log == TRUE, "Log deaths (per million of population)", "Deaths (per million of population)")) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") +
        scale_y_continuous(trans = ifelse(input$log == TRUE, "log10", "identity")) +
        lims(x=c(0,maxdays))
      
    })
    
    ## gganimate deaths
    output$deathanim <- renderImage({
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
      outfile2 <- tempfile(fileext='.gif')
      
      # now make the animation
      p = dataCountry() %>% 
        filter(country %in% input$place) %>% 
        mutate(pop = pop/1e6) %>%
        group_by(country, date) %>%
        summarise(Actual_new_deaths = sum(ndeath), Date = min(date), Country = country[1], Pop = mean(pop)) %>%
        na.omit() %>%
        mutate(New_deaths_per_million = round(Actual_new_deaths / Pop, 0)) %>%
        filter(New_deaths_per_million > 0.1) %>%
        mutate(Days = as.numeric(Date - min(Date)),
               roll = rollmean(New_deaths_per_million, k=7, na.pad = TRUE, align = "right")) %>%
        ggplot(aes(x = Days, y = New_deaths_per_million, color = country, fill = country)) +
        geom_line(aes(y=roll)) + 
        geom_point() + 
        geom_text(data = . %>% mutate(Days = Days + 2),
                  aes(y=roll,label = country), hjust = 0, size = 4) +
        labs(title = "Seven day rolling average (line) and actual (points) deaths per million", 
             x = "Days since 0.1 daily deaths (per million) first recorded", 
             y = ifelse(input$log == TRUE, "Log deaths (per million of population)", "Deaths (per million of population)")) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle=45, hjust = 1), legend.position = "none") + 
        transition_reveal(Days) +
        scale_y_continuous(trans = ifelse(input$log == TRUE, "log10", "identity")) +
        lims(x=c(0,maxdays))
      
      anim_save("outfile2.gif", animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile2.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )}, deleteFile = TRUE)
    
    
    
    #################### Data Tab  ###################
    output$dattable <- renderDT({
      
      dataRaw() %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))
      
    })


    #################### Detailed data tab ############

    ### Map
    output$map <-  renderLeaflet({
      
      labs <- lapply(seq(nrow(dataCounty())), function(i) {
        paste0( '<p>', dataCounty()[i, "ncase"],' cases in ', dataCounty()[i, "county"], '</p><p>') 
      })
      
       counties <- dataCounty()  %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date)) %>%
        mutate(logcases = log(ncases+1),
               case_groups = case_when(ncases < 200 ~ "1",
                                       ncases >= 200 & ncases < 300 ~ "2",
                                       ncases >= 300 & ncases < 750 ~ "3",
                                       ncases >= 750 ~ "4"))
       
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
                           radius = ~4*log(ncases),
                           color = ~pal(case_groups),
                           label = lapply(labs, htmltools::HTML),
                           fillOpacity = 0.9)
      })
    
    ## province time scaled
    output$cumulprovince <- renderPlotly({
      
      g = dataCounty() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,province) %>%
        summarise(Total_cases = sum(ncases), pop = sum(pop)) %>%
        mutate(Cases_permillion = round2(1e6*Total_cases/pop,0)) %>%
        ggplot(aes(x=date,y=Cases_permillion,color=province,group=province)) + 
        geom_line() + geom_point() + theme_bw() + labs(y="Cases per million population")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("date", "province", "Cases_permillion", "Total_cases"))
      
    })    
    
    ## county time
    ## county selector
    output$county_choice <- renderUI({
      selectInput("county", "Counties to compare",
                   choices = c("Carlow","Cavan","Clare","Cork","Donegal","Dublin",
                               "Galway","Kerry","Kildare","Kilkenny","Laois","Leitrim","Limerick",
                               "Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon",
                               "Sligo","Tipperary","Waterford","Westmeath","Wexford","Wicklow"),
                   selected = "Westmeath",
                   multiple = TRUE)
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
        mutate(Cases_permillion = round2(1e6*Total_cases/pop,0)) %>%
        ggplot(aes(x=date,y=Cases_permillion,color=county,group=county)) + 
        geom_line() + geom_point() + theme_bw() + labs(y="Cases per million")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g, tooltip = c("date", "county", "Cases_permillion", "Total_cases"))
      
    })   
    
    # new county
    output$newcounty <- renderPlotly({
      g = dataCounty() %>%
        filter(county %in% input$county) %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(county) %>% 
        mutate(New_cases = c(NA,diff(ncases)),
               New_cases = ifelse(New_cases < 0, 0, New_cases),
               New_cases_permillion = round2(1e6*New_cases/pop,0),
               roll = rollmean(New_cases_permillion, k=7, na.pad = TRUE, align = "right")) %>%
        ggplot(aes(x=date, y=New_cases_permillion, color = county, label1 = New_cases)) + 
        geom_line(aes(y=roll)) + theme_bw() + 
        labs(y="New cases (per million)", title = "Seven day rolling average of daily cases (per million)")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "7 days")
      ggplotly(g, tooltip = c("date","county","New_cases_permillion","New_cases"))
    })
    
    # new county animation
    output$animcounty <- renderImage({
      progress <- shiny::Progress$new(max = 100)
      progress$set(message = "Rendering", value = 0)
      on.exit(progress$close())
      
      updateShinyProgress <- function(detail) {    
        progress$inc(1, detail = detail)
      }
      outfile3 <- tempfile(fileext='.gif')
      
      # now make the animation
      p = dataCounty() %>%
        filter(county %in% input$county) %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(county) %>% 
        mutate(New_cases = c(NA,diff(ncases)),
               New_cases = ifelse(New_cases < 0, 0, New_cases),
               New_cases_permillion = round2(1e6*New_cases/pop,0),
               roll = rollmean(New_cases_permillion, k=7, na.pad = TRUE, align = "right")) %>%
        ggplot(aes(x=date, y=New_cases_permillion, color = county)) + 
        geom_point() + 
        geom_line(aes(y=roll)) + theme_bw() + 
        labs(y="New cases (per million)", title = "Seven day rolling average of daily cases (per million)")+
        geom_text(data = . %>% mutate(date = date + 2),
                  aes(y=roll,label = county), hjust = 0, size = 4) +
        theme(axis.text.x = element_text(angle=45, hjust = 1),
              legend.position = "none") + 
        scale_x_date(date_breaks = "7 days") + 
        transition_reveal(date)

      anim_save("outfile3.gif", animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile3.gif",
           contentType = 'image/gif',
           # width = 400,
           height = 400
           # alt = "This is alternate text"
      )}, deleteFile = TRUE)
      
    
    ## patient time raw
    output$patienttime <- renderPlotly({
      g = dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        ggplot() + 
        geom_point(aes(x=date,y=Hospitalised),color="orange") + 
        geom_line(aes(x=date,y=Hospitalised),color="orange") + 
        geom_point(aes(x=date,y=ICU),color="red") + 
        geom_line(aes(x=date,y=ICU),color="red") + 
        theme_bw() + 
        labs(y="Cumulative Hospitalised and ICU patients")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g)
    })
    
    ## patient time percentage
    output$patienttimepercent <- renderPlotly({
      g = dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(ICU_percent = round(100*ICU/Cases,1),
               Hospitalised_percent = round(100*Hospitalised/Cases,1)) %>%
        ggplot() + 
        geom_point(aes(x=date,y=Hospitalised_percent), color="orange") + 
        geom_line(aes(x=date,y=Hospitalised_percent), color="orange") + 
        geom_point(aes(x=date,y=ICU_percent),color="red") + 
        geom_line(aes(x=date,y=ICU_percent),color="red") + theme_bw() + 
        labs(y="Hospitalised and ICU patients (% of total cases)")+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g)
    })
    
    ## new icu, death
    output$newpatients <- renderPlotly({
      
      g = dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y"),
               Cases = c(NA,diff(Cases)),
               Hospitalised = c(NA,diff(Hospitalised)),
               ICU = c(NA,diff(ICU)),
               Deaths = c(NA,diff(Deaths))) %>%
        gather(type, count, Cases:Deaths, factor_key=TRUE) %>% 
        filter(type!="Cases") %>%
        ggplot(aes(x=date, y=count, color = type)) + geom_line() + 
        scale_color_manual(values=wes_palette("GrandBudapest1", n=3)) +
        geom_point() + labs(y="New daily counts") + theme_bw()+
        theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
        scale_x_date(date_breaks = "3 days")
      ggplotly(g)
      
    })

    
    ## Table weekly data
    ### county
    output$weekarea <- renderDT({

      dataCounty() %>% mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% arrange(desc(date))
      
    })
  

}

# Run the application 
shinyApp(
    ui = dashboardPage(header, sidebar, body, skin = 'black'),
    server = server
)

