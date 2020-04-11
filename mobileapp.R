library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyMobile)
library(readr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(googleVis)
library(rvest)
library(httr)
library(readxl)

# Set range for day shifts considered in comparison
minshift = -20
maxshift = 30
                            
           
ui = f7Page(
  title = "Coronavirus Ireland",
  f7Init(theme = "dark"),
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
      
      # Time tab
      f7Tab(
        tabName = "time",
        icon = f7Icon("graph_square"),
        active = FALSE,
        # Tab 2 content
        h4(tags$a(href="https://www2.hse.ie/conditions/coronavirus/coronavirus.html", 
                  "HSE Coronavirus information", 
                  target="_blank"),
           paste0(". Data (",Sys.Date(),") from Ireland ("),
           tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                  "HSE Health Protection Surveillance Centre", target="_blank"),
           paste0("), Northern Ireland ("),
           tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14", 
                  "NHS", target="_blank"),
           paste0(") and "),
           tags$a(href="https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases",
                  "ECDC", target="_blank")),
        h4(paste0("Please read this "), 
           tags$a(href="https://www.statslife.org.uk/features/4474-a-statistician-s-guide-to-coronavirus-numbers", 
                  "advice ", target="_blank"),
           paste0("before interpreting the data")),
        infoBoxOutput("CasesBox"),
        infoBoxOutput("MortBox"),
        h4("New cases per day"),
        plotOutput("newcases", width = "95%", height = 400),
        h4("New and cumulative cases per day"),
        plotOutput("cumulcases", width = "95%", height = 400),
        h4("Percentage growth per day"),
        plotOutput("growth", width = "95%", height = 400)
        
      ),
      
      # Map, report tab
      f7Tab(
        tabName = "map",
        icon = f7Icon("calendar"),
        active = FALSE,
        # Tab 5 content
        h3(paste0("Report data (midnight ", Sys.Date()-2, ") from Republic of Ireland only"),
           tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/casesinireland/epidemiologyofcovid-19inireland/", 
                  "HSE Health Protection Surveillance Centre", target="_blank")),
        valueBoxOutput("HospBox"),
        valueBoxOutput("ICUBox"),
        valueBoxOutput("CFRBox"),
        leafletOutput("map", width = "90%", height = 550),
        h4(tags$caption("Cumulative cases by province, per 100,000 population")),
        plotOutput("cumulcountyscaled", width = "90%", height = 400),
        #uiOutput("county_choice"), 
        f7SmartSelect("county", "Counties to compare",
                      choices = c("Carlow","Cavan","Clare","Cork","Donegal","Dublin",
                                  "Galway","Kerry","Kildare","Kilkenny","Laois","Leitrim","Limerick",
                                  "Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon",
                                  "Sligo","Tipperary","Waterford","Westmeath","Wexford","Wicklow"),
                      selected = "Westmeath",
                      multiple = TRUE),
        plotOutput("cumulrealcounty", width = "90%", height = 400),
        # h4(tags$caption("Cumulative cases by age group")),
        # plotOutput("cumulage", width = "70%", height = 600),
        h4(tags$caption("Cases hospitalised and in intensive care")),
        plotOutput("patienttime", width = "90%", height = 400)
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
        uiOutput("country_choice"), 
        # f7SmartSelect("place", "Country to compare", 
        #               choices = c("france","italy","germany", "spain", "united kingdom"), 
        #               selected = "france"),
        h4("Decide how many days behind/ahead Ireland is to your selected country"),
        f7Slider("days","Decide the time difference",min = -5,max=30,value=0,scale = TRUE,step = 1),
        h4("Compare cumulative growth"),
        plotOutput("irelandcompare", width = "90%", height = 400)
      )

    )
  )
)
 
# Define server logic required to draw a leaflet
server <- function(input, output) {
    
  # read in all data
  dataRaw <- reactive({
    dat1 <- read.csv("data/corona_ireland.csv") %>% 
      mutate(pop = case_when(country=="ireland"~6804000)) 
    
    dat2 <- read.csv("data/corona_ireland.csv") %>% 
      mutate(pop = case_when(country=="ireland"~4922000)) %>%
      filter(area!="north") 
    
    if(input$partition == TRUE) return(dat1)
    if(input$partition == FALSE) return(dat2)
    
  })
  
  # irish data only
  dataIreland <- reactive({
    dataRaw() %>% filter(country == "ireland")
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
             gender = "unknown", area = "unknown", pop = popData2018) %>%
      filter(country != "ireland") %>%
      select(date = dateRep, ncase = cases, ndeath = deaths, gender, area, country, pop)
    
    # Make into data.frame and start from date of first case/death
    ecdpcdata = data.frame(ecdpcdata)
    ecdpcdata = ecdpcdata[1:max(which((ecdpcdata$ncase + ecdpcdata$ndeath) > 0)),]
    
    ecdpcdata <- ecdpcdata %>% filter(!(ncase == 0 & ndeath == 0))
    
    dataRaw() %>% 
      select(-lab_location) %>%
      mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
      rbind(ecdpcdata)
  })
    
    # Work out growth rate and save for plotting
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
    
    ## country selector
    output$country_choice <- renderUI({
      f7SmartSelect("place", "Country to compare",
                    choices = unique(dataCountry()$country),
                    selected = "france",
                    multiple = FALSE)
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


    ### detailed data
    
    dataCounty <- reactive({
      read.csv("data/corona_county.csv")
    })
    dataAge <- reactive({
      read.csv("data/corona_age.csv") %>%
        mutate(age_group = factor(age_group, levels(age_group)[c(1,2,7,3:6,8,9)]))
    })
    dataStats <- reactive({
      read.csv("data/corona_stats.csv")
    })

    
    ################### Time series plots ##############
    
    #### Plot cases per day
    output$newcases <- renderPlot({
      
      # dataIreland() %>%
      #   mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
      #   group_by(date) %>%
      #   summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
      #   ggplot(aes(x=Date,y=New_cases)) + geom_col() +
      #   theme(legend.position="none") + theme_bw() + labs(y="Daily New Cases")
      dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,lab_location) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        ggplot(aes(x=Date,y=New_cases,fill=lab_location)) + geom_col() + 
        theme(legend.position="bottom") + labs(y="Daily New Cases")
      
    })
    
    output$cumulcases <- renderPlot({
      
      dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ccases,label=Date,label1=Total_cases,label2=New_cases)) + 
        geom_line() + geom_point() + geom_col(aes(x=date,y=ncases)) +
        theme(legend.position="none") + labs(y="Cases") + theme_bw()
      
    })
    
    # Growth rate
    output$growth <- renderPlot({
      
      dataGrowth() %>%
        mutate(Date = as.Date(date,format = "%d/%m/%Y")) %>%
        filter(Date >= as.Date("13/03/2020",format = "%d/%m/%Y")) %>%
        ggplot(aes(x=Date,y=Growth)) + 
        geom_line() + geom_point() +
        theme(legend.position="none") + labs(y="Growth in Total Cases per day (%)") + theme_bw()
      
    })
    

    ################### Comparison plots ############
    ##### Plot Ireland v other countries
    
    output$irelandcompare <- renderPlot({
      
      
      dat <- dataCountry() %>% 
        filter(country=="ireland" | country == input$place)
      dat <- dat %>% mutate(date = as.Date(date, format = "%d/%m/%Y"), country = as.character(country))
      
      # use round away from zero form of rounding (sometimes called banker's rounding)
      # what many of us learnt in school!
      # check out the "round" package to find out more than you ever wanted to know about the complexities of rounding
      round2 <- function(x, n = 0) (trunc((abs(x) * 10 ^ n) + 0.5) / 10 ^ n) * sign(x)
      
      # scale comparator country to Ireland based on population size (dat$pop)
      prop = mean(dat$pop[dat$country == "ireland"]) / mean(dat$pop[dat$country == input$place])
      dat$ncase[dat$country == input$place] <- round2(dat$ncase[dat$country == input$place] * prop, 0)
      
      # Function to calculate MSE in cumulative cases for a particular shift
      
      cdiffmad <- function(dayshift, dat) { # dat is input, so only locally defined
        
        dat$date[dat$country == input$place] <- dat$date[dat$country == input$place] + dayshift
        
        # extract Ireland and comparator data and calculate cumulative cases
        diffdat = dat %>%
          mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
          filter(country == "ireland" | country == input$place) %>%
          group_by(country, date) %>%
          summarise(ncases = sum(ncase), Date = min(date), Country = country[1]) %>% # sum up over all people
          na.omit() %>%
          mutate(ccases = cumsum(ncases)) # cumulative count upto each day
        
        # Calculate difference in number of cases when shifted and scaled to Ireland
        ireland <- diffdat %>% filter(country == "ireland")
        comp <- diffdat %>% filter(country == input$place)
        comp <- merge(ireland, comp, by = "date") %>%
          mutate(diff = abs(ccases.x - ccases.y)) %>% # absolute differences
          select(date, diff)
        
        cmad = mean(comp$diff, na.rm = T) # MAD estimator
        ifelse(is.na(cmad), Inf, cmad) # allow search to ignore the shift with no overlapping data
      }
      
      allshifts = minshift:maxshift
      allmads = sapply(allshifts, cdiffmad, dat)
      cmad <- allmads[which.min(allmads)]
      bestshift <- allshifts[which.min(allmads)] # must change slider if want this to be default behaviour
      
      # when setting search range of overlap, need to make sure there is enough non-zero entries
      
      # apply the shift to non-Ireland input country
      dayshift = input$days
      dat$date[dat$country==input$place] <- dat$date[dat$country==input$place] + dayshift
      
      # extract Ireland and comparator data and calculate cumulative cases
      diffdat = dat %>%
        mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
        filter(country == "ireland" | country == input$place) %>%
        group_by(country, date) %>%
        summarise(ncases = sum(ncase), Date = min(date), Country = country[1]) %>% # sum up over all people
        na.omit() %>%
        mutate(ccases = cumsum(ncases)) # cumulative count upto each day
      
      # Calculate difference in number of cases when shifted and scaled to Ireland
      ireland <- diffdat %>% filter(country == "ireland")
      comp <- diffdat %>% filter(country == input$place)
      comp <- merge(ireland,comp, by = "date") %>%
        mutate(diff = abs(ccases.x - ccases.y)) %>%  # absolute differences
        select(date, diff)
      
      # change text label for comparator to say indicate shift and scale
      dat$country[dat$country == input$place] <- paste(input$place, "scaled to ireland +", input$days, "days")
      
      xnote = min(as.Date(dat$date, format = "%d/%m/%Y")) + diff(range(as.Date(dat$date, format = "%d/%m/%Y"))) * 0.5
      ynote = max(diffdat$ccases)
      dat %>% mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
        group_by(country, date) %>%
        summarise(New_cases = sum(ncase), Date = min(date), Country = country[1]) %>%
        na.omit() %>%
        mutate(Total_cases = cumsum(New_cases)) %>%
        ggplot(aes(x = date, y = Total_cases, color = country, fill = country, label = Date,
                   label1 = Country, label2 = Total_cases, label3 = New_cases)) +
        geom_line() + geom_point() + labs(y = "Cases (scaled to Ireland)") + theme_bw() + 
        ggtitle(paste0("Mean daily difference is ", round2(mean(comp$diff, na.rm = TRUE), 0), " cases ")) +
        annotate("text", x = xnote, y = ynote, 
                 label = paste0("Closest trajectory at ", bestshift, " days")) + theme(legend.position = "bottom")
      
    })
    
    
    ######################### weekly tab

    
    ## info boxes
    output$HospBox <- renderValueBox({
      dat <-  dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date)) %>%
        mutate(Percentage = round(100*Hospitalised/Cases,1))
      valueBox(paste0(dat$Hospitalised, "(", dat$Percentage, "%)"), "Hospitalised",
               color = "blue"
      )
    })
    output$ICUBox <- renderValueBox({
      dat <-  dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date)) %>%
        mutate(Percentage = round(100*ICU/Cases,1))
      valueBox(paste0(dat$ICU, "(", dat$Percentage, "%)"), "in Intensive Care",
               color = "purple"
      )
    })
    output$CFRBox <- renderValueBox({
      dat <-  dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>% 
        filter(date == max(date))
      valueBox(paste0(dat$CFR, "%"), "Case fatality rate",
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
                                county == "Donegal" ~ -8.11667,
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
                                county == "Roscommon" ~ -8.5792,
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
                               county == "Donegal" ~ 54.65,
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
                               county == "Roscommon" ~ 53.9017,
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
               logcases = log(ncases+1),
               case_groups = case_when(ncases < 20 ~ "1",
                                       ncases >= 20 & ncases < 100 ~ "2",
                                       ncases >= 100 & ncases < 300 ~ "3",
                                       ncases >= 300 ~ "4"))
      
      pal <- colorFactor("YlOrRd", counties$case_groups)
      
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
                         radius = ~6*log(ncases),
                         color = ~pal(case_groups),
                         label = lapply(labs, htmltools::HTML),
                         fillOpacity = 0.9)
    })
    
    ## province time scaled
    output$cumulcountyscaled <- renderPlot({
      
      dataCounty() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(ncases = as.character(ncase),
               ncases = ifelse(ncases == "< = 5","5",ncases)) %>%
        mutate(ncases = as.numeric(ncases)) %>%
        mutate(province = case_when(county=="Carlow"|county=="Dublin"|
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
                                      county=="Monaghan" ~ "Ulster (ROI)")) %>% 
        mutate(pop = case_when(province == "Connacht" ~ 550742,
                               province == "Munster" ~ 1280020,
                               province == "Leinster" ~ 2630720,
                               province == "Ulster (ROI)" ~ 159192+32044+76176)) %>%
        group_by(date,province) %>%
        summarise(Total_cases = sum(ncases), pop = mean(pop)) %>%
        mutate(Cases_per100k = round(100000*Total_cases/pop,0)) %>%
        ggplot(aes(x=date,y=Cases_per100k,color=province,group=province)) + 
        geom_line() + geom_point() + labs(y="Cases per 100,000 population") + theme(legend.position = "bottom")
      
    })  
    
    ## county time
    output$county_selector <- renderUI({
        f7SmartSelect("county", "Counties to compare",
                      choices = unique(dataCounty()$county),
                      selected = "Westmeath",
                      multiple = TRUE)
    })
    
    output$cumulrealcounty <- renderPlot({
      
      dataCounty() %>%
        filter(county %in% input$county) %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(ncases = as.character(ncase),
               ncases = ifelse(ncases == "< = 5","5",ncases)) %>%
        mutate(ncases = as.numeric(ncases)) %>%
        mutate(pop = case_when(county == "Carlow" ~ 24272,
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
                               county == "Dublin" ~ 1347359)) %>%
        group_by(date,county) %>%
        summarise(Total_cases = sum(ncases), pop = mean(pop)) %>%
        mutate(Cases_per100k = round(100000*Total_cases/pop,0)) %>%
        ggplot(aes(x=date,y=Cases_per100k,color=county,group=county)) + 
        geom_line() + geom_point() + theme(legend.position = "bottom") + labs(y="Cases per 100k")
      
    })   


    
    # age spaghetti chart
    output$cumulage <- renderPlot({
      
      dataAge() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,age_group) %>%
        summarise(Total_cases = sum(ncase)) %>%
        ggplot(aes(x=date,y=Total_cases,color=age_group)) + 
        geom_line() + geom_point() + labs(y="Cases") + theme_bw()
      
    })

    ## patient time raw
    output$patienttime <- renderPlot({
      dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        ggplot() + 
        geom_point(aes(x=date,y=Hospitalised),color="orange") + 
        geom_line(aes(x=date,y=Hospitalised),color="orange") + 
        geom_point(aes(x=date,y=ICU),color="red") + 
        geom_line(aes(x=date,y=ICU),color="red") + 
        theme_bw() + 
        labs(y="Hospitalised (orange) and ICU (red) patients")
    })
    
    ## patient time percentage
    output$patienttimepercent <- renderPlot({
      dataStats() %>% 
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(ICU_percent = round(100*ICU/Cases,1),
               Hospitalised_percent = round(100*Hospitalised/Cases,1)) %>%
        ggplot() + 
        geom_point(aes(x=date,y=Hospitalised_percent), color="orange") + 
        geom_line(aes(x=date,y=Hospitalised_percent), color="orange") + 
        geom_point(aes(x=date,y=ICU_percent),color="red") + 
        geom_line(aes(x=date,y=ICU_percent),color="red") + theme_bw() + 
        labs(y="Hospitalised and ICU patients (% of total cases)")
    })

    
}

# Run the application 
shinyApp(
    ui = ui,
    server = server
)

