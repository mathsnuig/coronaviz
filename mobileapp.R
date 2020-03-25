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

# Set range for day shifts considered in comparison
minshift = -5
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
           paste0(". Data (25/03/2020) from Ireland ("),
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
        plotOutput("cumulcases", width = "90%", height = 400)
        
      ),
      
      # Map, report tab
      f7Tab(
        tabName = "map",
        icon = f7Icon("calendar"),
        active = FALSE,
        # Tab 5 content
        h3(paste0("Report data (midnight 23/03/20) from Republic of Ireland only"),
           tags$a(href="https://www.gov.ie/en/collection/ef2560-analysis-of-confirmed-cases-of-covid-19-coronavirus-in-ireland/", 
                  "(National Public Health Emergency Team)", target="_blank")),
        valueBoxOutput("HospBox"),
        valueBoxOutput("ICUBox"),
        valueBoxOutput("CFRBox"),
        leafletOutput("map", width = "90%", height = 600),
        h4(tags$caption("Cumulative cases by province, per 100,000 population")),
        plotOutput("cumulcountyscaled", width = "70%", height = 600),
        # h4(tags$caption("County data")),
        # DTOutput("weekarea", width = "80%"),
        h4(tags$caption("Cumulative cases by age group")),
        plotOutput("cumulage", width = "70%", height = 600),
        # h4(tags$caption("Age data")),
        # DTOutput("weekage", width = "80%"),
        h4(tags$caption("Cumulative cases by transmission type")),
        plotOutput("cumultrans", width = "70%", height = 600)
        #,
        # h4(tags$caption("Transmission data")),
        # DTOutput("weektrans", width = "80%")
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
        plotOutput("irelandcompare", width = "90%", height = 500)
      )
      # ,
      # # Data tab
      # f7Tab(
      #   tabName = "data",
      #   icon = f7Icon("list_number"),
      #   active = FALSE,
      #   # Tab 4 content
      #   h4(tags$caption("Raw data")),
      #   DTOutput("dattable", width = "80%")
      # )

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
    
    # irish data only
    dataIreland <- reactive({
      dataRaw() %>% filter(country == "ireland")
    })

    # Country comparison data
    dataCountry <- reactive({
      dataRaw() %>% filter(country=="ireland" | country == input$place)
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

    # growth box
    output$GrowthBox <- renderValueBox({
      
      dat <- dataGrowth()
      
      valueBox(paste0(dat$median[1],"%"), "Median growth rate", color = "blue")
      
    })
    
    # doubling box
    output$DoublingBox <- renderValueBox({
      
      dat <- dataGrowth()
      
      valueBox(round(70/dat$median[1],2), "Estimated days for number of cases to double", color = "maroon")
      
    })
    
    
    ### detailed data
    
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
    
    
    #################### Data Table  ###################
    output$dattable <- renderDT({
      
      dataRaw() %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))
    
      })
    
    
    
    ################### Time series plots ##############
    #### Plot cases per day
    output$cumulcases <- renderPlot({
      
      dataIreland() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date) %>%
        summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date)) %>%
        na.omit() %>%
        mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
        ggplot(aes(x=date,y=ccases,label=Date,label1=Total_cases,label2=New_cases)) + 
        geom_line() + geom_point() + geom_col(aes(x=date,y=ncases)) +
        theme(legend.position="none") + labs(y="Cases")
      
    })
    

    ################### Comparison plots ############
    ##### Plot Ireland v other countries
    # output$irelandcompare <- renderPlot({
    #   
    #   
    #   dat <- dataCountry()
    #   dat <- dat %>% mutate(date = as.Date(date,format = "%d/%m/%Y"), country = as.character(country)) 
    #   prop = mean(dat$pop[dat$country=="ireland"])/mean(dat$pop[dat$country==input$place])
    #   dat$ncase[dat$country==input$place] <- round(dat$ncase[dat$country==input$place]*prop,0)
    #   dat$date[dat$country==input$place] <- dat$date[dat$country==input$place]+input$days
    #   
    #   diffdat = dat %>% 
    #     mutate(date = as.Date(date,format = "%d/%m/%Y"))%>%
    #     filter(country=="ireland"|country==input$place) %>%
    #     group_by(country, date) %>%
    #     summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date), Country = country[1]) %>%
    #     na.omit() %>%
    #     mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases))
    #   
    #   ireland <- diffdat %>% filter(country == "ireland")
    #   comp <- diffdat %>% filter(country == input$place)
    #   comp <- merge(ireland,comp,by="date") %>%
    #     mutate(diff = abs(Total_cases.x - Total_cases.y)) %>%
    #     select(date,diff)
    #   
    #   
    #   dat$country[dat$country==input$place] <- paste(input$place, "scaled to ireland +",input$days,"days")
    #   
    #   dat %>% mutate(date = as.Date(date,format = "%d/%m/%Y"))%>%
    #     group_by(country, date) %>%
    #     summarise(ncases = sum(ncase), New_cases = sum(ncase), Date = min(date), Country = country[1]) %>%
    #     na.omit() %>%
    #     mutate(ccases = cumsum(ncases), Total_cases= cumsum(ncases)) %>%
    #     ggplot(aes(x=date,y=ccases,color=country,fill=country,label=Date,label1=Country,label2=Total_cases)) +
    #     geom_line() + geom_point() + labs(y="Cases (scaled to Ireland)") + theme_bw() + 
    #     theme(legend.position = "bottom")
    #   
    # }) 
    
    output$irelandcompare <- renderPlot({
      
      
      dat <- dataCountry()
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
                 label = paste0("Closest trajectory at ", bestshift, " days")) + 
        theme(legend.position = "bottom")
      
    })
    
    
    ######################### weekly tab

    
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
        geom_line() + geom_point() + labs(y="Cases per 100,000 population")
      
    })  


    
    # age spaghetti chart
    output$cumulage <- renderPlot({
      
      dataAge() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        group_by(date,age_group) %>%
        summarise(Total_cases = sum(ncase)) %>%
        ggplot(aes(x=date,y=Total_cases,color=age_group)) + 
        geom_line() + geom_point() + labs(y="Cases")
      
    })
    

    # trans spaghetti chart
    output$cumultrans <- renderPlot({
      
      dataTrans() %>%
        mutate(date = as.Date(date,format = "%d/%m/%Y")) %>%
        mutate(transmission = case_when(transmission == "Community transmission" ~ "Community",
                                        transmission == "Contact with a confirmed case" ~ "Contact",
                                        transmission == "Travel abroad" ~ "Travel",
                                        transmission == "Under investigation" ~ "Not yet known")) %>%
        group_by(date,transmission) %>%
        summarise(Total_cases = sum(ncase)) %>%
        ggplot(aes(x=date,y=Total_cases,color=transmission)) + 
        geom_line() + geom_point() + labs(y="Cases")

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

