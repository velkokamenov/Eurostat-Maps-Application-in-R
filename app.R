#######################################################
#                                                     #
# Aim of the script - This is a script of a shiny app #
# Allowing to plot data downloaded from eurostat      #
#                                                     #
#######################################################

# All the parts of the application - global, ui and server are in this script

# Date developed: 14 March 2018 - revised on 25 December 2018

# Load user-defined function for package loading and installation
source("./Functions/F_Load_Install_Packages.R")

# Load dictionaries - downloaded from Eurostat
# Dictionary about the units of measurment
D_Unit = readr::read_tsv("./Dictionaries/unit.dic", col_names = c("unit", "unit_name"),
                col_types = readr::cols(.default = readr::col_character()))

# Dictionary about countries and regions names
D_Geo = readr::read_tsv("./Dictionaries/geo.dic", col_names = c("geo", "geo_name"),
                            col_types = readr::cols(.default = readr::col_character()))

# Load user-defined labeling function
source("./Functions/F_Label_Eurostat.R")

#Load libraries - and install if still not installed on the machine
load_install_packages("shiny")
load_install_packages("eurostat")
load_install_packages("countrycode")
load_install_packages("dplyr")
load_install_packages("DT")
load_install_packages("xlsx")
load_install_packages("googleVis")

# Load libraries
# library(DT)
# library(shiny)
# library(eurostat)
# library(dplyr)
# library(xlsx)
# library(googleVis)

##################################################################
#                                                                #  
# This part of the script plays the role of the global part      # 
# Of the application - here we load data from eurostat           #
# Using the Eurostat package for easy data download in R         #
#                                                                #
##################################################################

### Extract here economic indicators from eurostat - using the get_eurostat() function
#queryGDP = search_eurostat("GDP",type = "table")

dataGDPperCapita = get_eurostat(id = "sdg_08_10",time_format="num") %>%
  filter(values > 500)

dataGDPGrowthVolume = get_eurostat(id = "tec00115", time_format = "num")

dataInflationRate = get_eurostat(id = "tec00118", time_format = "num")

dataUnemployment = get_eurostat(id = "tps00203", time_format = "num") %>%
  filter(unit == "PC_POP")

dataMinimumWage = get_eurostat(id = "tps00155", time_format = "num") %>%
  filter(!is.na(values)) %>%
  plyr::rename(c("currency" = "unit"))

### Extract here education indicators data 
dataEducation = get_eurostat(id = "tesem030",time_format="num")

dataUpperSecondary = get_eurostat(id = "tesem240", time_format = "num")

dataEarlyLeavers = get_eurostat(id = "tesem020", time_format = "num")

### Extract here financial indicators data
dataInvestments = get_eurostat(id = "tec00011",time_format="num") %>%
  filter(unit == "PC_GDP")

dataVentureCapitalInvestments = get_eurostat(id = "tin00141", time_format = "num")

dataFinancialSectorLiabilities = get_eurostat(id = "tipsfs30", time_format = "num") %>%
  filter(unit == "PC_GDP" & !geo %in% c("LU"))

dataShortTermInterest = get_eurostat(id = "tec00035", time_format = "num") %>%
  mutate(unit = "PC")

### Extract here technology indicators data
dataExport = get_eurostat(id = "tin00140",time_format="num")

dataHighTechEmployment = get_eurostat(id = "tsc00011",time_format = "num")

dataHumanResourcesTech = get_eurostat(id="tsc00025", time_format = "num") %>%
  filter(sex == "T")

dataInternetAccess = get_eurostat(id = "tin00134", time_format = "num")

##################################################################
#                                                                #
# This part of the script defines the UI part of the application #
#                                                                #
##################################################################

# Define UI for application 
ui <- shinyUI( fluidPage(
  includeCSS("styles.css"),
  
  headerPanel("Eurostat maps"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      tags$head(tags$style("#mapplot{height:100vh !important;}")),
      
      # Select type of data to examine and plot on a map
      radioButtons(inputId = "idicatorsGroups",
                   label = "Select type of data you want to examine:",
                   choices = c("Economics" 
                               ,"Finance"
                               ,"Education" 
                               ,"Technology"
                   )
                   ,selected = "Economics"
                   
      ),
      
      uiOutput(
        "SelectEurostatData"
      ),
      
      uiOutput("SelectYear"),
      
      h4("Downloading data"),
      
      downloadButton("DownloadData","Download data"),
      
      br(),br(),
      
      h4("Built with", tags$img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "thanks to", tags$img(src = "http://www.novinite.com/media/images/2016-06/photo_verybig_174936.jpg", height = "30px"),".")),
    
    
    # Outputs
    mainPanel(
      tabsetPanel( type = "tabs",
                   id = "eurostatTabs",
                   tabPanel("Map Plot",span(textOutput(outputId = "MapTitle"),style = "font-size: 20px"),
                            htmlOutput(outputId = "mapplot")
                   ),
                   tabPanel("Data", dataTableOutput(outputId = "mapdata"))
                   
      )
    )
  )
)
)

######################################################################
#                                                                    #
# This part of the script defines the server part of the application #
#                                                                    #
######################################################################

# Define server logic 
server <- function(input, output) {
  
  output$SelectEurostatData = renderUI({
    selectInput(inputId = "EurostatData",
                label = "Select indicator to plot on a map:",
                choices = if (input$idicatorsGroups == "Economics"){
                  c("GDP Per Capita"
                    ,"Real GDP % Growth"
                    ,"Inflation - HICP"
                    ,"Unemployment Rate"
                    ,"Minimum Wage")
                  
                }
                
                else if (input$idicatorsGroups == "Education"){
                  c("Tertiary education attainment - age group 30-34"
                    ,"Percent with at least secondary education"
                    ,"Percent early leavers from education age 18-24")
                }
                
                else if (input$idicatorsGroups == "Finance"){
                  c("Investments"
                    ,"Venture capital investments"
                    ,"Financial sector liabilities"
                    ,"Short-term interest rates"
                  )
                }
                else if (input$idicatorsGroups == "Technology") {
                  c("High tech exports %"
                    ,"% employed in high-tech manufacturing and service"
                    ,"Human resource in science and technology"
                    ,"Level of internet access"
                  )
                }
                
    )
    
    
    
  })
  
  datasetInput <- eventReactive(input$EurostatData,{
    
    switch(input$EurostatData,
           # Economy
           "GDP Per Capita" = dataGDPperCapita
           ,"Real GDP % Growth" = dataGDPGrowthVolume
           ,"Inflation - HICP" = dataInflationRate
           ,"Unemployment Rate" = dataUnemployment
           ,"Minimum Wage" = dataMinimumWage
           # Finance
           ,"Investments" = dataInvestments
           ,"Venture capital investments" = dataVentureCapitalInvestments
           ,"Financial sector liabilities" = dataFinancialSectorLiabilities
           ,"Short-term interest rates" = dataShortTermInterest
           # Technology
           ,"High tech exports %" = dataExport
           ,"% employed in high-tech manufacturing and service" = dataHighTechEmployment
           ,"Human resource in science and technology" = dataHumanResourcesTech
           ,"Level of internet access" = dataInternetAccess
           #Education
           ,"Tertiary education attainment - age group 30-34" = dataEducation
           ,"Percent with at least secondary education" = dataUpperSecondary
           ,"Percent early leavers from education age 18-24" = dataEarlyLeavers
    )
    
  })
  
  
  output$SelectYear = renderUI({
    req(datasetInput())
    yearMin = min(datasetInput()$time)
    yearMax = max(datasetInput()$time)
    sliderInput(inputId = "YearSelection",
                label = "Select the year for which you want to plot the data:",
                min = yearMin,
                max = yearMax,
                value = yearMax,
                step = 1,
                sep = "")
  })
  
  unitValues = reactive({
    #req(datasetInput())
    dataLabeled = label_eurostat_my(datasetInput(),D_Unit, D_Geo)
    unit = tolower(dataLabeled$unit[1])
    return(unit)
  })
  
  mapTitle = reactive({
    titleString = paste(input$EurostatData,"by country,",input$YearSelection," in", unitValues())
    return(titleString)
  })
  
  dataToPlot = reactive({
    req(datasetInput())
    
    dataLabeled = label_eurostat_my(datasetInput(),D_Unit, D_Geo)
    
    dataLabeled = dataLabeled %>%
      mutate(geo = case_when(.$geo == "Germany (until 1990 former territory of the FRG)" ~ "Germany",
                             .$geo == "Former Yugoslav Republic of Macedonia, the" ~ "Macedonia",
                             .$geo == "Czechia" ~ "Czech Republic",
                             TRUE ~ as.character(.$geo)))
    
    dataForPlot = dataLabeled %>%
      plyr::rename(c("geo"="Country","time"="Year")) %>%
      filter(Year == input$YearSelection) 
    
    googlePlot = gvisGeoChart(dataForPlot, locationvar="Country", colorvar="values",
                              options=list(width=600, height=400, region = "150", colors = "blue"))
    
    return(googlePlot)
    
  })
  
  dataToTable = reactive({
    req(datasetInput())
    dataForTable = datasetInput() %>% 
      label_eurostat_my(D_Unit,D_Geo) %>%
      select(geo,time,values,unit) %>%
      plyr::rename(c("geo" = "Country", "time" = "Year"))
    
    return(dataForTable)
  })
  
  output$MapTitle = renderText({
    req(mapTitle())
    mapTitle()
  })
  
  output$mapplot <- renderGvis({
    req(dataToPlot())
    dataToPlot()
    
  })
  
  output$mapdata = renderDataTable({
    req(dataToTable())
    datatable(data = dataToTable(), 
              options = list(pageLength = 10), 
              rownames = FALSE)
  })
  
  
  output$DownloadData = downloadHandler(
    filename = function() {paste(input$EurostatData,' by country ',min(datasetInput()$time),'-',
                                 max(datasetInput()$time), '.xlsx', sep='')},
    content = function(file) {
      write.xlsx(data.frame(dataToTable()),file,row.names=FALSE)
    }
    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)