#TO DEPLOY rsconnect::deployApp('/Users/sambryan/Documents/School/WPI/2022_Fall/DS501/Homework/Homework_6/ds501_homework6')

library(shiny)
library(maps)
library(mapproj)
library(rworldmap)

#setwd("/Users/sambryan/Documents/School/WPI/2022_Fall/DS501/Homework/Homework_6/ds501_homework6")

head(human_resources)
summary(facilities)

# Define UI ----
ui <- fluidPage(
  titlePanel(
      h1("Worldwide Mental Health Statistics", align = "center")
    ),
  
  sidebarLayout(
    sidebarPanel(
      br(),
      h2(strong("Visualizing the Problem")),
      h3("And the state of the current solutions"),
      br(),
      p("Select the tabs to toggle between viewing between seeing gross suicides, mental health facilities, and human resources per country."),
      p("The dropdown menu will provide options of subcategories in each"),
      br(),
      tags$a(href="https://www.kaggle.com/datasets/twinkle0705/mental-health-and-suicide-rates?resource=download", "Source of data, from Kaggle ")
      
      ),
  mainPanel(
      tabsetPanel(type="tabs", id="tp",
                  tabPanel("Suicides", 
                           selectInput("select", h3("Select age range"), 
                                       choices = list("All age groups" = 'all', "10 to 19" = '10s Suicides', "20 to 29" = '20s Suicides',
                                                      "30 to 39" = '30s Suicides', "40 to 49" = '40s Suicides',
                                                      "50 to 59" = '50s Suicides', "60 to 69" = '60s Suicides',
                                                      "70 to 79" = '70s Suicides', "80 and above" = '80s+ Suicides'), selected = 'all'),
                           plotOutput("mPlot", height="560px", width="950px")
                           ),
                  
                  tabPanel("Facilities",
                           selectInput("f_select", h3("Select Mental Health Facilities"), 
                                       choices = list("Mental Hospitals" = 'hospitals', "Health Units" = 'health units',
                                                      "Outpatient Facilities" = 'outpatient facilites', "Day Treatment" = 'day treatment',
                                                      "Residential Facilities" = 'residential facilities'), selected = 'hospitals'),
                           plotOutput("mPlot2", height="560px", width="950px")
                           ),
                  tabPanel("Human Resources",
                           selectInput("hr_select", h3("Select Mental Health Facilities"), 
                                       choices = list("Psychiatrists" = 'psychiatrists', "Nurses" = 'nurses',
                                                      "Social Workers" = 'social workers', "Psychologists" = 'psychologists'), selected = 'psychiatrists'),
                           plotOutput("mPlot3", height="560px", width="950px")
                           )
                  )
      )
    ) 
  )

# Define server logic ----
server <- function(input, output) {
  
  # Generate the Map 
    output$mPlot <- renderPlot({
      
      suicide_rates <- read.csv("data/crude suicide rates.csv")
      
      df10 <- aggregate(suicide_rates$X10to19, list(suicide_rates$Country), FUN=sum)
      df20 <- aggregate(suicide_rates$X20to29, list(suicide_rates$Country), FUN=sum)
      df30 <- aggregate(suicide_rates$X30to39, list(suicide_rates$Country), FUN=sum)
      df40 <- aggregate(suicide_rates$X40to49, list(suicide_rates$Country), FUN=sum)
      df50 <- aggregate(suicide_rates$X50to59, list(suicide_rates$Country), FUN=sum)
      df60 <- aggregate(suicide_rates$X60to69, list(suicide_rates$Country), FUN=sum)
      df70 <- aggregate(suicide_rates$X70to79, list(suicide_rates$Country), FUN=sum)
      df80 <- aggregate(suicide_rates$X80_above, list(suicide_rates$Country), FUN=sum)
      dfa <- data.frame(suicide_rates$Country, df10$x + df20$x + df30$x + df40$x + df50$x + df60$x + df70$x + df80$x)
    
      if (input$select == '10s Suicides') dfx <- df10
      if (input$select == '20s Suicides') dfx <- df20
      if (input$select == '30s Suicides') dfx <- df30
      if (input$select == '40s Suicides') dfx <- df40
      if (input$select == '50s Suicides') dfx <- df50
      if (input$select == '60s Suicides') dfx <- df60
      if (input$select == '70s Suicids') dfx <- df70
      if (input$select == '80s+ Suicides') dfx <- df80
      if (input$select == 'all') dfx <- dfa
      
      
      dfx2 <- setNames(dfx,c("Country", "x"))
      sPDF <- joinCountryData2Map(dfx2, joinCode='NAME', nameJoinColumn='Country')
      
      mapParams <- mapPolys(sPDF, nameColumnToPlot="x", mapRegion='world',
                              missingCountryCol='dark grey', numCats=10, 
                              colourPalette=c('green4','green1','greenyellow','yellow','yellow2','orange','coral','red','red3','red4'),
                              addLegend=TRUE,
                              oceanCol='light blue',
                              mapTitle='Suicides per 100,000 in age range by country in 2016')
      mtext("[Grey Color: No Data Available]",side=1,line=-1)
        
      })
    
    output$mPlot2 <- renderPlot({
      
      facilities <- read.csv("data/facilities.csv")
      
      if (input$f_select == 'hospitals') dfx <- data.frame(facilities$Country, facilities$Mental._hospitals)
      if (input$f_select == 'health units') dfx <- data.frame(facilities$Country, facilities$health_units)
      if (input$f_select == 'outpatient facilites') dfx <- data.frame(facilities$Country, facilities$outpatient._facilities)
      if (input$f_select == 'day treatment') dfx <- data.frame(facilities$Country, facilities$day._treatment)
      if (input$f_select == 'residential facilities') dfx <- data.frame(facilities$Country, facilities$residential_facilities)
      dfx2 <- setNames(dfx, c("Country", "x"))
      sPDF <- joinCountryData2Map(dfx2, joinCode='NAME', nameJoinColumn='Country')
      
      mapParams_f <- mapPolys(sPDF, nameColumnToPlot="x", mapRegion='world',
                            missingCountryCol='dark grey', numCats=10, 
                            colourPalette=c('green4','green1','greenyellow','yellow','yellow2','orange','coral','red','red3','red4'),
                            addLegend=TRUE,
                            oceanCol='light blue',
                            mapTitle='Facilities per 100,000 by country in 2016')
      mtext("[Grey Color: No Data Available]",side=1,line=-1)
      
    })
    
    output$mPlot3 <- renderPlot({
    
    human_resources <- read.csv("data/human resources.csv")

    if (input$hr_select == 'psychiatrists') dfx <- data.frame(human_resources$Country, human_resources$Psychiatrists)
    if (input$hr_select == 'nurses') dfx <- data.frame(human_resources$Country, human_resources$Nurses)
    if (input$hr_select == 'social workers') dfx <- data.frame(human_resources$Country, human_resources$Social_workers)
    if (input$hr_select == 'psychologists') dfx <- data.frame(human_resources$Country, human_resources$Psychologists)
    
    dfx2 <- setNames(dfx, c("Country", "x"))
    sPDF <- joinCountryData2Map(dfx2, joinCode='NAME', nameJoinColumn='Country')
    
    mapParams_f <- mapPolys(sPDF, nameColumnToPlot="x", mapRegion='world',
                            missingCountryCol='dark grey', numCats=10, 
                            colourPalette=c('green4','green1','greenyellow','yellow','yellow2','orange','coral','red','red3','red4'),
                            addLegend=TRUE,
                            oceanCol='light blue',
                            mapTitle = 'Human resources per 100,000 by country in 2016')
    mtext("[Grey Color: No Data Available]",side=1,line=-1)
    
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)

