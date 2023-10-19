library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(lubridate)
library(stringr)
library(leaflet)

a = read.csv('Airline Dataset.csv') 

a2 = a %>%
  mutate(
    Departure.Date = mdy(Departure.Date),
    Month = month(Departure.Date)
    
  )


ui = dashboardPage(
  dashboardHeader(title = "Passenger & Airline Dashboard(DQ/SY/YXX)"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Airport", tabName = "Airport", icon = icon("th")),
      menuItem("Passenger", tabName = "Passenger", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Airport",
              h2("Airport Data"),
              fluidRow(
                infoBoxOutput("num_ap",width = "200px"),
                
              ),
              fluidRow(
                column(4, selectInput('Continents', 'Continents', choices=c('All', sort(unique(a2$Continents))), selected='All'))
              ),
              leafletOutput('map'),
              plotOutput("ontime_rate")
              
      ),
      tabItem(tabName = "Passenger",
        h2("Passenger Data"),
        fluidRow(
          # Dynamic infoBoxes
           infoBoxOutput("num_psg",width = "100px"),
           infoBoxOutput("num_countries",width = "100px")
        ),
        fluidRow(
          fluidRow(
            column(4, selectInput('Month', 'Flight Month', choices=c('All', sort(unique(a2$Month))), selected = 'All')),
            column(4, selectInput('Nationality', 'Nationality', choices=c('All', sort(unique(a2$Nationality))), selected = 'All'))
                           # column(4, selectInput('bar_border', 'Border Color', choices=colors(), selected='black'))
          ),
          plotOutput("gender_bar"),  
          plotOutput("age_bar")
          
        )
      ) #Passenger
    
    ) # tabitems
  )
)



##############################
# SERVER (ALL THE RENDERING) #
##############################

server = function(input, output){
  
  output$num_psg = renderInfoBox({
    
    num_psg = a2 %>%
      summarise(num_psg = n_distinct(Passenger.ID)) %>%
      pull(num_psg)
    
    infoBox(
      "Total Passengers", num_psg, icon = icon("list"),
      color = "purple"
    )
  })
  
  output$num_ap = renderInfoBox({
   
    num_ap = a2 %>%
      summarise(num_ap = n_distinct(Airport.Name)) %>%
      pull(num_ap)
    
    infoBox(
      "Total Airports", num_ap, icon = icon("list"),
      color = "blue"
    )
  })
  
  output$num_countries = renderInfoBox({
    
    num_c = a2 %>%
      summarise(num_c = n_distinct(Country.Name)) %>%
      pull(num_c)
    
    infoBox(
      "Total Countries", num_c, icon = icon("list"),
      color = "red"
    )
  })
  
  
  # Bar plot of sales by state
  output$gender_bar = renderPlot({
    
    rows = TRUE
    
    if(input$Month != 'All') rows = rows & a2$Month == input$Month
    if(input$Nationality != 'All') rows = rows & a2$Nationality == input$Nationality
    
    
    a2[rows, ] %>%
      group_by(Gender, Nationality) %>%
      ggplot(aes( x= Gender)) +
      geom_bar()+
      labs(y='Number of Passengers') +
      geom_bar(color = 'black', fill = 'skyblue')+
      theme(
        axis.text = element_text(face='bold', size=18),
        text = element_text(face='bold', size=18),
        plot.title = element_text(hjust = 0.5)
      ) +
      ggtitle("Gender Distribution of Passengers")
  })  # gender_bar 
  
 
  
  output$age_bar = renderPlot({
    age_ranges = c(0,18,25,35,45,55, Inf)
    age_labels = c("0-18", "19-25","26-35","36-45", "46-55", "56+")
    
    rows = TRUE
    if(input$Month != 'All') rows = rows & a2$Month == input$Month
    if(input$Nationality != 'All') rows = rows & a2$Nationality == input$Nationality
    
    a2[rows, ]  %>%
      mutate(
        age_group = cut(Age, breaks = age_ranges, labels = age_labels, include.lowest = TRUE)
      ) %>%
      group_by(Gender, age_group) %>%
      ggplot(aes( x= age_group, fill =Gender)) +
      geom_bar() +
      labs(y='Number of Passengers', x='Age group') +
      geom_bar(
        # color = 'black', fill = 'orange'
        )+
      theme(
        axis.text = element_text(face='bold', size=18),
        text = element_text(face='bold', size=18),
        plot.title = element_text(hjust = 0.5)
      )+
      ggtitle("Age Distribution of Passengers")
  })  # age_bar
  
  output$ontime_rate = renderPlot({
    rows = TRUE
    if(input$Continents != 'All') rows = rows & a2$Continents == input$Continents
    a2[rows,] %>%
      group_by(Month) %>%
      summarise( ontimerate = mean(Flight.Status == 'On Time')) %>%
      ggplot(aes(x= Month, y=ontimerate)) +
      geom_line()+
      geom_point()+
      scale_x_continuous(breaks = seq(1, 12, 1))+
      scale_y_continuous(label =percent)+
      labs(
        x = "Month",  
        y = "On Time Rate",  
        title = "On Time Rate by Month",
        caption = "Source: Airline Dataset "
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(face='bold', size=16),
        text = element_text(face='bold', size=16),
        plot.title = element_text(hjust = 0.5)) 
  }) #ontime_rate
  
  
  output$map = renderLeaflet({
    
    
    rows = TRUE
    if(input$Continents != 'All') rows = rows & a2$Continents == input$Continents

    a3 = a2[rows,] %>%
      group_by(Continents, Country.Name, latitude,longitude) %>%
      summarise(num_airport = n_distinct(Airport.Name)) 
    
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)  %>%
      addCircles(
        data = a3,
        label = ~Country.Name,
        # radius=3,
        opacity = 1,
        group = 'Country.Name',
        radius = ~ sqrt(num_airport), # Adjust the size of circles based on counts
        color = "orange",
        fillOpacity = 0.5
      )

  })
  
}

shinyApp(ui, server)