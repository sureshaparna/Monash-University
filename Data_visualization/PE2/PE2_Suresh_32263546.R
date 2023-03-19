#Loading the libraries
library(dplyr)
library(shiny)
library(leaflet)
library(ggplot2)
library(lubridate)

# Reading the CSV file
data <- read.csv("Butterfly_biodiversity_survey_2017_PE2.csv")


# Calculating the mean of the latitude, longitude and the count of the butterflies in each site
site_data <- data %>% group_by(Site) %>%
  summarise(Longitude = mean(Lon), Latitude = mean(Lat), BflyCount = sum(ButterflyCount)) %>% 
  ungroup() %>% 
  
  #creating a new column with the name of the site and its count together
  mutate(site_count = paste(Site , ", ", BflyCount))


#Converting the format of the date in the dateTime column 
data$Datetime <- parse_date_time(data$Datetime, "%m/%d/%Y %H:%M:%S %p")


# Creating a dataframe of the sites with highest butterfly count 
BCount <- data %>% 
  group_by(Site) %>%
  summarise(ButterflyCount = sum(ButterflyCount)) %>%
  arrange(-ButterflyCount) %>%
  slice(1:5)



# Define UI for application that draws the visualisation for the butterflies data
ui <- fluidPage(
  
 
    fluidRow(
      
      #Column 1 for the title
      column(12,
             wellPanel(tags$h2("SURVEY OF BUTTERFLIES IN MELBOURNE 2017"))),
      
      #Column 2 for the description of the data and the project
      column(12,
             wellPanel("The following visualisations are from the data Butterfly biodiversity survey from the city of Melbourne site. The main agenda of the data was to study the butterfly biodiversity and flower-butterfy interactions in Melbourne between January - March 2017. The visualization shows the butterfly count in each site in the form of a map, a bar graph that shows the sites with the highest butterfly count and the number of butterflies in those five sites during each day.")),
      
      #Column 3 for the description of the map 
      column(4,
             tags$h5("Location of the survey"),
             wellPanel("The Map shows the count of butterflies available in the areas namely  Argyle Square, 	
 Canning/Neill St Reserve,  Carlton Gardens South,  Fitzroy-Treasury Gardens, Gardiner Reserve, Garrard Street Reserve,  Lincoln Square,  Murchinson Square,  Pleasance Gardens, Princes Park, 	
 Royal Park, State Library of Victoria,  University Square, Westgate Park, and  Womens Peace Gardens in Melbourne, Victoria. The highest count of buterflies (60) is found in Womens Peace Gardens and Royal Park and the least (0) is found in State Library of Victoria, Canning/ Neill St Reserve,  and University Square. Click on the circle markers to see the Butterfly count in each site."
      )),
      
      
      #Column 4 for the slider and the map
      #Constructing a map on the basis of butterfly count in each site
      column(8,
             sliderInput("range", "Butterfly Count in each area:",
                                          min = min(site_data$BflyCount), max = max(site_data$BflyCount),
                                          value = c(min(site_data$BflyCount),max(site_data$BflyCount))),
             
                              #output of the map
                              leafletOutput("map")),
    
        #adding a horizontal line after the row
     column(12,
          tags$hr())
),



  fluidRow(
    
    #Column 1 for the visualization-1
    column(4, 
           plotOutput("sitecount")),
    
    #Column 2 for the description of both the visualisations
    column(2,
           tags$h5("Top Sites for Butterflies"),
           wellPanel("The visualisation in the left shows the top five places with the highest count of butterflies which are Royal Park (60), Womens Peace Gardens (60), Carlton Gardens South (37), Fitzroy-Treasury Gardens (36) and  Westgate Park (27). The visualisation plot on the right shows the total number of
butterflies observed each day at the same 5 sites. By keeping the mouse over the graphs, you can see the values of Site, butterfly count and the date.")),
    
    #Column 3 for the visualisation-2
    column(6,
           plotOutput("vis2"))
  )
  )


    
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    
    #Setting the minimum and maximum range for the slider
    temp_data <- site_data %>% filter(BflyCount >= input$range[1] & BflyCount <= input$range[2])
    
    #Adding cirle markers on the map based on the butterfly count, latitude and longitude of every site
    leaflet(temp_data) %>%
      addTiles() %>% 
      addCircleMarkers(~Longitude , 
                       ~Latitude, 
                       popup = ~as.character(site_count), 
                       label = ~BflyCount, 
                       labelOptions = labelOptions(permanent = TRUE))
  })
  
  # VIS-1
  #Plotting the top 5 sites with the highest butterfly count 
  output$sitecount <- renderPlot(
    {
      #plotting a bar graph between the site and butterfly count from the data BCount
      BCount %>% ggplot(aes(x = Site, y = ButterflyCount, fill = Site, label = ButterflyCount)) + 
        geom_bar(stat = "identity") +
        theme(legend.position = "none") +
        ggtitle("Top 5 places with highest Butterfly count") +
        theme(axis.text.x = element_text(angle=90, hjust = 1)) +
        labs(x = "Site", y = "Butterfly Count")
      }
  )
  
  
  #VIS-2
  #Visualising the number of butterflies observed during each day in those five sites with highest butterfly count using ggplot2
  output$vis2 <-renderPlot(
    {
        data %>% 
        #filtering the sites from the data
        filter(Site %in% BCount$Site) %>% 
        
        #creating a new column for date
        mutate(date = as.Date(Datetime)) %>% 
        
        #grouping the data by site and date
        group_by(Site, date) %>% 
        
        #adding the butterfly count by site
        summarise(ButterflyCount = sum(ButterflyCount)) %>% 
        ungroup() %>% 
        
        #plotting the plot between date and butterfly count of the five sites from the dataframe data.
        ggplot(aes(x = date, y = ButterflyCount, color = Site, label = ButterflyCount)) +
        theme(legend.position = "bottom") +
        ggtitle("Number of butterflies observed during each day in the top 5 Sites" ) +
        geom_text(vjust = -0.5, color = "black")+
        geom_line(stat = "identity")
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
