#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#AUTHOR - APARNA SURESH
#STUDENT ID - 32263546


library(knitr)
library(DataExplorer)
library(naniar)
library(RColorBrewer)
library(randomcoloR)
library(leaflet)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(shiny)
library(plotly)
library(xts)
library(dygraphs)
library(bslib)
library(repr)

#/Users/aparnasuresh/Desktop/data_vis/Visualisation/www/
data <- read_csv("data.csv")
options(warn=-1)

# Define UI for application that draws a histogram
ui <- fluidPage(

  

  navbarPage(title = "YouTube Data Visualisation", 
             theme = bs_theme(bootswatch = "lux",
                              bg = "#FFFFFF ",
                              fg = "black",
                              primary = "maroon",
                              base_font = font_google("Montserrat")),
             #tabPanel(title = "Introduction",
             tabPanel(title = "Home",
                      imageOutput("home_img"),
                      #br(),
                      hr(),
                      h4(strong("Project Description")),
                      p(style="text-align: justify; font-size = 25px",
                        "Youtube is one of the most famous entertainment platforms in the recent years. Youtube has been serving us all right when it comes to music, entertainment, or even in terms of education. The number of videos that is being uploaded all over the world on a daily basis is innumerable which gives many opportunities to clean and analyse the data. The amount of possibilities to analyse this data is enormous. The dataset that is used for this project is the YouTube data of the countries namely the USA, Canada and Great Britain. Here, I have tried to explore and visualise the dataset. This presentation shows the five design sheet for this project which will be explained in the upcoming slides.
 ", align = "center",
                      hr(),
                      p(strong("Aparna Suresh ")),
                      #hr(),
                      p(strong("Student ID - 32263546"))
                      
             )),
              tabPanel(title = "How to orchestrate?",
                fluidRow(
                  p("The first visualisation is about the time when most of the videos have been published. On hovering over the plot, you can see the number of videos published and the time of the day when the videos were published. The time seen in the visualisation represents 24 hour format.
                    The X-axis represents the time and Y-axis represents the video count."),
                  br(),
                  p("The second visualisation focusses on the relationship between the publishing date and the trending date of the video. (i.e) How many days does it to take to reach trending from publishing and the corresponding count of them. The X-axis is the range of time difference between the publish and trending date in days. The Y-axis is the range of time difference between the publish and trending date. On hovering over the points on the graph, you can see the number of videos published within the range of days mentioned on the X-axis."),
                  br(),
                  p(" The third visualisation draws the connection among the likes, views and dislikes of the videos. The publish date goes back from 2006 t0 2018. To explain this, I have used a time series graph to visualise it better.  
                  There is a dropdown box from which the user can choose from Views Vs Likes, Views Vs Dislikes and Likes vs  Dislikes. On hovering over the graph, you can see the mean of the entity and the count of that selected entities on the top right corner. 
                  Since there will be two graph lines within one graph to represent two entities, hovering on either of them, you can view the corresponding details of the graph on that point.")
                )
              ),       
                      
  
             tabPanel(title = "Overall analysis",
                      theme = bs_theme
                      (bg = "red",
                       fg = "black",
                       primary = "maroon",
                       base_font = font_google("Montserrat")),
                      fluidRow
                      (
                        h1("Data exploration and Visualisation of the YouTube dataset"),
                        column
                        (8,
                          h3("Time of the day when people usually tend to post videos"),
                        plotlyOutput("publishtime_all")
                        )
                      ),
                      
                      fluidRow(
                        h3("Relationship between the publishing date and the trending date of the video"),
                        column(8,
                        plotlyOutput("dayrange_all"))),
                      
                      fluidRow(
                        h3("Relationship in views, dislikes and likes")),
                      fluidRow(
                        selectInput(inputId = "major1", label = "Choose one", choices = c("Views Vs Likes", "Views Vs Dislikes", "Likes Vs Dislikes")),
                        dygraphOutput("major1")
                    )),
                      
             tabPanel(title = "USA",
                      theme = bs_theme(bg = "white",
                                       fg = "black",
                                       primary = "maroon",
                                       base_font = font_google("Montserrat")),
                      
             
             fluidRow(
               h1("Data exploration and Visualisation of the YouTube dataset in USA"),
               h3("Time of the day when people usually tend to post videos in USA"),
               column(8,
               plotlyOutput("publishtime_usa"))),
             
             fluidRow(
               h3("Relationship between the publishing date and the trending date of the video"),
               column(8,
               plotlyOutput("dayrange_usa"))),
             
             fluidRow(
               h3("Relationship in Views, Likes, and Dislikes ")),
             fluidRow(
               selectInput(inputId = "major2", label = "Choose one", choices = c("Views Vs Likes", "Views Vs Dislikes", "Likes Vs Dislikes")),
               dygraphOutput("major2")
             )),

             
             tabPanel(title =  "Canada",
                      theme = bs_theme(bg = "#D8FAFC",
                                       fg = "black",
                                       primary = "maroon",
                                       base_font = font_google("Montserrat")),
                      fluidRow(
                        h1("Data exploration and Visualisation of the YouTube dataset in Canada"),
                        column(8,
                        
                        h3("Time of the day when people usually tend to post videos in Canada"),
                        
                        plotlyOutput("publishtime_ca"))),
                      
                      fluidRow(
                        h3("Relationship between the publishing date and the trending date of the video in Canada"),
                        br(),
                        column(8,
                        plotlyOutput("dayrange_ca"),
                        br())),
                      
                      fluidRow(
                        h3("Relationship in views, likes and dislikes in Canada")),
                        #br(),
                      fluidRow(
                        selectInput(inputId = "major3", label = "Choose one", choices = c("Views Vs Likes", "Views Vs Dislikes", "Likes Vs Dislikes")),
                        dygraphOutput("major3")
                      )),
                      
             
             tabPanel(title = "Great Britain",
                      theme = bs_theme(bg = "#D8FAFC",
                                       fg = "black",
                                       primary = "maroon",
                                       base_font = font_google("Montserrat")),
                      
                      fluidRow(
                        h1("Data exploration and Visualisation of the YouTube dataset in the Great Britain"),
                        column(8,
                        h3("Time of the day when people usually tend to post videos in the Great Britain"),
                        
                        plotlyOutput("publishtime_gb"))),
                      
                      fluidRow(
                        h3("Relationship between the publishing date and the trending date of the video in the Great Britain"),
                        column(8,
                               plotlyOutput("dayrange_gb")
                               )
                        ),
                      
                      fluidRow(
                        h3("Relationship between views, likes and dislikes in the Great Britain")),
                      fluidRow(
                        selectInput(inputId = "major4", label = "Choose one" ,choices = c("Views Vs Likes", "Views Vs Dislikes", "Likes Vs Dislikes")),
                        dygraphOutput("major4")
                      )

  ),
  inverse = T
  )
)

 
 
 
 
 
 
# Define server logic required to draw a histogram
server <- function(input, output) {

    
    d <- reactive({
      req(input$country_timepublish)
      
    })
  
    output$home_img <- renderImage({
      
      list(src = "yt.png",
           width = "20%",
           #align = "center",
           height = 100
           )}, deleteFile = F)
    
    
    
    
    output$publishtime_all <- renderPlotly({
      publish_data_all <- data %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(hour = substring(publish_time, 1, 2))
      
      publish_data_all <-  publish_data_all %>% group_by(hour) %>% 
        summarise(count = n())
      
      p1 <- 
        ggplot(publish_data_all, aes(x = hour, y = count, fill = count)) +
        geom_segment(aes(x=hour, xend=hour, y=0, yend=count), color = "black", size = 2.5) +
        geom_point(size=1, color="red", fill=alpha("orange", 0.3), alpha=0.9, shape=21, stroke=2) +
        theme(axis.text.x = element_text(angle=90, hjust = 1)) +
        labs(x = "Time", y = "Video count") +
        ggtitle("Time of the day when people tend to publish videos")
        

      ggplotly(p1)
    })
    
    
    output$publishtime_usa <- renderPlotly({
      publish_data_usa <- data %>%
        filter(`Country Name` == "USA") %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(hour = substring(publish_time, 1, 2))
      
      publish_data_usa <-  publish_data_usa %>% group_by(hour) %>% 
        summarise(count = n())
      publish_data_usa
      
      p2 <- ggplot(publish_data_usa, aes(x = hour, y = count, fill = count)) +
        geom_segment(aes(x=hour, xend=hour, y=0, yend=count), color = "black", size = 2.5) +
        geom_point(size=1, color="red", fill=alpha("orange", 0.3), alpha=0.9, shape=21, stroke=2) +
        theme(axis.text.x = element_text(angle=90, hjust = 1)) +
        labs(x = "Time", y = "Video count") +
        ggtitle("Time of the day when people tend to publish videos")

      
      ggplotly(p2)
    })
    
    
    output$publishtime_ca <- renderPlotly({
      publish_data_ca <- data %>%
        filter(`Country Name` == "Canada") %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(hour = substring(publish_time, 1, 2))
      
      publish_data_ca <-  publish_data_ca %>% group_by(hour) %>% 
        summarise(count = n())
      publish_data_ca
      
      p3 <- ggplot(publish_data_ca, aes(x = hour, y = count, fill = count)) +
        geom_segment(aes(x=hour, xend=hour, y=0, yend=count), color = "black", size = 2.5) +
        geom_point(size=1, color="red", fill=alpha("orange", 0.3), alpha=0.9, shape=21, stroke=2) +
        theme(axis.text.x = element_text(angle=90, hjust = 1)) +
        labs(x = "Time", y = "Video count") +
        ggtitle("Time of the day when people tend to publish videos")
      
      ggplotly(p3)
    })
    
    
    output$publishtime_gb <- renderPlotly({
      publish_data_gb <- data %>%
        filter(`Country Name` == "Great Britain") %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(hour = substring(publish_time, 1, 2))
      
      publish_data_gb <-  publish_data_gb %>% group_by(hour) %>% 
        summarise(count = n())
      publish_data_gb
      
      p4 <- ggplot(publish_data_gb, aes(x = hour, y = count, fill = count)) +
        geom_segment(aes(x=hour, xend=hour, y=0, yend=count), color = "black", size = 2.5) +
        geom_point(size=1, color="red", fill=alpha("orange", 0.3), alpha=0.9, shape=21, stroke=2) +
        theme(axis.text.x = element_text(angle=90, hjust = 1)) +
        labs(x = "Time", y = "Video count") +
        ggtitle("Time of the day when people tend to publish videos")

      ggplotly(p4)
    })
 #-----------------------------------------------------------------------------------------------   
    
    
    
    output$dayrange_all <- renderPlotly({
      
      trend_data_all <- data %>%
        select(c(trending_date, publish_time)) %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(trending_date = as.Date(trending_date, format = "%y-%d-%m"),
               publish_date = as.Date(publish_date , format = "%Y-%m-%d"),
               time_difference =  trending_date - publish_date) %>%
        group_by((time_difference)) %>%
        summarise(videos_count = n()) %>%
        separate(`(time_difference)`, sep = " ", into = c("days_difference_count", "days"))
      
      trend_data_all <- trend_data_all %>% select(days_difference_count, videos_count)
      
  
      
      
      trend_data_range_all <- trend_data_all %>% 
        mutate(days_difference_count = as.numeric(days_difference_count),
               range = ifelse(days_difference_count >= 0 & days_difference_count < 10, "0-10 days", 
                              ifelse(days_difference_count >= 10 & days_difference_count < 20, "10-20 days", 
                                     ifelse(days_difference_count >= 20 & days_difference_count < 30, "20-30 days",
                                            ifelse(days_difference_count >= 30 & days_difference_count < 40, "30-40 days",
                                                   ifelse(days_difference_count >= 40 & days_difference_count < 50, "40-50 days", 
                                                          ifelse(days_difference_count >= 50 & days_difference_count < 100, "50-100 days", 
                                                                 ifelse(days_difference_count >= 100 & days_difference_count < 1000, "100-1000 days", 
                                                                        ifelse(days_difference_count >= 1000 & days_difference_count < 2000, "1000-2000 days",
                                                                               ifelse(days_difference_count >= 2000 & days_difference_count < 3000, "2000-3000 days",                                                                      ifelse(days_difference_count >= 3000 & days_difference_count< 4000, "3000-4000 days" ,
                                                                                                                                                                                                                                                  ifelse(days_difference_count >= 4000 & days_difference_count < 5000, "4000-5000 days", 
                                                                                                                                                                                                                                                         days_difference_count))))))))))))

      trend_data_range_all$range = factor(trend_data_range_all$range,levels = c("0-10 days", "10-20 days", "20-30 days", "30-40 days", "40-50 days", "50-100 days", "100-1000 days", "1000-2000 days", "2000-3000 days", "3000-4000 days", "4000-5000 days"))
      
      trend_data_range_all <- trend_data_range_all %>%
        group_by(range) %>% 
        summarise(total_count = sum(videos_count)) 
      

        
        q1 <- ggplot(trend_data_range_all, aes(x = range, y = total_count)) +
        geom_point() +
        geom_line(group = 1) +
        geom_text(aes(label = total_count), angle = 45, vjust = 0.2, hjust = -0.5, size = 2.5) +
        theme(axis.text.x = element_text(angle = 90)) +
        xlab("Days difference between the publish and trending date") +
        ylab("Video count within the day difference") +
        ggtitle("Relationship between the publishing date and the trending date of the video")
      
      ggplotly(q1)
    })
    
    ###########################################################################################################
    
    output$dayrange_usa <- renderPlotly({
      
      trend_data_usa <- data %>%
        filter(`Country Name` == "USA") %>%
        select(c(trending_date, publish_time)) %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(trending_date = as.Date(trending_date, format = "%y-%d-%m"),
               publish_date = as.Date(publish_date , format = "%Y-%m-%d"),
               time_difference =  trending_date - publish_date) %>%
        group_by((time_difference)) %>%
        summarise(videos_count = n()) %>%
        separate(`(time_difference)`, sep = " ", into = c("days_difference_count", "days"))
      
      trend_data_usa <- trend_data_usa %>% select(days_difference_count, videos_count)
      
      
      
      
      trend_data_range_usa <- trend_data_usa %>% 
        mutate(days_difference_count = as.numeric(days_difference_count),
               range = ifelse(days_difference_count >= 0 & days_difference_count < 10, "0-10 days", 
                              ifelse(days_difference_count >= 10 & days_difference_count < 20, "10-20 days", 
                                     ifelse(days_difference_count >= 20 & days_difference_count < 30, "20-30 days",
                                            ifelse(days_difference_count >= 30 & days_difference_count < 40, "30-40 days",
                                                   ifelse(days_difference_count >= 40 & days_difference_count < 50, "40-50 days", 
                                                          ifelse(days_difference_count >= 50 & days_difference_count < 100, "50-100 days", 
                                                                 ifelse(days_difference_count >= 100 & days_difference_count < 1000, "100-1000 days", 
                                                                        ifelse(days_difference_count >= 1000 & days_difference_count < 2000, "1000-2000 days",
                                                                               ifelse(days_difference_count >= 2000 & days_difference_count < 3000, "2000-3000 days",                                                                      ifelse(days_difference_count >= 3000 & days_difference_count< 4000, "3000-4000 days" ,
                                                                                                                                                                                                                                                  ifelse(days_difference_count >= 4000 & days_difference_count < 5000, "4000-5000 days", 
                                                                                                                                                                                                                                                         days_difference_count))))))))))))

      
      trend_data_range_usa$range = factor(trend_data_range_usa$range,levels = c("0-10 days", "10-20 days", "20-30 days", "30-40 days", "40-50 days", "50-100 days", "100-1000 days", "1000-2000 days", "2000-3000 days", "3000-4000 days", "4000-5000 days"))
      
      trend_data_range_usa <- trend_data_range_usa %>%
        group_by(range) %>% 
        summarise(total_count = sum(videos_count)) 
      

      
      q2 <- ggplot(trend_data_range_usa, aes(x = range, y = total_count)) +
        geom_point() +
        geom_line(group = 1) +
        geom_text(aes(label = total_count), angle = 5, vjust = 0.2, hjust = -0.5, size = 2.5) +
        theme(axis.text.x = element_text(angle = 90)) +
        xlab("Days difference between the publish and trending date") +
        ylab("Video count within the day difference") +
        ggtitle("Relationship between the publishing date and the trending date of the video")
      
      ggplotly(q2)
    })
    
    ######################################################################################################
    
    
    output$dayrange_ca <- renderPlotly({
      
      trend_data_ca <- data %>%
        filter(`Country Name` == "Canada") %>%
        select(c(trending_date, publish_time)) %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(trending_date = as.Date(trending_date, format = "%y-%d-%m"),
               publish_date = as.Date(publish_date , format = "%Y-%m-%d"),
               time_difference =  trending_date - publish_date) %>%
        group_by((time_difference)) %>%
        summarise(videos_count = n()) %>%
        separate(`(time_difference)`, sep = " ", into = c("days_difference_count", "days"))
      
      trend_data_ca <- trend_data_ca %>% select(days_difference_count, videos_count)
      
      
      
      
      trend_data_range_ca <- trend_data_ca %>% 
        mutate(days_difference_count = as.numeric(days_difference_count),
               range = ifelse(days_difference_count >= 0 & days_difference_count < 10, "0-10 days", 
                              ifelse(days_difference_count >= 10 & days_difference_count < 20, "10-20 days", 
                                     ifelse(days_difference_count >= 20 & days_difference_count < 30, "20-30 days",
                                            ifelse(days_difference_count >= 30 & days_difference_count < 40, "30-40 days",
                                                   ifelse(days_difference_count >= 40 & days_difference_count < 50, "40-50 days", 
                                                          ifelse(days_difference_count >= 50 & days_difference_count < 100, "50-100 days", 
                                                                 ifelse(days_difference_count >= 100 & days_difference_count < 1000, "100-1000 days", 
                                                                        ifelse(days_difference_count >= 1000 & days_difference_count < 2000, "1000-2000 days",
                                                                               ifelse(days_difference_count >= 2000 & days_difference_count < 3000, "2000-3000 days",                                                                      ifelse(days_difference_count >= 3000 & days_difference_count< 4000, "3000-4000 days" ,
                                                                                                                                                                                                                                                  ifelse(days_difference_count >= 4000 & days_difference_count < 5000, "4000-5000 days", 
                                                                                                                                                                                                                                                         days_difference_count))))))))))))
      
      
      trend_data_range_ca$range = factor(trend_data_range_ca$range,levels = c("0-10 days", "10-20 days", "20-30 days", "30-40 days", "40-50 days", "50-100 days", "100-1000 days", "1000-2000 days", "2000-3000 days", "3000-4000 days", "4000-5000 days"))
      
      trend_data_range_ca <- trend_data_range_ca %>%
        group_by(range) %>% 
        summarise(total_count = sum(videos_count)) 
      

      
      q3 <- ggplot(trend_data_range_ca, aes(x = range, y = total_count)) +
        geom_point() +
        geom_line(group = 1) +
        geom_text(aes(label = total_count), angle = 5, vjust = 0.2, hjust = -0.5, size = 2.5) +
        theme(axis.text.x = element_text(angle = 90)) +
        xlab("Days difference between the publish and trending date") +
        ylab("Video count within the day difference") +
        ggtitle("Relationship between the publishing date and the trending date of the video")
      
      ggplotly(q3)
    })
    
    
    ######################################################################################################
    
    
    output$dayrange_gb <- renderPlotly({
      
      trend_data_gb <- data %>%
        filter(`Country Name` == "Great Britain") %>%
        select(c(trending_date, publish_time)) %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>%
        mutate(trending_date = as.Date(trending_date, format = "%y-%d-%m"),
               publish_date = as.Date(publish_date , format = "%Y-%m-%d"),
               time_difference =  trending_date - publish_date) %>%
        group_by((time_difference)) %>%
        summarise(videos_count = n()) %>%
        separate(`(time_difference)`, sep = " ", into = c("days_difference_count", "days"))
      
      trend_data_gb <- trend_data_gb %>% select(days_difference_count, videos_count)
      
      
      
      
      trend_data_range_gb <- trend_data_gb %>% 
        mutate(days_difference_count = as.numeric(days_difference_count),
               range = ifelse(days_difference_count >= 0 & days_difference_count < 10, "0-10 days", 
                              ifelse(days_difference_count >= 10 & days_difference_count < 20, "10-20 days", 
                                     ifelse(days_difference_count >= 20 & days_difference_count < 30, "20-30 days",
                                            ifelse(days_difference_count >= 30 & days_difference_count < 40, "30-40 days",
                                                   ifelse(days_difference_count >= 40 & days_difference_count < 50, "40-50 days", 
                                                          ifelse(days_difference_count >= 50 & days_difference_count < 100, "50-100 days", 
                                                                 ifelse(days_difference_count >= 100 & days_difference_count < 1000, "100-1000 days", 
                                                                        ifelse(days_difference_count >= 1000 & days_difference_count < 2000, "1000-2000 days",
                                                                               ifelse(days_difference_count >= 2000 & days_difference_count < 3000, "2000-3000 days",                                                                      ifelse(days_difference_count >= 3000 & days_difference_count< 4000, "3000-4000 days" ,
                                                                                                                                                                                                                                                  ifelse(days_difference_count >= 4000 & days_difference_count < 5000, "4000-5000 days", 
                                                                                                                                                                                                                                                         days_difference_count))))))))))))
      
      
      trend_data_range_gb$range = factor(trend_data_range_gb$range,levels = c("0-10 days", "10-20 days", "20-30 days", "30-40 days", "40-50 days", "50-100 days", "100-1000 days", "1000-2000 days", "2000-3000 days", "3000-4000 days", "4000-5000 days"))
      
      trend_data_range_gb <- trend_data_range_gb %>%
        group_by(range) %>% 
        summarise(total_count = sum(videos_count)) 
      
      
      #trend_data_range %>% 
      
      q4 <- ggplot(trend_data_range_gb, aes(x = range, y = total_count)) +
        geom_point() +
        geom_line(group = 1) +
        geom_text(aes(label = total_count), angle = 5, vjust = 0.2, hjust = -0.5, size = 2.5) +
        theme(axis.text.x = element_text(angle = 90)) +
        xlab("Range of time difference between the publish and trending date") +
        ylab("Count of videos within the time difference in publish and trending date") +
        ggtitle("Relationship between the publishing date and the trending date of the video")
      
      ggplotly(q4)
    })
    
    
    
#---------------------------------------------------------------------------------------------------------------
    
    #OVERALL
 
    #likeviews_all
    output$major1 <- renderDygraph({
      
      #VIEWS VS LIKES IN ALL
      if (input$major1 == "Views Vs Likes"){
      data_vl_all<- data %>% 
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
        mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
        mutate(dt = format(publish_date, "%Y-%m")) %>% 
        select(views, likes, dt)  %>% 
        mutate(dt = as.yearmon(dt))%>% 
        group_by(dt) %>% 
        summarise(views = mean(views)/10,
                  likes = mean(likes)) %>% 
        select(dt, views, likes)
      
      views_likes_all = xts( x = data_vl_all [,-1], order.by= data_vl_all$dt)
      dygraph(views_likes_all, main = "views vs likes") %>%
        dyOptions( fillGraph=TRUE)%>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
      }
      #VIEWS VS DISLIKES
      else if (input$major1 == "Views Vs Dislikes"){
        data_vd_all<- data %>% 
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(views, dislikes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(views = mean(views)/10,
                    dislikes = mean(dislikes)) %>% 
          select(dt, views, dislikes)
        
        views_dislikes_all = xts( x = data_vd_all [,-1], order.by= data_vd_all$dt)
        dygraph(views_dislikes_all, main = "views vs dislikes") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
      
      #LIKES VS DISLIKES
      else if (input$major1 == "Likes Vs Dislikes"){
        data_ld_all<- data %>% 
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(likes, dislikes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(likes = mean(likes),
                    dislikes = mean(dislikes)) %>% 
          select(dt, likes, dislikes)
        
        likes_dislikes_all = xts( x = data_ld_all [,-1], order.by= data_ld_all$dt)
        dygraph(likes_dislikes_all, main = "likes vs dislikes") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
   } )
    
    
    #USA
    output$major2 <- renderDygraph({
      
      #VIEWS VS LIKES IN USA
      if (input$major2 == "Views Vs Likes"){
      data_vl_usa<- data %>% 
        filter(`Country Name` == "USA") %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
        mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
        mutate(dt = format(publish_date, "%Y-%m")) %>% 
        select(views, likes, dt)  %>% 
        mutate(dt = as.yearmon(dt))%>% 
        group_by(dt) %>% 
        summarise(views = mean(views)/10,
                  likes = mean(likes)) %>% 
        select(dt, views, likes)
      
      views_likes_usa = xts( x = data_vl_usa [,-1], order.by= data_vl_usa$dt)
      dygraph(views_likes_usa, main = "views vs likes in USA") %>%
        dyOptions( fillGraph=TRUE)%>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
      }
      
      #VIEWS VS DISLIKES
      else if(input$major2 == "Views Vs Dislikes"){
        data_vd_usa<- data %>% 
          filter(`Country Name` == "USA") %>%
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(views, dislikes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(views = mean(views)/10,
                    dislikes = mean(dislikes)) %>% 
          select(dt, views, dislikes)
        
        views_dislikes_usa = xts( x = data_vd_usa [,-1], order.by= data_vd_usa$dt)
        dygraph(views_dislikes_usa, main = "views vs dislikes in USA") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
      
      #LIKES VS DISLIKES
      else if(input$major2 == "Likes Vs Dislikes"){
        data_ld_usa<- data %>% 
          filter(`Country Name` == "USA") %>%
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(likes, dislikes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(likes = mean(likes),
                    dislikes = mean(dislikes)) %>% 
          select(dt, likes, dislikes)
        
        likes_dislikes_usa = xts( x = data_ld_usa [,-1], order.by= data_ld_usa$dt)
        dygraph(likes_dislikes_usa, main = "likes vs dislikes in the USA") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
      
    } )
    
  #####################################################################################################################
    
    
   #CANADA
    output$major3 <- renderDygraph({
      
      #VIEWS VS LIKES IN CA
      if (input$major3 == "Views Vs Likes"){
      data_vl_ca<- data %>% 
        filter(`Country Name` == "Canada") %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
        mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
        mutate(dt = format(publish_date, "%Y-%m")) %>% 
        select(views, likes, dt)  %>% 
        mutate(dt = as.yearmon(dt))%>% 
        group_by(dt) %>% 
        summarise(views = mean(views)/10,
                  likes = mean(likes)) %>% 
        select(dt, views, likes)
      
      views_likes_ca = xts( x = data_vl_ca [,-1], order.by= data_vl_ca$dt)
      dygraph(views_likes_ca, main = "views vs likes in Canada") %>%
        dyOptions( fillGraph=TRUE)%>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
      }
      
      #VIEWS VS DISLIKES
      
      else if(input$major3 == "Views Vs Dislikes"){
        data_vd_ca<- data %>% 
          filter(`Country Name` == "Canada") %>%
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(views, dislikes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(views = mean(views)/10,
                    dislikes = mean(dislikes)) %>% 
          select(dt, views, dislikes)
        
        views_dislikes_ca = xts( x = data_vd_ca [,-1], order.by= data_vd_ca$dt)
        dygraph(views_dislikes_ca, main = "views vs dislikes in Canada") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
      
      #LIKES VS DISLIKES
      
      else if(input$major3 == "Likes Vs Dislikes"){
        data_ld_ca<- data %>% 
          filter(`Country Name` == "Canada") %>%
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(likes, dislikes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(likes = mean(likes),
                    dislikes = mean(dislikes)) %>% 
          select(dt, likes, dislikes)
        
        likes_dislikes_ca = xts( x = data_ld_ca [,-1], order.by= data_ld_ca$dt)
        dygraph(likes_dislikes_ca, main = "likes vs dislikes in Canada") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
      
    } )
    

    
    #################################################################################################################
    
  
    #GREAT BRITAIN
    
    output$major4 <- renderDygraph({
      #VIEWS VS LIKES IN GB
      if(input$major4 == "Views Vs Likes"){
        data_vl_gb<- data %>% 
          filter(`Country Name` == "Great Britain") %>%
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(views, likes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(views = mean(views)/100,
                    likes = mean(likes)) %>% 
          select(dt, views, likes)
        
        views_likes_gb = xts( x = data_vl_gb [,-1], order.by= data_vl_gb$dt)
        dygraph(views_likes_gb, main = "views vs likes in Great Britain") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
      
      #VIEWS VS DISLIKES IN GB
      else if(input$major4 == "Views Vs Dislikes"){
        data_vd_gb<- data %>% 
          filter(`Country Name` == "Great Britain") %>%
          separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
          mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
          mutate(dt = format(publish_date, "%Y-%m")) %>% 
          select(views, dislikes, dt)  %>% 
          mutate(dt = as.yearmon(dt))%>% 
          group_by(dt) %>% 
          summarise(views = mean(views)/100,
                    dislikes = mean(dislikes)) %>% 
          select(dt, views, dislikes)
        
        views_dislikes_gb = xts( x = data_vd_gb [,-1], order.by= data_vd_gb$dt)
        dygraph(views_dislikes_gb, main = "views vs dislikes in the Great Britain") %>%
          dyOptions( fillGraph=TRUE)%>%
          dyRangeSelector() %>%
          dyCrosshair(direction = "vertical") %>%
          dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
          dyRoller(rollPeriod = 1)
      }
      
      #LIKES VS DISLIKES IN GB
      else if(input$major4 == "Likes Vs Dislikes"){
      data_ld_gb<- data %>% 
        filter(`Country Name` == "Great Britain") %>%
        separate(publish_time, sep = " ", into = c("publish_date", "publish_time")) %>% 
        mutate(publish_date = as.Date(publish_date , format = "%Y-%m-%d")) %>%
        mutate(dt = format(publish_date, "%Y-%m")) %>% 
        select(likes, dislikes, dt)  %>% 
        mutate(dt = as.yearmon(dt))%>% 
        group_by(dt) %>% 
        summarise(likes = mean(likes),
                  dislikes = mean(dislikes)) %>% 
        select(dt, likes, dislikes)
      
      likes_dislikes_gb = xts( x = data_ld_gb [,-1], order.by= data_ld_gb$dt)
      dygraph(likes_dislikes_gb, main = "likes vs dislikes in the Great Britain") %>%
        dyOptions( fillGraph=TRUE)%>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
      }
    })

    
    
    
    
 #-----------------------------------------------------------------------------------   
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
