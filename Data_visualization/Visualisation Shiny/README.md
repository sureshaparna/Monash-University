## Analysis of the YouTube Dataset using R Shiny
This Application using R Shiny takes you through the analysis of the Youtube Dataset from Kaggle. I have combined three datasets of 3 different countries (as mentioned below) and used it for the analysis. The dataset comprises of variables like the likes, comments, views, country, publish time and so on. The countries that I have analysed are the Great Britain, Canada, and the USA. I have analysed factors like views, likes and comments based on countries and the timeline. The timelaine in the dataset varies from 2006-2018. 

### Home Page

<img width="1440" alt="homepage" src="https://github.com/sureshaparna/Monash-University/assets/39727231/1f1a4fe9-e427-47cc-94df-54fc46889a43">


This page helps you navigate your way to analayse and see the visualisations. There are 6 options where you can see the visualisations depending on the option you choose. 

## How to orchestrate?

This page tells you how you can navigate through each page of the application. 

<img width="1439" alt="orchestration" src="https://github.com/sureshaparna/Monash-University/assets/39727231/fad584a2-47a9-4db5-86ad-d22055d18780">


## Overall Analysis 

This page shows the overall analysis of all the 3 countries. Here you can see factors like what time the videos get published usually globally, relationship between publishing date and the trending date (Basically how many days it takes for a video to reach trending on the platform), Relationship between 
- Likes and Views
- Likes and Dislikes
- Views and Dislikes
  
The user can select what relationship they want to see within what specific timeline. 

<img width="1404" alt="general_1" src="https://github.com/sureshaparna/Monash-University/assets/39727231/c5a03c8f-afb0-4021-b05b-ce1835da13ae">


## Analysis of Trending videos

<img width="1433" alt="general_2" src="https://github.com/sureshaparna/Monash-University/assets/39727231/baebd537-e614-4dfe-bc80-a40f9cdb5f47">

## Relationship between the key factors of a video

The user can choose a specific timeline and a specific relationship. This is achieved using a Dygraph for the timeline selection.

![image](/Users/aparnasuresh/Desktop/Monash/data_vis/Visualisation/README/selection_1.png)

The above image shows the relationship between Views and Likes for a timeline. On hovering over the graph, it shows the corresponding data for that specific timeline. 


## Analysis for the data in the USA

![image](/Users/aparnasuresh/Desktop/Monash/data_vis/Visualisation/README/usa.png)


## Analysis for the data in Canada

![image](/Users/aparnasuresh/Desktop/Monash/data_vis/Visualisation/README/canada.png)

## Analysis for the data in the Great Britain

![image](/Users/aparnasuresh/Desktop/Monash/data_vis/Visualisation/README/gb.png)

Thus, we can see the analysis of the YouTube data from three different countries using the application created using R Shiny.
