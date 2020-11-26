library(shiny)
library(stringr)
library(shinydashboard)
library(ggplot2) 
library(dplyr)
library(DT)
library(data.table)
library(leaflet)


shinyUI(
    dashboardPage(
      skin = "black",
        dashboardHeader(title = 'AirBnb project'),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Comparing Cities", tabName = "tab1"),
                menuItem("Deep dive into a city ", tabName = "tab2")
            )),
        
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "tab1",
                        fluidRow( 
                            selectInput("city", "City : ", c("barcelona", "berlin", "bordeaux", "euskadi", "girona", "lyon", "madrid", "malaga", "mallorca", "menorca", "munich", "paris", "sevilla", "valencia"),multiple = TRUE, selected = "euskadi"),
                            plotOutput("histogram1"),
                            selectInput("cat", "Category", c("room_type" , 'bedrooms' ,
                                                                        "beds", "price", "neighbourhood_cleansed", "availability_30", "price_30", "revenue_30"
                                                                        )),
                            selectInput("typePlot1", "Plot type", c("Bars"="geom_bar", "Density"="geom_density", "Points"="geom_point", "Violin" = "geom_violin")),
                                dateRangeInput(
                                    inputId = "daterange",
                                    label = "Date", 
                                    start = "2020-01-01",
                                    end = "2020-09-19",
                                    min = "2020-01-01",
                                    max = "2020-09-19",
                                    format = "dd/mm/yyyy"
                                ),
                        )
                ),
                tabItem(tabName = "tab2",
                        fluidPage(
                            leafletOutput("map"),
                                selectInput("cities", "City", c("barcelona", "berlin", "bordeaux", "euskadi", "girona", "lyon","madrid", "malaga", "mallorca", "menorca", "munich", "paris", "sevilla", "valencia"), selected = "euskadi"),
                                selectizeInput("neighborhood", "Neighborhood", choices = "", selected = ""),
                                selectizeInput("roomtype", "Room type", choices = "", selected = "Hotel room"),
                                sliderInput("numbed", "Number of bedrooms", min = 0, max = 10, value = c(0,10)),
                        )
                )
            )
            
            
        )
    )
)