
library(rsconnect)


prepare_data <- function(city,linstings_url,calendar_url,data_date,row)
{
  #read url
  listings <- read.csv(textConnection(readLines(gzcon(url(linstings_url$listings_url)))))
  
  calendar <- read.csv(textConnection(readLines(gzcon(url(calendar_url$calendar_url)))))
  
 
  
  ## Add Keys: columns city and day date
  listings$city <- city$city
  
  listings$data_date <- data_date$data_date
  

  
  ## Select interesting columns
  ### Most columns don't contain interesting information
  columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed", 
                        "latitude", "longitude", 
                        "room_type", "accommodates", "bedrooms", 
                        "beds", "price", "minimum_nights",  "maximum_nights")
  
  listings <- listings %>% 
    select(columns_listings) %>% 
    arrange(id)
  
  # Cleaning calendar dataframe
  
  ## arrange by id and date
  calendar <- calendar %>% 
    arrange(listing_id, date)
  
  ## add day number (starting first day)
  calendar <- calendar %>%
    group_by(listing_id) %>%
    mutate(day_nb = row_number()) %>%
    ungroup()
  
  ## change available column to binary
  calendar <- calendar %>%
    mutate(available = ifelse(available=="t", 1, 0))
  
  ## clean price column and transform to numeric
  calendar <- calendar %>%
    mutate(price = str_replace(price, "\\$", ""),
           adjusted_price = str_replace(adjusted_price, "\\$", ""))
  calendar <- calendar %>%
    mutate(price = str_replace(price, ",", ""),
           adjusted_price = str_replace(adjusted_price, ",", ""))
  calendar <- calendar %>%
    mutate(price = as.numeric(price),
           adjusted_price = as.numeric(adjusted_price))
  
  ## calculate estimated revenue for upcoming day
  calendar <- calendar %>%
    mutate(revenue = price*(1-available))
  
  ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
  calendar <- calendar %>%
    group_by(listing_id) %>%
    summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
        
              price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
              
              revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                         
    )
  
  listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
  
  dir.create(file.path("data_cleansed", city, data_date), recursive = TRUE)
  
  write.csv(listings_cleansed, file.path("data_cleansed", city, data_date, "listings.csv"))
  print(paste0("saving data into ", file.path("data_cleansed", city, data_date, "listings.csv")))
  
}  

#Getting data from the file
read_data <- function(city){
  
  file_url <- file.path("data_cleansed", city)
  file_names <- list.files(file_url)
  files_paths <- c()
  
  for(i in 1:length(file_names)){
    date <- file_names[i]
    file_dir <- file.path("data_cleansed", city,date)
    file_subdirs <- list.dirs(file_dir)
    files_paths <- c(files_paths, file_subdirs)
  }
  
  files_paths <- file.path(files_paths, "listings.csv")
  listings <- 
    do.call(rbind, lapply(files_paths, read.csv, row.names=1))
}


shinyServer(function(session,input,output){
  
  output$country <- renderText(input$statenames)
  output$cities <- renderText(input$city)
   
  #Input Value
  country <- reactive({
    input$statenames
  })
  category <- reactive({
    input$Category
  })
  category2 <- reactive({
    input$Category2
  })
  
  countrydim <- reactive({
    input$statenamesdim
  })
  
  cities <- reactive({
    input$city
  })
  
  citiesdim <- reactive({
    input$citydim
  })
  
  plottype <- reactive({
    input$plottype
  })
  typePlot1 <- reactive({
    input$typePlot1
  })
  typePlot1dim <- reactive({
    input$typePlot1dim
  })
  
  countrydata <- reactive({
    input$statenamesdata
  })
  
  observeEvent(
    input$cities,
    
    if(!is.null(input$cities)){
      ville <- read_data(input$cities)
      updateSelectizeInput(session,"neighborhood", "Select the neighborhood",choices = unique(ville["neighbourhood_cleansed"]))
      updateSelectizeInput(session,"roomtype", "Select the room type",choices = unique(ville["room_type"]))
    }
  )
   
   output$histogram1 <- renderPlot({
     datalist = list()
     
     for(i in 1:length(cities())){
       dat <- read_data(input$city[i])
       datalist[[i]] <- dat
     }
     
     ville = do.call(rbind, datalist)
     ville = subset(ville, data_date>= input$daterange[1] & data_date <= input$daterange[2])
     
     if(input$typePlot1=="geom_bar"){
       #hist(ville[,category()], breaks=input$bins, col=input$color, main="Histogram of Airbnb city", xlab=names(ville[category()]))
       print(ggplot(data=ville,aes(x = ville[,input$cat], fill=ville[,"city"])) + geom_bar(position = position_dodge())
             +labs(title="Histogram of Airbnb city",x =names(ville[,input$cat]),fill=ville[,"city"]))
     }
     
     if(input$typePlot1=="geom_density"){
       print(ggplot(data=ville,aes(x = ville[,input$cat], fill=ville[,"city"])) + geom_density(position = position_dodge())
             +labs(title="Density of Airbnb city",x =names(ville[,input$cat]),fill=ville[,"city"]))
     }
     if(input$typePlot1=="geom_point"){
       print(ggplot(data=ville,aes(x = ville[,input$cat], fill=ville[,"city"])) + geom_boxplot(position = position_dodge())
             +labs(title="Scatter of Airbnb city",x =names(ville[,input$cat]),fill=ville[,"city"]))
     }
     if(input$typePlot1=="geom_violin"){
       print(ggplot(data=ville,aes(x = ville[,input$cat], fill=ville[,"city"])) + geom_boxplot(position = position_dodge())
             +labs(title="Violin of Airbnb city",x =names(ville[,input$cat]),fill=ville[,"city"]))
     }
   })
   
   output$tableavg <- renderDataTable({
     villemean <- read_data(input$city)
     villemean = subset(villemean, data_date>= input$daterange[1] & data_date <= input$daterange[2])
     villemean %>%
       select(city,input$cat) %>%
        filter(city==input$city) %>%
          summarise(avg = mean(villemean[,input$cat], na.rm=TRUE)) %>%
            datatable(villemean, rownames=FALSE)
   })
   
   #render map
   listingCity <- reactive({
     selected0 = input$cities
     selected1 = input$neighborhood
     selected2 = input$roomtype
     selected3 = input$numbed
     print(selected3)
     
     var <- read_data(selected0)
     
     var %>%
       select(neighbourhood_cleansed,room_type,bedrooms,longitude,latitude,price) %>%
       filter(neighbourhood_cleansed == selected1 & room_type == selected2 & bedrooms >= selected3[1] & bedrooms <= selected3[2] )
   })
   output$map <- renderLeaflet({
     leaflet()%>% 
       addTiles() %>% addMarkers(lng = listingCity()$longitude, lat = listingCity()$latitude, popup =listingCity()$price)
   })
   
   output$table <- DT::renderDataTable({
     var2 <- read_data(countrydata())
     datatable(var2, rownames=FALSE)
   })
   
})