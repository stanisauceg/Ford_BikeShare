library(shiny)
library(sf)
library(tidyverse)
library(lubridate)

sample_day <- readr::read_csv(file.path("..", "results", "sample_day.csv")) %>%
  mutate(member_gender = as.factor(member_gender),
         user_type = as.factor(user_type),
         bike_share_for_all_trip = as.factor(bike_share_for_all_trip),
         weekday = as.factor(weekday),
         is_weekend = as.factor(is_weekend))

bikeways <- st_read("../data/SF_bikeways/bikeways.shp")
sf_shp <- st_read("../data/BayArea/bayarea_general.shp")

sample_day_sf <- st_as_sf(sample_day, coords = c("start_station_longitude", "start_station_latitude"), crs = st_crs(bikeways))
sf_shp_crs <- st_transform(sf_shp, crs = st_crs(bikeways))

ui <- fluidPage(
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for time
      sliderInput("hr", "Time of Day (24 hr)",
                  min = 0, max = 23,
                  value = 6)#,
      # select display of arrivals vs departures vs both
      # selectInput(inputId = "type", label = "Display arrivals or departures:",
      #             choices = c("Arrivals", "Departures", "Arrivals & Departures"),
      #             selected = 'Arrivals')
    ),
    
    # Outputs
    mainPanel(
      #tabsetPanel(tabPanel("Main",plotOutput("map", height = 1000, width = 1000)))
      plotOutput(outputId = "map", width = 800)
    )
  )
)

server <- function(input, output) {
  # Create a subset of data filtering for selected title types
#  rides_selected <- reactive({
#    req(input$type) # ensure availablity of value before proceeding
#    filter(movies, title_type %in% input$type)
#  })
#  
#  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
#  
#  output$scatterplot <- renderPlot({
#    ggplot(data = movies_selected(), aes_string(x = input$x, y = input$y)) +
#      geom_point()})
  
  # Create scatterplot object the plotOutput function is expecting
  output$map <- renderPlot({
    ggplot(sample_day %>% filter(hour(start_time) == input$hr)) +
      geom_sf(data = sf_shp_crs, color = "black", fill = "NA") +
      geom_sf(data = bikeways, color = "cornflower blue") +
      geom_segment(aes(x = start_station_longitude, y = start_station_latitude,
                     xend = end_station_longitude, yend = end_station_latitude),
                 alpha = 0.1, show.legend = FALSE) +
      scale_size(range = c(.1, 10), limits = c(1, 30)) +
      xlim(c(-122.48, -122.37)) + 
      ylim(c(37.745, 37.81)) +
      theme_bw() +
      coord_sf() +
      geom_point(aes(x = start_station_longitude, y = start_station_latitude),
                 color = "blue", alpha = 0.2, stat = "sum") +
      geom_point(aes(x = end_station_longitude, y = end_station_latitude),
                 color = "red", alpha = 0.15, stat = "sum") +
      labs(title = "San Francisco hourly bikeshare traffic", x = "Longitude", y = "Latitude", 
           size = "Average \ndepartures (blue)\nand arrivals (red)\nper hour")
  })
} 

shinyApp(ui = ui, server = server)
