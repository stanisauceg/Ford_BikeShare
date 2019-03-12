library(shiny)
library(tidyverse)

data_clean <- data.table::fread("data_clean.csv") %>% 
  as.tbl() %>%
  mutate(start_station_city = as.factor(start_station_city),
         end_station_city = as.factor(end_station_city),
         member_gender = as.factor(member_gender),
         user_type = as.factor(user_type),
         bike_share_for_all_trip = as.factor(bike_share_for_all_trip),
         start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time),
         bike_id = as.character(bike_id),
         weekday = as.factor(weekday),
         is_weekend = as.factor(is_weekend))

station_stats <- readr::read_csv(file.path("results", "station_stats.csv")) %>% 
  as.tbl() %>%
  mutate(city = as.factor(city))


# set scaling factor: what is the duration, in days, of the data series?
get_timespan <- function(time_data){
  time_diff <- max(time_data) - min(time_data)
  return(time_diff[[1]])
}

timespan <- get_timespan(data_clean$start_time)

# randomly subsample the dataset to obtain an "average" day, at certain times
set.seed(24)
sample_day <- sample_frac(data_clean, size = 1/timespan)


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
      geom_segment(aes(x = start_station_longitude, y = start_station_latitude,
                     xend = end_station_longitude, yend = end_station_latitude),
                 alpha = 0.1, show.legend = FALSE) +
      scale_size(range = c(.1, 10), limits = c(1, 30)) +
      xlim(c(-122.48, -122.37)) + 
      ylim(c(37.745, 37.81)) +
      theme_bw() +
      coord_equal() +
      geom_point(aes(x = start_station_longitude, y = start_station_latitude),
                 color = "blue", alpha = 0.2, stat = "sum") +
      geom_point(aes(x = end_station_longitude, y = end_station_latitude),
                 color = "red", alpha = 0.15, stat = "sum") +
      labs(title = "SF hourly bike traffic", x = "Longitude", y = "Latitude", 
           size = "departures \nand arrivals \nper hour")
  })
} 

shinyApp(ui = ui, server = server)
