#### BikeShare: deeper dive ########################################################

# make sure to run the "Ford_BikeShare_Load_and_Prep.R" file first;
# the "Ford_BikeShare_EDA.R" script is informative but optional

### *** Question 1: characterize a typical day's rides from station to station ####

# set scaling factor: what is the duration, in days, of the data series?
time_diff <- max(data_clean$start_time) - min(data_clean$start_time)
span <- time_diff[[1]]

# randomly subsample the dataset to obtain an "average" day, at certain times
sample_day <- sample_frac(data_clean, size = 1/span)
sample_day_highlights <- sample_day %>% filter(hour(start_time) %in% c(5, 8, 11, 14, 17, 20)) # selected hrs

# select which subsample to use: all 24 hrs or just selected hrs
figure_data <- sample_day_highlights

# prepare map: add xlim & ylim to restrict to San Francisco only
g <- ggplot(figure_data) +  
  geom_segment(aes(x = start_station_longitude, y = start_station_latitude,
                   xend = end_station_longitude, yend = end_station_latitude),
               alpha = 0.1, show.legend = FALSE) +
  scale_size(range = c(.1, 5)) +
  xlim(c(-122.48, -122.37)) + 
  ylim(c(37.745, 37.81)) +
  theme_bw() +
  facet_wrap(.~ hour(start_time)) +
  coord_equal()

# map departures
g + geom_point(aes(x = start_station_longitude, y = start_station_latitude),
               color = "blue", alpha = 0.2, stat = "sum") +
  labs(title = "SF hourly bike traffic", x = "Longitude", y = "Latitude", 
       size = "departures \nper hour")

# map arrivals
g + geom_point(aes(x = end_station_longitude, y = end_station_latitude),
               color = "red", alpha = 0.2, stat = "sum") +
  labs(title = "SF hourly bike traffic", x = "Longitude", y = "Latitude", 
       size = "arrivals per\nhour")

### Figure 1: map arrivals and departures together
g + geom_point(aes(x = start_station_longitude, y = start_station_latitude),
               color = "blue", alpha = 0.2, stat = "sum") +
  geom_point(aes(x = end_station_longitude, y = end_station_latitude),
             color = "red", alpha = 0.15, stat = "sum") +
  labs(title = "SF hourly bike traffic", x = "Longitude", y = "Latitude", 
       size = "departures \nand arrivals \nper hour")

rm(figure_data, sample_day, sample_day_highlights)

#### *** Question 2: How does elevation influence rides? #####################################

### * proportional bike surplus by elevation ####

ggplot(station_stats, aes(x = elevation, y = prop_inflow, color = city)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()

g <- ggplot(station_stats, aes(x = elevation, y = prop_inflow, color = city, weight = departure_count, size = departure_count)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  # scale_x_log10() +
  theme_bw()

# facet out by city
g + geom_smooth(lty = 2, span = 1.5, se = FALSE, color = "black") +
  facet_wrap(. ~ city) +
  ggtitle("bike surplus proportion by elevation")

# facet by city and transit linkage
g + facet_wrap(. ~ is_transit + city) +
  ggtitle("bike surplus proportion by elevation; transit stations in bottom panels")


### * absolute bike surplus by elevation ####

g <- ggplot(station_stats, aes(x = elevation, y = net_change, color = city, 
                               weight = departure_count+arrival_count, size = departure_count+arrival_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  theme_bw()

# facet by city
g + geom_smooth(lty = 2, span = 1.5, se = FALSE, color = "black") +
  facet_wrap(. ~ city) +
  ggtitle("bike surplus (absolute) by elevation")

# facet by city & transit linkage
g + facet_wrap(. ~ is_transit + city) +
  ggtitle("bike surplus (absolute) by elevation; transit stations in bottom panels")

### * ride volume by elevation ####

g <- ggplot(station_stats, aes(x = elevation, y = arrival_count + departure_count, color = city)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  theme_bw()

# facet by city
g + geom_smooth(lty = 2, span = 1.5, se = FALSE, color = "black") +
  facet_wrap(. ~ city) +
  ggtitle("station use by elevation")

# facet by city & transit linkage
g + facet_wrap(. ~ is_transit + city) +
  ggtitle("station use by elevation; transit stations in bottom panels") +
  coord_cartesian(ylim = c(0, 130000))


### *** Preliminary statistical analysis: what explains net bike flow from station to station? #########

# do rides end lower than they started?
mean(data_clean$elev_change)
sd(data_clean$elev_change)
t.test(data_clean$elev_change, alternative = "less")
# confirmed: riders are significantly likely to end up lower than they started

data_clean %>% 
  mutate(descend = elev_change<0) %>% 
  summarise(desc_rides = sum(descend),
            tot_rides = nrow(data_clean),
            desc_fraction = desc_rides/tot_rides)

# response: prop_inflow
m1 <- lm(prop_inflow ~ is_transit * elevation * city, data = station_stats)
anova(m1)

m2 <- update(m1, .~. - is_transit:elevation:city)
anova(m1, m2)
anova(m2)
summary(m2)

# response: net_change
m3 <- lm(net_change ~ is_transit * elevation * city, data = station_stats)
anova(m3)
summary(m3)

# response: total ride volume (departures + arrivals)
m4 <- lm((departure_count+arrival_count) ~ is_transit * elevation * city, data = station_stats)
anova(m4)
summary(m4)

rm(m1, m2, m3, m4)

m5 <- lm(prop_inflow ~ elevation * city, data = station_stats)
anova(m5)
summary(m5)

m6 <- lm(net_change ~ elevation * city, data = station_stats)
anova(m6)
summary(m6)

lm((departure_count+arrival_count) ~ elevation * city, data = station_stats) %>% summary()
