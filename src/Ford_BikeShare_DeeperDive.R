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

library(raster)
sfn_dem <- raster("data/RGIS1_Data/sanfrancisconorth.dem")
sfn_dem %>% crs()
summary(sfn_dem, maxsamp = ncell(sfn_dem))
plot(sfn_dem)
sfn_dem_df <- as.data.frame(sfn_dem, xy = TRUE)
str(sfn_dem_df)
pryr::object_size(sfn_dem_df)

ggplot() +
  geom_raster(data = sfn_dem_df , aes(x = x, y = y, fill = sanfrancisconorth)) +
  scale_fill_viridis_c(option = "B") +
  coord_quickmap()

crs(sfn_dem)

library(sf)
sf_shp <- st_read("http://spatial.lib.berkeley.edu/public/ark28722-s7d02x/data.zip")
sf_shp <- st_read("data/BayArea/bayarea_general.shp")
bikeways <- st_read("data/SF_bikeways/bikeways.shp")
st_geometry_type(sf_shp)
st_crs(sf_shp)
st_bbox(sf_shp)
st_bbox(bikeways)
sf_shp
plot(sf_shp)

bikeways_simple <- st_simplify(bikeways)

plot(sfn_dem)
plot(sf_shp_crs$geometry, add=TRUE)
plot(bikeways_crs$geometry, col = "blue", add = TRUE)

library(rasterVis)
gplot(sfn_dem) +
  geom_raster(aes(fill = value)) +
  geom_sf(data = sf_shp, color = "black", fill = "NA") +
  # geom_sf(data = bikeways, color = "blue") +
  # coord_sf(xlim = c(-122.6, -122.3), ylim = c(37.6, 37.9), crs = the_crs) +
  theme_light()

crs(sfn_dem)
st_crs(bikeways_crs)
st_crs(sf_shp_crs)
the_crs <- crs(sfn_dem, asText = TRUE)

crs(sfn_dem)
sfn_dem_crs <- projectRaster(sfn_dem, crs = st_crs(bikeways), method = "ngb")
sfn_dem_crs_df <- as.data.frame(sfn_dem_crs, xy = TRUE)
bikeways_crs <- st_transform(bikeways, crs = st_crs(sf_shp))
sf_shp_crs <- st_transform(sf_shp, crs = st_crs(bikeways))

#library(rasterVis)
library(viridis)
# library(rvest)
library(tmap)

tm_shape(sfn_dem) + tm_raster() + 
  tm_shape(sf_shp_crs) + tm_borders() + 
  tm_shape(bikeways_crs) + tm_lines(col = "blue", alpha = 0.3)

roi <- st_bbox(c(xmin = -122.5, xmax = -122.37,
          ymin = 37.75, ymax = 37.83),
        crs = 4326) %>%
  st_as_sfc() %>% 
  st_transform(crs = the_crs) 

tm_shape(sfn_dem, bbox = roi) + tm_raster() + 
  tm_shape(sf_shp_crs) + tm_borders() + 
  tm_shape(bikeways_crs) + tm_lines(col = "blue", alpha = 0.3)

sample_day_sf <- st_as_sf(sample_day, coords = c("start_station_longitude", "start_station_latitude"), crs = crs(bikeways))

station_launch <- data_clean %>% 
  group_by(start_station_name) %>% 
  top_n(n= -1, wt = start_time) %>% 
  ungroup() %>% 
  dplyr::select(c(start_station_id, start_time)) %>%
  arrange(start_station_id)

tm_shape(sfn_dem, bbox = roi) + tm_raster() + 
  tm_shape(sf_shp_crs) + tm_borders() + 
  tm_shape(bikeways_crs) + tm_lines(col = "blue") +
  tm_shape(sample_day_sf) + tm_dots()

sf_shp %>% bind_cols(start_time = 1)

ggplot(sample_day_sf) +
  # geom_raster(data = sfn_dem_crs_df, aes(x = x, y = y, fill = sanfrancisconorth)) +
  geom_sf(data = sf_shp_crs, color = "black", fill = "NA") +
  geom_sf(data = bikeways, color = "blue") +
  geom_segment(data = sample_day, aes(x = start_station_longitude, y = start_station_latitude,
                  xend = end_station_longitude, yend = end_station_latitude),
              alpha = 0.1, show.legend = FALSE) +
  geom_point(data = sample_day, aes(x = start_station_longitude, y = start_station_latitude),
                           color = "blue", alpha = 0.2, stat = "sum") +
  geom_point(data = sample_day, aes(x = end_station_longitude, y = end_station_latitude),
            color = "red", alpha = 0.15, stat = "sum") +
  scale_size(range = c(.1, 5)) +
  xlim(c(-122.48, -122.37)) + 
  ylim(c(37.74, 37.81)) +
  # facet_wrap(~ hour(start_time)) +
  theme_light() +
  # geom_sf(data = sample_day_sf) +
  labs(title = "SF hourly bike traffic", x = "Longitude", y = "Latitude", 
       size = "Departures \nand arrivals \nper hour")


plot(st_geometry(sample_day_sf))

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
