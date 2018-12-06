### Dependencies ###########################################

library(data.table)
library(magrittr)
library(tidyverse)
library(lubridate)
library(sp)
library(elevatr)
library(hexbin)
library(viridis)
library(corrplot)

### Import Data ############################################

# load 2017 data
path <- file.path("data", "2017-fordgobike-tripdata.csv")
data2017 <- data.table::fread(path) %>% as.tbl()

# 2018 data are spread across 9 files, so list them out
file_names <- list.files(path = "./data", pattern = "*2018")
file_paths <- paste("./data/", file_names, sep = "")

# load 2018 data into a list
data2018 <- map(file_paths, data.table::fread) %>% map(as.tbl)


##### Wrangling, pt 1 ##########################################

#### * goal 1: combine all data into a single data frame ####

# collapse all 2018 data into a single data frame
data2018 <- bind_rows(data2018)
# fail - column type mismatch for start_station_id, possibly others


# for a list of data tables ("list"), check that a given attribute ("attrib") of the columns 
# is consistent across all items in the list
compare_cols <- function(list, attrib) {
  for (i in 1:(length(list)-1)) print(identical(map(list[[i]], attrib), map(list[[i+1]], attrib)))
}

# do all column names match across each month?
compare_cols(data2018, names)

# do all column classes match across each month?
compare_cols(data2018, class)
# no! changes from mo. 5 to mo. 6

early_cols <- data2018[[5]] %>% sapply(class) %>% unname() # get col classes of month 5
late_cols <- data2018[[6]] %>% sapply(class) %>% unname() # get col classes of month 6

which(early_cols != late_cols)
# cols 4 & 8 are different: start_station_id and end_station_id
rm(early_cols, late_cols)

str(data2018[[5]])
str(data2018[[6]])
# station IDs switch to character starting in June 2018

# make two internally-consistent subsets of the data
data2018a <- bind_rows(data2018[1:5])
data2018b <- bind_rows(data2018[-c(1:5)])
# success

colnames(data2018a)

# change station ID columns to char
data2018a %<>%
  mutate(start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))
# now all 2018 data should have consistent formatting

# bind all 2018 data together, overwriting initial list
data2018 <- bind_rows(data2018a, data2018b)
# success

## now to combine 2017 and 2018
str(data2017)
str(data2018)

cols2017 <- data2017 %>% sapply(class) %>% unname() # col classes
cols2018 <- data2018 %>% sapply(class) %>% unname() # col classes

which(cols2017 != cols2018)
# cols 4 & 8 are different, and col 16 is new for 2018

# change station ID columns to char
data2017 %<>%
  mutate(start_station_id = as.character(start_station_id),
         end_station_id = as.character(end_station_id))

# combine years
data <- data2017 %>% bind_rows(data2018)

# cleanup
rm(cols2017, cols2018, data2017, data2018, data2018a, data2018b, path, file_names, file_paths)


#### * goal 2: set the classes of variables ####

#              * and a few other useful tweaks

str(data)

# convert column types
data <- data %>%
  mutate(member_gender = as.factor(member_gender),
         user_type = as.factor(user_type),
         bike_share_for_all_trip = as.factor(bike_share_for_all_trip),
         start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time),
         bike_id = as.character(bike_id))

# create new variables: weekday (to split out the day of the week) and hour (ride start hour)
data <- data %>%
  mutate(weekday = factor(wday(start_time, label = TRUE), ordered = FALSE),
         hour = hour(start_time))

# simplify weekday even further, to binary is_weekend
data <- data %>%
  mutate(is_weekend = ifelse(weekday %in% c("Sat", "Sun"), "wk_end", "wk_day"),
         is_weekend = as.factor(is_weekend))

# earlier, I converted station IDs to character. What do they look like?
data$start_station_id %>% unique() %>% sort()

# turn them back into numeric, converting "NULL" values into NAs
data <- data %>%
  mutate(start_station_id = as.numeric(start_station_id),
         end_station_id = as.numeric(end_station_id))

summary(data$start_station_id)

# convert "NULL" station names into NAs
data <- data %>%
  mutate(start_station_name = replace(start_station_name, which(start_station_name == "NULL"), NA),
         end_station_name = replace(end_station_name, which(end_station_name == "NULL"), NA))


#### * goal 3: address NA stations #####

# how many unknown stations?
sum(is.na(data$start_station_id))
sum(is.na(data$start_station_name))

data %>%
  filter(is.na(start_station_name)) %>%
  group_by(start_station_id) %>%
  summarise(count = n())
# all unnamed stations lack IDs

data %>%
  filter(is.na(start_station_id)) %>%
  group_by(start_station_name) %>%
  summarise(count = n())
# all no-ID stations also lack names - and it's the same number, so 1:1 match

## short-term hack: delete all NA stations, since they are a small fraction of total

# what fraction?
sum(is.na(data$start_station_name)) / nrow(data)
# approx. 0.5%

# further, all stations starting at NA also end at NA:
data %>%
  filter(is.na(start_station_name)) %>%
  group_by(end_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# do the deed
data_clean <- data %>%
  filter(!is.na(start_station_name))
# now, all rides are to/from named stations

summary(data_clean)


##### Wrangling, pt 2: ##########################################

#### Summarize arrival/departure stats by station ####

departures <- data_clean %>%
  group_by(start_station_name) %>%
  summarise(departure_count = n()) %>%
  rename(station_name = start_station_name)

arrivals <- data_clean %>%
  group_by(end_station_name) %>%
  summarise(arrival_count = n()) %>%
  rename(station_name = end_station_name)

station_stats <- departures

# expand station_stats w/ arrivals, net change, proportional change
station_stats <- station_stats %>% 
  left_join(arrivals) %>%
  mutate(net_change = arrival_count - departure_count,
         prop_inflow = net_change/departure_count)

rm(arrivals, departures)


# make a table of station info (name, id, lat, long)
station_info <- data_clean %>%
  group_by(start_station_id, 
           start_station_name, 
           start_station_latitude, 
           start_station_longitude) %>% # this is the most specific grouping of stations by location
  summarize(count = n()) %>%
  group_by(start_station_name) %>%
  rename(station_name = start_station_name,
         station_id = start_station_id,
         station_latitude = start_station_latitude,
         station_longitude = start_station_longitude) 

station_info

length(unique(station_info$station_id))
length(unique(station_info$station_name))
length(unique(data_clean$start_station_latitude))
length(unique(data_clean$start_station_longitude))

# 356 unique lat/long combos vs 349 unique latitudes vs 348 unique longitudes vs 346 unique station names vs 331 unique station IDs

# find repeat stations
station_count <- station_info %>%
  summarize(station_count = n()) %>%
  arrange(desc(station_count))

head(station_count, n = 10)

id_count <- station_info %>%
  group_by(station_id) %>%
  summarise(n_per_id = n()) %>%
  arrange(desc(n_per_id))

head(id_count, n = 20)

# 19 station IDs are associated with >1 lat/long location. 9 station names are associated with >1 lat/long locations. 
# So these are likely different dock locations at the same general location.
# The docks might be operated simultaneously, or sequentially (repositioned over time). Can check this later if necessary.

# join station stats
station_stats <- station_stats %>%
  full_join(station_info) %>%
  full_join(id_count) %>%
  full_join(station_count) %>%
  select(station_id, n_per_id, 
         station_name, 
         station_count, 
         station_latitude,
         station_longitude, 
         departure_count:prop_inflow) %>%
  arrange(desc(n_per_id), station_id)

rm(id_count, station_info, station_count)

station_stats %>% print(n = 20)

# let's check further: why do some stations seem to repeat silently, i.e. identical name but appear 2+ times?
temp <- station_stats %>%
  arrange(desc(station_count)) %>%
  filter(station_count > 1)
as.data.frame(temp) # this hack prints more decimal places than tbl's default does

# Shattuck at Hearst has very similar - but not identical - lat/long coordinates. What about the rest?
i <- seq(from = 4, to = 18, by = 2)
temp$station_latitude[i] - temp$station_latitude[i+1]
temp$station_longitude[i] - temp$station_longitude[i+1]

identical(temp$station_longitude[2], temp$station_longitude[3])
identical(temp$station_longitude[10], temp$station_longitude[11])

# one station has identical longitudes but differing latitudes for its docks
# this accounts for discrepancy btw the number of unique latitudes vs longitudes

# so the "silent" repeat stations (i.e., no name change but different lat-long) are:
# Shattuck Ave at Hearst Ave;
# 37th at West
# S. 4th St at San Carlos St;
# Doyle St at 59th St;
# North Berkeley BART Station; and
# Tamien Station

# All have identical arrival and departure counts, since I initially grouped them by name. 
# So we should pick a single set of lat-long coordinates for each.
rm(i, temp)

# check the duplicate stations
station_stats %>% arrange(desc(station_count)) %>% print(n = 20)

duplicates <- which(station_stats$station_count > 1)
station_stats[duplicates,] # yep, checks out

extras <- duplicates[c(2,4,6,7,9,11,13,15,17,19)]

# delete the extras
station_stats <- station_stats[-extras,]
rm(duplicates, extras)

# station_count now superfluous, and n_per_id outdated
station_stats <- station_stats %>%
  select(-station_count, -n_per_id)

station_stats %>% print(n = 10)

# recalculate station ID counts
new_id_counts <- station_stats %>%
  group_by(station_id) %>%
  summarise(n_per_id = n()) %>%
  arrange(desc(n_per_id))

# add in the new station ID counts
station_stats <- station_stats %>%
  full_join(new_id_counts) %>%
  select(station_id, n_per_id, station_name:prop_inflow) %>%
  arrange(desc(n_per_id))
rm(new_id_counts)

station_stats %>% filter(n_per_id > 1) %>% print(n = "all")
# here we can see sometimes slightly different names, and therefore station use stats are separate. To lump by ID, would need to recalculate.

# Overall, station ID is associated w/ a general location,
# and different names (& data) are associated w/ slightly different positions of that station
# so, either do the analysis by a single ID (and average the lat/long, and re-calculate ridership by station ID)
# or keep stations separate by name for greater geographic precision.


#### * goal 1: Label stations by city #################################

# cluster analysis of stations by lat-long, just for practice

station_locations <- station_stats %>% select(station_latitude, station_longitude)
dist_stations <- dist(station_locations, method = "euclidean")
hc_stations <- hclust(dist_stations, method = "complete")

# extract clusters
cluster_assigments <- cutree(hc_stations, k = 3)
stations_clustered <- mutate(station_locations, cluster = cluster_assigments)
# now plot, color by assignment
ggplot(stations_clustered, aes(x = station_longitude, y = station_latitude, color = factor(cluster))) +
  geom_point()

# label stations by city!
stations_clustered <- stations_clustered %>%
  mutate(cluster = recode(cluster, '1' = "EastBay",
                          '2' = "SanJose",
                          '3' = "SanFrancisco")) %>%
  rename(city = cluster)

# should be clear boundaries by longitude, according to the plot
stations_clustered %>%
  group_by(city) %>%
  summarise(min_long = min(station_longitude),
            max_long = max(station_longitude)) %>%
  as.data.frame()
# yes: divide along latitude: SJ > -122, -122 > EastBay > -122.3, SF < -122.3

label_city_by_long <- function(long) {
  if_else (long > -122, "SanJose",
           if_else (long < -122.3, "SanFrancisco",
                    "EastBay"))
}

# add city column to station_stats
station_stats <- station_stats %>%
  mutate(city = label_city_by_long(station_longitude)) %>%
  select(station_id:station_longitude, city, departure_count:prop_inflow)

ggplot(station_stats, aes(x = station_longitude, y = station_latitude, color = city)) + geom_point()
# success

rm(station_locations, hc_stations, stations_clustered, cluster_assigments, dist_stations)

#### * goal 2: Add transit status of stations to station_stats ####################################################

# for now, search for hits to key search strings
transit <- unique(c(grep("BART", station_stats$station_name, value = TRUE),
                    grep("train", station_stats$station_name, ignore.case = TRUE, value = TRUE),
                    grep("station", station_stats$station_name, ignore.case = TRUE, value = TRUE),
                    grep("ferry", station_stats$station_name, ignore.case = TRUE, value = TRUE)))[-22] # ignore Outside Lands temp station

station_stats <- station_stats %>%
  mutate(is_transit = station_name %in% transit)

rm(transit)


#### * goal 3: Add elevation to station_stats #####################################################

## elevation chunk start ###
# coord_df <- station_stats %>% select(station_longitude, station_latitude)
# prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# 
# # Create SpatialPoints
# sp <- SpatialPoints(coord_df, proj4string = CRS(prj_dd))
# 
# # Create SpatialPointsDataFrame
# spdf <- SpatialPointsDataFrame(sp, proj4string = CRS(prj_dd), data = station_stats)
# 
# # use USGS Elevation Point Query Service (slow, USA only)
# spdf_elev_epqs <- get_elev_point(spdf, src = "epqs")
# spdf_elev_epqs # this took a while, so export the results for future use
# 
# readr::write_csv(as.data.frame(spdf_elev_epqs), path = file.path("results", "station_elevation_df.csv"))
# rm(coord_df, sp, spdf, spdf_elev_epqs, prj_dd)
# # elevation chunk end ###

# import saved elevations
spdf_elev_epqs <- readr::read_csv(file.path("results", "station_elevation_df.csv"))

## add elevation to station info

# make sure rows are aligned
spdf_elev_epqs %<>% arrange(station_id, station_name)
station_stats %<>% arrange(station_id, station_name)

# add it in
station_stats$elevation <- spdf_elev_epqs$elevation

rm(spdf_elev_epqs)

##### Reincorporate select station stats into primary data frame ####################################

#### * add in elevation & transit info to start & end stations of data for data_clean ####
data_clean <- data_clean %>% 
  arrange(start_time) %>% 
  left_join(station_stats, by = c("start_station_name" = "station_name")) %>%
  mutate(start_station_city = city,
         start_station_is_transit = is_transit,
         start_station_elevation = elevation) %>%
  select(start_time:start_station_longitude, start_station_city:start_station_elevation,
         end_station_id:end_station_longitude, duration_sec, bike_id:is_weekend) %>%
  left_join(station_stats, by = c("end_station_name" = "station_name")) %>%
  mutate(end_station_city = city,
         end_station_is_transit = is_transit,
         end_station_elevation = elevation) %>%
  select(start_time, start_station_id:start_station_elevation,
         end_time, end_station_id:end_station_longitude, end_station_city:end_station_elevation,
         duration_sec:is_weekend) %>%
  mutate(elev_change = end_station_elevation - start_station_elevation)

