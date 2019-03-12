library(hexbin)
library(viridis)

data <- data.table::fread(file.path("results", "data.csv")) %>% 
  as.tbl()%>%
  mutate(member_gender = as.factor(member_gender),
         user_type = as.factor(user_type),
         bike_share_for_all_trip = as.factor(bike_share_for_all_trip),
         start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time),
         bike_id = as.character(bike_id),
         weekday = as.factor(weekday),
         is_weekend = as.factor(is_weekend))

data_clean <- data.table::fread(file.path("results", "data_clean.csv")) %>% 
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

##### EDA, part 1 ######################################################

data %>%
  # select_if(is.numeric) %>%
  skimr::skim()

data %>%
  map_df(~n_distinct(.)) %>%
  gather(variable, num_distinct)

# data %>%
#   count(start_station_name, sort = TRUE)

#### *** univariate exploration ####

# bar plots of categorical (factor) columns
data %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

# numeric columns
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_freqpoly()

### * User profiles ####

# ridership: gender
plot(data$member_gender)

# ridership: age
hist(2019-data$member_birth_year, main = "rider age")

# one-off ride or subscriber ride?
plot(data$user_type)

# subsidized subscriptions
plot(data$bike_share_for_all_trip)


### * Station profiles ####

# which stations are popular? bin by ID
ggplot(data, aes(x = start_station_id)) + geom_bar()
ggplot(data, aes(x = end_station_id)) + geom_bar()

station_stats %>%
  arrange(net_change) %>%
  select(station_name, city, departure_count, arrival_count, net_change, prop_inflow, is_transit, elevation)
# everyone's getting out of Bancroft @ College (Berkeley near the stadium)
# and presumably going downhill?
# ditto McAllister St at Baker St, SF - I should find all station elevations
# and everyone's trying to catch the train (Caltrain or BART)

### add in subscriber info ####
data_clean %>%
  group_by(start_station_city, start_station_id) %>%
  filter(!is.na(bike_share_for_all_trip)) %>%
  summarise(subsidized_frac = sum(as.numeric(bike_share_for_all_trip)-1)/n()) %>%
  # filter(start_station_city == "SanFrancisco") %>%
  arrange(desc(subsidized_frac))

# stations w/ most subsidized users in SF:
station_stats %>% 
  filter(station_id %in% c(34, 33, 358, 76, 10, 41, 62, 349)) %>% 
  select(station_name, station_id, arrival_count, departure_count) %>% 
  mutate(volume = arrival_count + departure_count) %>% 
  select(-arrival_count, -departure_count)

station_stats %>% 
  filter(station_id %in% c(280, 296, 317, 357)) %>% 
  select(station_name, station_id, arrival_count, departure_count) %>% 
  mutate(volume = arrival_count + departure_count) %>% 
  select(-arrival_count, -departure_count)

station_stats %>% 
  filter(city == "SanFrancisco") %>% 
  select(station_name, elevation, station_id, arrival_count, departure_count, prop_inflow) %>% 
  mutate(volume = arrival_count + departure_count) %>% 
  select(-arrival_count, -departure_count) %>%
  arrange(desc(elevation))

### * Ride profiles ####

# how long are rides? do a histogram w/ varying y-limits
g <- ggplot(data, aes(x = duration_sec/60)) + 
  xlab("duration (min)") +
  geom_histogram(bins = 50)

g
g + coord_cartesian(ylim = c(0,8000))

# log-transform the duration
g + scale_x_log10(breaks = c(1, 10, 30, 100, 1000))

# counts -> Poisson

### When did people start rides?

# seasonality
hist(data$start_time, breaks = "months")
# increase over time, dampened in winter months

# daily distribution
hist(hour(data$start_time))
# rides all 24 hrs of the day, peaks for rush hour


#### *** bivariate/multivariate exploration ####

### * correlation plot ####
M1 <- data_clean %>%
  mutate(user_type = as.numeric(user_type),
         is_weekend = as.numeric(is_weekend)) %>%
  keep(is.numeric) %>%
  filter(!is.na(member_birth_year)) %>%
  cor()

corrplot(M1, method = "circle", order = "hclust")
# not super informative...

M2 <- data_clean %>%
  select(-start_station_id, -end_station_id, -member_birth_year, -start_station_name, -end_station_name) %>%
  mutate(start_city_number = order(as.factor(start_station_city))) %>%
  select(-start_station_city, -end_station_city, -bike_share_for_all_trip) %>%
  mutate_all(as.numeric) %>%
  # filter(!is.na(member_birth_year)) %>% 
  cor()

corrplot(M2, method = "circle", order = "hclust")
# ok...

rm(M1, M2)

### * riders ####
# age by gender
ggplot(data, aes(x = 2019-member_birth_year, color = member_gender)) +
  geom_freqpoly() +
  coord_cartesian(xlim = c(0,100))

# gender, by subscription status
ggplot(data, aes(x = user_type, fill = member_gender)) +
  geom_bar(position = "fill")

# restrict to users who provided all data
ggplot(na.omit(data), aes(x = user_type, fill = member_gender)) +
  geom_bar(position = "fill")

# subsidized subscriptions by city
ggplot(na.omit(data_clean), aes(x = start_station_city, fill = bike_share_for_all_trip)) +
  geom_bar(position = "fill")

# subsidized subscriptions by gender
# ggplot(data, aes(x = bike_share_for_all_trip, fill = member_gender)) +
#   geom_bar(position = "fill")
# 
## no NAs
ggplot(na.omit(data), aes(x = bike_share_for_all_trip, fill = member_gender)) +
  geom_bar(position = "fill")

ggplot(na.omit(data), aes(x = member_gender, fill = bike_share_for_all_trip)) +
  geom_bar(position = "fill")

# subsidized use by city and gender
ggplot(na.omit(data_clean), aes(x = bike_share_for_all_trip, fill = member_gender)) +
  geom_bar(position = "fill") + facet_wrap(.~start_station_city)

### usage by subscription vs weekend
ggplot(data_clean, aes(x = is_weekend, fill = user_type, color = bike_share_for_all_trip)) + 
  geom_bar(position = "fill")


### * stations & rides ####
# do all stations connect to all other stations? plot start vs end station ID
ggplot(sample_n(data_clean, size = 20000), 
       aes(x = start_station_id, y = end_station_id, color = start_station_city)) +
  geom_point(alpha = 0.5)
# 3 cities in this dataset - hence 3 apparent main clusters - return to this later

# hex bin the rides by duration and start time
ggplot(data, aes(x = start_time, y = duration_sec/60)) +
  geom_hex() +
  scale_y_log10(breaks = c(1, 10, 30, 100, 1000)) +
  scale_fill_viridis() +
  ylab("duration of ride (min) - log scale") +
  xlab("ride start date") +
  theme_bw()

# rides by day of week
ggplot(data, aes(x = weekday)) +
  geom_bar() +
  theme_bw()

# ride duration by day of week
ggplot(data, aes(x = duration_sec/60, color = weekday)) +
  geom_density() +
  scale_x_log10(breaks = c(1, 10, 30, 100, 1000)) +
  xlab("ride duration in minutes") +
  coord_flip() +
  theme_bw()
# weekend rides tend to be longer

# does ride length change through the day?
ggplot(sample_n(data, size = 100000), 
       aes(x = hour(start_time), y = duration_sec/60, color = is_weekend)) +
  geom_density2d() +
  scale_y_log10() +
  xlab("ride start time (hr)") +
  ylab("ride duration in minutes") +
  theme_bw()
# no obvious changes in ride length thru the day, only obvious difference in weekday vs weekend

# # what time of day do rides happen? by day of week
# ggplot(data, aes(hour(start_time), stat(count))) +
#   geom_density(adjust = 2.5) + 
#   facet_wrap(. ~ weekday) +
#   theme_bw()

# what time of day do rides happen? weekday vs weekend
ggplot(data, aes(x = hour(start_time), fill = is_weekend, color = is_weekend)) +
  geom_density(adjust = 2.5, alpha = 0.3) + 
  theme_bw()

### ride duration by time of day & weekday vs weekend
g <- ggplot(data, aes(x = duration_sec/60)) +
  scale_x_log10(breaks = c(1, 10, 30, 100, 1000)) +
  coord_flip() +
  xlab("ride duration in minutes") +
  theme_bw()

# # faceted by start time
# g + geom_density(aes(color = is_weekend)) +
#   facet_wrap(.~ hour(start_time)) +
#   ggtitle("ride length by time of day")
# 
# # same as above, but faceted by weekday vs weekend
# g + geom_density(aes(color = factor(hour(start_time)))) +
#   facet_wrap(.~ is_weekend) +
#   ggtitle("facets: weekend?")
# 
# # ride duration by weekday vs weekend, faceted by month
# g + geom_density(aes(color = is_weekend)) +
#   facet_wrap(.~ month(start_time, label = TRUE, abbr = TRUE))

# ride duration by month, faceted by weekday vs weekend
g + geom_density(aes(color = factor(month(start_time, label = TRUE, abbr = TRUE)))) +
  facet_wrap(.~ is_weekend) +
  ggtitle("facets: weekend?")
# slightly longer weekend rides in summer than in winter
# weekday rides mostly unchanged, maybe marginally thicker upper tail in summer

### * map in physical space ####

# where are the rides originating? bin lat/long coordinates of start_station
  ggplot(data) +
  geom_hex(aes(x = start_station_longitude, y = start_station_latitude)) +
  scale_fill_viridis()
# this is strange - almost all observations are clustered in small lat/long range
# but a handful of very distant outliers - note the scale!!

#### *** Making sense of names, IDs, and lat/long coordinates #################################

# what's with station_id vs station_name vs station_lat/long? do they all match as expected?
# count unique instances of each:
n_distinct(data$start_station_id)
n_distinct(data$end_station_id)
n_distinct(data$start_station_name)
n_distinct(data$end_station_name)
n_distinct(data$start_station_longitude)
n_distinct(data$start_station_latitude)
n_distinct(data$end_station_longitude)
n_distinct(data$end_station_latitude)

# what are the lat-long ranges?
range(data$start_station_longitude)
range(data$end_station_longitude)
range(data$start_station_latitude)
range(data$end_station_latitude)

# same start and end latitude ranges, but these are very big ranges
# from Google Maps, here are coordinates for 3 cities:
# SF: 37.77 N, 122.42 W
# Oakland: 37.80 N, 122.27 W
# SJ: 37.33 N, 121.89 W

data %>%
  filter(end_station_longitude == max(end_station_longitude)) %>%
  select(end_station_name)
# stations at the maximum longitude are unnamed

# sort start stations by latitude
data %>%
  group_by(start_station_name, start_station_latitude) %>%
  summarise(n = n()) %>%
  arrange(desc(start_station_latitude))

# sort start stations by longtitude 
data %>%
  group_by(start_station_name, start_station_longitude) %>%
  summarise(n = n()) %>%
  arrange(desc(start_station_longitude))

# sort start stations by volume
data %>%
  group_by(start_station_name) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  print(n = "all")

unique(data$start_station_name)
sum(is.na(data$start_station_name))
# number of NA names matches number of NA stations

# # a few weird unnamed starting stations: Montreal, Minneapolis, Brooklyn
# # (do a google map search by coordinates for the top four unusual coordinates)
# data %>%
#   group_by(start_station_id, 
#            start_station_name, 
#            start_station_latitude, 
#            start_station_longitude) %>%
#   summarise(n = n()) %>%
#   filter(is.na(start_station_name)) %>%
#   arrange(desc(start_station_latitude))%>%
#   as.data.frame() %>%
#   head(n = 10)
# 
# # same for ending stations
# data %>%
#   group_by(end_station_id, 
#            end_station_name, 
#            end_station_latitude, 
#            end_station_longitude) %>%
#   summarise(n = n()) %>%
#   filter(is.na(end_station_name)) %>%
#   arrange(desc(end_station_latitude)) %>%
#   as.data.frame() %>%
#   head(n = 10)
# 
# # check out high-latitude trips
# weird_station_data <- data %>%
#   filter(end_station_latitude > 40)
# 
# weird_station_data
# 
# # start & end stations roughly similar?
# weird_station_data$end_station_latitude - weird_station_data$start_station_latitude
# # yes
# 
# # remove this small subset of trips:
data_BayArea <- data %>% filter(end_station_latitude < 40)
# rm(weird_station_data)
# 
# data$end_station_longitude %>% range()
# data$start_station_longitude %>% range()
# data$end_station_latitude %>% range()
# data$start_station_latitude %>% range()
# # hooray, all station locations are now in the Bay Area

# there are still unknown stations
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

#### * further investigation of NA rides/stations ######################

na_trips <- data %>% filter(is.na(start_station_id), start_station_longitude < -110)

# prepare new map
g <- ggplot(station_stats, aes(x = station_longitude, y = station_latitude, color = city)) + 
  geom_point() + 
  xlim(c(-122.5, -121.8)) + 
  ylim(c(37.2, 37.85)) + 
  coord_fixed() 

# plot NA departure stations in gray
g + geom_point(data = na_trips, aes(x = start_station_longitude, y = start_station_latitude), color = "black", alpha = 0.4)

# plot NA arrival stations in gray
g + geom_point(data = na_trips, aes(x = end_station_longitude, y = end_station_latitude), color = "black", alpha = 0.4)

# some variation by start vs. end station, but those NAs still look fishy: 
# A very regular grid that seems like it might extend into the bay --
# maybe mislabeled somehow?

# which trips in the entire data set are NA trips?
hist(which(is.na(data$start_station_id)))
# later trips, past the millionth one

# are the NA trips at regular intervals?
hist(diff(which(is.na(data$start_station_id))), ylim = c(0, 10))
# a bunch in close order, but sometimes lots of other trips between them

# when are the NA trips?
ggplot(na_trips, aes(x = start_time)) + geom_histogram()
range(na_trips$start_time)
ggplot(na_trips, aes(x = month(start_time))) + geom_bar()
# all in 2018; a bunch in June through August, fewer in September/October

# do NA trips follow the standard timing pattern?
ggplot(na_trips, aes(x = hour(start_time))) + geom_histogram(bins = 24)
ggplot(na_trips, aes(x = duration_sec/60)) + geom_histogram() + scale_x_log10(breaks = c(1, 10, 30, 100, 1000))
# sort-of... but fewer morning trips, more evening and mid-day trips, and more trips over 30 min!

# what is the ridership like?
ggplot(na_trips, aes(x = user_type, fill = bike_share_for_all_trip)) + geom_bar()
ggplot(na_trips, aes(x = member_gender)) + geom_bar()
# roughly comparable to the usual breakdown of users

# which bikes are involved?
data %>% 
  filter(is.na(start_station_id)) %>% 
  group_by(bike_id) %>% 
  summarise(count = n()) %>% 
  arrange(bike_id) %>% 
  print(n = "all")

na_trip_bikes <- data %>% 
  filter(is.na(start_station_id)) %>% 
  select(bike_id) %>% 
  arrange(bike_id) %>% 
  unique() %>% 
  unlist()

hist(as.numeric(na_trip_bikes))

# do bikes w/ NA rides ever have properly tagged stations?
data_clean %>% filter(bike_id %in% na_trip_bikes)
data %>% filter(bike_id %in% na_trip_bikes)
# NO, they do not

# so, a certain subset of 200 bikes, the first of which was introduced in late June 2018 (ID #s in the 3k+ range),
# are associated with missing station IDs and names, and possibly spurrious lat/long coordinates
# however, there is no reason to presume the ride start and stop times are incorrect??

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

get_min_dist <- function(x0, x1, y0, y1, latlong = FALSE) {
  if(latlong == FALSE) {
    a = x1-x0
    b = y1-y0
  } else{
    mean_y = (y0 + y1) / 2
    a = (x1 - x0) * cos(deg2rad(mean_y)) * 69.172 # 69.172 miles per longitude degree @ equator
    b = (y1 - y0) * 69 # 69 miles per latitude degree
  }
  c = sqrt(a^2 + b^2)
  return(c)
}

data_BayArea <- data_BayArea %>% mutate(trip_min_dist = get_min_dist(x0 = start_station_longitude, x1 = end_station_longitude,
                                                     y0 = start_station_latitude, y1 = end_station_latitude, latlong = TRUE),
                        trip_min_avg_speed = trip_min_dist / duration_sec * 60 * 60, # convert to mph
                        na_trip = is.na(start_station_id)) 

data_BayArea %>%
  group_by(na_trip) %>%
  summarise(n_trips = n(),
            avg_min_dist = mean(trip_min_dist),
            avg_mins = mean(duration_sec) / 60,
            avg_min_speed = mean(trip_min_avg_speed))

rm(na_trips)

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

summary(data_clean)

n_distinct(data_clean$start_station_id)
n_distinct(data_clean$end_station_id)
# 334 unique station IDs

n_distinct(data_clean$start_station_name)
n_distinct(data_clean$end_station_name)
# 351 unique station names

n_distinct(data_clean$start_station_latitude)
n_distinct(data_clean$end_station_latitude)
# 354 unique station latitudes

n_distinct(data_clean$start_station_longitude)
n_distinct(data_clean$end_station_longitude)
# 353 unique station longitudes

# do some stations have multiple locations?
data_clean %>%
  group_by(start_station_id,
           start_station_name,
           start_station_latitude, 
           start_station_longitude) %>%
  summarise(count = n()) %>%
  group_by(start_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# YES, 4 double locations among the named/ID'd stations:
# Doyle St at 59th St
# North Berkeley BART Station
# S. 4th St at San Carlos St
# Shattuck Ave at Hearst Ave

# same for the end stations

# other redundancies?
sort(unique(data_clean$start_station_name))
# some stations w/ similar names

index <- grep(pattern = "Station 2", x = unique(data_clean$start_station_name))
sort(unique(data_clean$start_station_name)[index])
# "16th St Mission BART" vs "16th St Mission BART Station 2"
# "San Francisco Caltrain (Townsend St at 4th St)" vs "San Francisco Caltrain Station 2  (Townsend St at 4th St)"
# "2nd St at Townsend St" vs "2nd St at Townsend St - Coming Soon"

# temporary locations:
index <- grep(pattern = "Temporary", x = unique(data_clean$start_station_name))
sort(unique(data_clean$start_station_name)[index])
# "4th Ave at E 12th St (Temporary Location)"
# "6th Ave at E 12th St (Temporary Location)"
# "6th St at San Fernando St (Temporary Location)"
# "Colin P Kelly Jr St at Townsend St (Temporary Site)"
# "Miles Ave at Cavour St (Temporary Location)"
# "MLK Jr Way at 36th St (Temporary Location)"
# "Outside Lands 2018 (Temporary Station)"
# "Potrero Ave at 15th St (Temporary Location)"
# "Webster St at MacArthur Blvd (Temporary Location)"
# "William St at 4th St (Temporary Location)"
rm(index)

ggplot(data = sample_frac(data_BayArea, size = 0.1), 
       aes(x = start_station_longitude, y = start_station_latitude)) + 
  geom_point() + 
  coord_fixed()

data_BayArea %>% 
  filter(is.na(start_station_name)) %>% 
  summarise(max_lat = max(start_station_latitude), 
            min_lat = min(start_station_latitude), 
            max_long = max(start_station_longitude), 
            min_long = min(start_station_longitude)) %>% 
  as.data.frame()

data_clean %>% 
  filter(start_station_city == "SanJose") %>% 
  summarise(max_lat = max(start_station_latitude), 
            min_lat = min(start_station_latitude), 
            max_long = max(start_station_longitude), 
            min_long = min(start_station_longitude)) %>% 
  as.data.frame()


##### EDA, part 2: preliminary maps ######################################################

### * bin trips in space ####

# set theme for spatial plotting
g <- ggplot(data_clean) +
  scale_fill_viridis() +
  theme_bw()

# where are the rides originating? bin lat/long coordinates of start_station
g + geom_hex(aes(x = start_station_longitude, y = start_station_latitude))
# much cleaner now! upper clusters are SFO and OAK, lower cluster is SJC!

# where do the rides end? lat/long of end_station
g + geom_hex(aes(x = end_station_longitude, y = end_station_latitude))

# most popular departure stations
data_clean %>%
  group_by(start_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n = 20) %>%
  ggplot(aes(x = fct_reorder(as.factor(start_station_name), count), y = count)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

# most popular arrival stations
data_clean %>%
  group_by(end_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))%>%
  head(n = 20) %>%
  ggplot(aes(x = fct_reorder(as.factor(end_station_name), count), y = count)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

# what are the top 20 most popular trips? include rank of a given trip for its respsective start station
data_clean %>%
  group_by(start_station_name, end_station_name) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(-n)) %>%
  filter(rank < 5) %>%
  arrange(desc(n)) %>%
  head(n = 20)

# so is everyone converging on Embarcadero & the Ferry Building?


##### EDA, part 3 #######################################

### * check out ridership by city

### plot stations by city ###
ggplot(station_stats, aes(x = station_longitude, y = station_latitude, color = city)) + geom_point()

### differences in usage by gender?
data_clean %>% group_by(member_gender) %>% summarise(count = n())
data_clean %>% group_by(start_station_city, member_gender) %>% summarise(count = n())

# gender breakdown by city
g <- ggplot(data_clean, aes(x = start_station_city, fill = member_gender))
g + geom_bar()
g + geom_bar(position = position_fill())


### usage by age: density distribution
g <- ggplot(data_clean, aes(x = year(today())-member_birth_year, color = start_station_city)) +
  xlab("rider age")

g + geom_density()
# some spurious ages
g + geom_density() + 
  xlim(c(0,80))

# usage by age: cumulative distribution
g + stat_ecdf() +
  xlim(c(0,80))

#### * Where and when are the rides? map them! ###################################################

### plot station-to-station rides
ggplot(sample_n(data_clean, size = 20000), aes(x = start_station_id, y = end_station_id, color = start_station_city)) +
  geom_point(alpha = 0.5)
# station IDs are not entirely grouped by city, and East Bay seems to have a sub-cluster

# do people ever cross cities?
data_clean %>%
  filter(start_station_city != end_station_city) %>%
  select(start_station_city, end_station_city, start_station_name, end_station_name, duration_sec, 
         user_type, member_gender, member_birth_year, is_weekend) %>%
  rename(duration_min = duration_sec) %>%
  mutate(duration_min = duration_min/60) %>%
  # group_by(member_gender) %>%
  group_by(start_station_city, start_station_name, end_station_city, end_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# yes but very rarely

