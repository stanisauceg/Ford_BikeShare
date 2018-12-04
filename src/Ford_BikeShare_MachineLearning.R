library(caret)
library(ranger)


# convert all factors into numeric, including dummy coding when ordering does not make sense

data1 <- data_clean %>%
  select(-start_station_name, -end_station_name) %>%
  mutate(start_station_id = as.factor(start_station_id),
         end_station_id = as.factor(end_station_id),
         start_station_is_transit = 1 * start_station_is_transit, # 1 = yes, 0 = no
         end_station_is_transit = 1 * end_station_is_transit, # 1 = yes, 0 = no
         casual_user = 2-as.numeric(user_type), # 1 = customer, 0 = subscriber [UT: customer = 1, subscriber= 2]
         subsidized_subscriber = 1 - as.numeric(bike_share_for_all_trip), # 1 = yes, 0 = no, NA = NA [BSFAT: no = 1, yes = 1]
         weekday = recode_factor(weekday, "Mon" = 1, "Tue" = 2, "Wed" = 3,
                                 "Thu" = 4, "Fri" = 5, "Sat" = 6, "Sun" = 7),
         weekday = as.integer(weekday), # weekday makes sense to turn into an integer representation, w/ 1 = Mon, 2 = Tues, etc.
         is_weekend = as.numeric(is_weekend)-1, # 1 = yes, 0 = no
         rider_m = ifelse(member_gender == "Male", 1, 0),
         rider_f = ifelse(member_gender == "Female", 1, 0),
         rider_NA = ifelse(member_gender == "", 1, 0),
         start_SF_city = ifelse(start_station_city == "SanFrancisco", 1, 0),
         start_SJ_city = ifelse(start_station_city == "SanJose", 1, 0),
         end_SF_city = ifelse(end_station_city == "SanFrancisco", 1, 0),
         end_SJ_city = ifelse(end_station_city == "SanJose", 1, 0)) %>%
  select(-user_type, -bike_share_for_all_trip, -start_station_city, -end_station_city, -member_gender) %>%
  rename("duration" = "duration_sec")


# function to rescale a series between 0 and 1
rescale_01 <- function(x) {(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

data_scaled <- data1 %>%
  mutate(log_duration = log(duration)) %>%
  select(start_time, start_station_id, end_time, end_station_id, bike_id,
         start_station_latitude:start_station_elevation, end_station_latitude:end_station_elevation,
         member_birth_year:log_duration) %>%
  mutate_at(vars(start_station_latitude:end_SJ_city), rescale_01)
  
head(data_scaled) %>% as.data.frame()  

### * correlation plot ####
M1 <- data_scaled %>%
  filter(complete.cases(.)) %>%
  select(start_station_latitude:end_SJ_city) %>%
  cor()

corrplot(M1, method = "circle")

##### Where is this ride going? A machine learning approach ######################

summary(data_scaled)

# first, keep only the 2018 data, where bike_share_for_all info is available
data_scaled_1 <- data_scaled %>%
  filter(complete.cases(subsidized_subscriber), # data is available for all rides starting in 2018
         rider_NA != 1) %>% # dump rides where gender is unknown (339 or so instances)
  # fill in median age by city
  group_by(start_SF_city, start_SJ_city) %>%
  mutate(member_birth_year = ifelse(is.na(member_birth_year), median(member_birth_year, na.rm = TRUE), member_birth_year)) %>%
  select(-rider_NA, -bike_id)

summary(data_scaled_1)

# determine which rows are for train, which are for test  
set.seed(24)
sample_rows <- sample.int(n = nrow(data_scaled_1), size = floor(.8*nrow(data_scaled_1)), replace = F)

# make the split
train <- data_scaled_1[sample_rows, ] %>% ungroup()
test  <- data_scaled_1[-sample_rows, ] %>% ungroup()

# error: factors '131', '184', '185', '199 have no outcomes
train %>% group_by(end_station_id) %>%
  summarise(count = n()) %>%
  arrange(end_station_id) %>% print(n= "all")

test %>% group_by(end_station_id) %>%
  summarise(count = n()) %>%
  arrange(end_station_id) %>% print(n= "all")

data %>% group_by(end_station_id) %>%
  summarise(count = n()) %>%
  arrange(end_station_id) %>% print(n= "all")
# indeed, these stations have no arrivals

data %>%
  filter(end_station_id %in% c("131", "184", "185", "199")) %>%
  summarise(first = min(start_time),
            last = max(start_time))
# apparently these stations were discontinued after 10/26/2017

# exclude their factor levels from the original data_scaled prior to splitting
data_scaled_1$end_station_id <- factor(data_scaled_1$end_station_id)
set.seed(24)
sample_rows <- sample.int(n = nrow(data_scaled_1), size = floor(.8*nrow(data_scaled_1)), replace = F)

train <- data_scaled_1[sample_rows, ] %>% ungroup()
test  <- data_scaled_1[-sample_rows, ] %>% ungroup()

### fit model

## baseline predictions: single most popular end station for each start station
top_destinations <- train %>%
  group_by(start_station_id, end_station_id) %>%
  summarize(n = n()) %>%
  mutate(rank = rank(-n)) %>%
  filter(rank == min(rank)) %>%
  select(ends_with("_id")) %>%
  group_by(start_station_id) %>%
  summarise(end_station_id = first(end_station_id))

top2_destinations <- train %>%
  group_by(start_station_id, end_station_id) %>%
  summarize(n = n()) %>%
  mutate(rank = rank(-n)) %>%
  filter(rank < 3)

numerify.factor <- function(f) {
  if(!is.factor(f)) stop("input must be a factor")
  f <- as.numeric(levels(f))[f]
  return(f)
}

predictions <- test
predictions$predicted <- top_destinations$end_station_id[match(predictions$start_station_id, top_destinations$start_station_id)]
predictions <- predictions %>%
  mutate(correct = predicted == end_station_id)
predictions %>% group_by(correct) %>% summarize(n = n(), rate = n/nrow(predictions)) %>% as.data.frame()

# fit ranger random forest, still only use start station to predict end station
rg3 <- ranger(end_station_id ~ start_station_id, data = train)
rg3
# same 87% OOB prediction error, or 13% accuracy

# incorporate time & rider info
rg2 <- ranger(end_station_id ~ start_station_id + member_birth_year + is_weekend + 
                hour + casual_user + subsidized_subscriber + rider_m + rider_f, data = train)
rg2
# this is WORSE!! 87.8% OOB prediction error, or 12.2% accuracy


# using all station info EXCEPT start_station_id for the start stations
rg1 <- ranger(end_station_id ~ start_station_latitude + start_station_longitude +
                start_station_is_transit + start_station_elevation + member_birth_year +
                is_weekend + hour + casual_user + subsidized_subscriber + rider_m + rider_f +
                start_SF_city + start_SJ_city, data = train)
rg1
# BETTER: 74.4% OOB prediction error, 25.6% accuracy

pred_data <- predict(rg1, data = test)

comparison1 <- cbind(predicted = pred_data$predictions, actual = test$end_station_id) %>% as.data.frame() %>% as.tbl()

comparison1 <- comparison1 %>%
  mutate(correct = predicted == actual)
sum(comparison1$correct)/nrow(comparison1)
