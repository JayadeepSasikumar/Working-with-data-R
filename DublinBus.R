library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)

kNineAm <- strptime(as.character('09:00:00'), format = "%H:%M:%S")
kNoon <- strptime(as.character('12:00:00'), format = "%H:%M:%S")
kThreePm <- strptime(as.character('15:00:00'), format = "%H:%M:%S")
kSixPm <- strptime(as.character('18:00:00'), format = "%H:%M:%S")
kNinePm <- strptime(as.character('21:00:00'), format = "%H:%M:%S")
kBucket1 <- '6 AM to 9 AM'
kBucket2 <- '9 AM to Noon'
kBucket3 <- 'Noon to 3 PM'
kBucket4 <- '3 PM to 6 PM'
kBucket5 <- '6 PM to 9 PM'
kBucket6 <- '9 PM +'

# db.url <- paste0("https://data.dublinked.ie/dataset/a97edfe6-1ee2-494c-9998-",
#                  "c7ab29214d59/resource/e95bd0b4-1ac3-471d-93ea-2d129c8e8dfe",
#                  "/download/googletransitdublinbusp20130315-1546.zip")
# download.file(db.url, "gtfs_dataset.zip")
unzip("gtfs_dataset.zip", exdir = "gtfs")

agencies <- read_csv('gtfs/agency.txt')
calendar <- read_csv('gtfs/calendar.txt')
calendar.dates <- read_csv('gtfs/calendar_dates.txt')
routes <- read_csv('gtfs/routes.txt')
shapes <- read_csv('gtfs/shapes.txt')
stop.times <- read_csv('gtfs/stop_times.txt')
stops <- read_csv('gtfs/stops.txt')
trips <- read_csv('gtfs/trips.txt')


# ---- Trimming down ----
# After examining the tables, we can see that some of the tables have variables
# that will not be helpful in the analysis. So, we can remove those variables.

# 1. Dropping agency_id and route_type from routes as they do not vary
# throughout the data.
routes <- select(routes, -c(agency_id, route_type))

# 2. Dropping trip_headsign and block_id from trips as they are NA throughout.
# Also, service_id can be converted to a factor as only 3 possible values are
# there.
trips <- trips %>%
  select(-c(trip_headsign, block_id)) %>%
  mutate(service_id = factor(service_id))

# 3. Dropping pickup_type and drop_off_type from stop.times as they are 0
# throughout, departure_time because it is same as arrival_time for all the
# rows
stop.times <- select(stop.times, -c(pickup_type, drop_off_type,
                                    departure_time))

# 4. Dropping start_date and end_date from calendar as they are the same
# across the table - 20130127 and 20131031 respectively. Also, service_id can
# be converted to a factor as only 3 possible values are there.
calendar <- calendar %>%
  select(-c(start_date, end_date)) %>%
  mutate(service_id=factor(service_id))

# EDA
# 1. Service-wise summary.
# a) How many routes are covered under each service?
# b) How many trips are offered by each service?
PlotTripsByServices <- function (trips) {
  # Plots the trips per services
  #
  # Args:
  #   trips: the trips data frame.
  trips.by.services <- trips %>%
    group_by(service_id) %>%
    summarise(trip_count=sum(!is.na(trip_id)))
  qplot(x=reorder(service_id, -trip_count),
        y=trip_count, data = trips.by.services,
        fill=service_id, geom = 'blank', main = 'Trips by services') +
    geom_bar(stat='identity', position = 'dodge') +
    xlab("Services") + ylab('Trips') + labs(fill = 'Service')
}

PlotRoutesByServices <- function (trips) {
  # Plots the routes per services
  #
  # Args:
  #   trips: the trips data frame.
  routes.by.services <- trips %>%
    group_by(service_id, route_id) %>%
    summarise(trip_count=sum(!is.na(trip_id))) %>%
    summarise(route_count=sum(!is.na(route_id)))
  qplot(x=reorder(service_id, -route_count),
        y=route_count, data=routes.by.services,
        fill=service_id, geom = 'blank', main = 'Routes by services') +
    geom_bar(stat = 'identity', position = 'dodge') +
    xlab("Services") + ylab('Routes') + labs(fill = 'Service')
}

# 2. Stop-wise summary.
# a) How many stops are there? Grouped by stop_ids and stop_names.
# Grouping by stop_names, we can get bus stops clustered in an area.
# b) How vital is an area(How many different routes are there in each area?)
# c) How busy is an area(How many different trips are there in each area?)

# a) How many stops are there?
stop.count <- length(unique(stops$stop_id))
stop.area.count <- length(unique(stops$stop_name))

# Importance and busyness of an area will vary based on the day of the week as
# well. So, while trying to evaluate these, service_id must be factored in.
# Also, how many stops are there in an area will also give a clearer picture.

# b) How vital is an area? (How many different routes are there in each area?)
stops.and.trips <- stop.times %>%
  inner_join(stops, by = "stop_id") %>%
  select(-c(stop_lat, stop_lon)) %>%
  inner_join(trips, by = "trip_id") %>%
  filter(service_id == 1)

# Getting area-wise stop counts.
areas.and.stops <- stops.and.trips %>%
  group_by(stop_name, stop_id) %>%
  summarise(stop_count=sum(!is.na(stop_id))) %>%
  summarise(stop_count=sum(!is.na(stop_id)))

# We are looking at the importance of an area by looking at how many different
# routes are passing through the area. We divide this by number of stops in the
# area to get area_importance. The higher the
# value for this, the more well-connected or important the area is considered.
vital.stops <- stops.and.trips %>%
  group_by(stop_name, service_id, route_id) %>%
  summarise(route_count=sum(!is.na(route_id))) %>%
  summarise(route_count=sum(!is.na(route_id))) %>%
  inner_join(areas.and.stops, by = "stop_name") %>%
  mutate(area_importance=round(route_count / stop_count, 2)) %>%
  select(stop_name, area_importance)

# c) How busy is an area? (How many different trips are there in each area?)
# Similar analysis to one done for evaluating the importance of areas.
busy.stops <- stops.and.trips %>%
  group_by(stop_name, service_id) %>%
  summarise(trip_count=sum(!is.na(route_id))) %>%
  inner_join(areas.and.stops, by = "stop_name") %>%
  mutate(area_busyness=round(trip_count / stop_count, 2)) %>%
  select(stop_name, area_busyness)

PlotStopImportance <- function (vital.stops, busy.stops) {
  # Plots area importance score against area busyness score.
  #
  # Args:
  #   vital.stops: data frame containing area importance scores.
  #   busy.stops: data frame containing area busyness scores.
  mean.importance <- round(mean(vital.stops$area_importance), 2)
  mean.busyness <- round(mean(busy.stops$area_busyness), 2)
  sub.title <- paste("Mean area importance score is",
                     as.character(mean.importance),
                     ", mean area busyness score is",
                     as.character(mean.busyness))
  stop.info <- inner_join(vital.stops, busy.stops, by='stop_name')
  qplot(x = area_importance, y = area_busyness, data = stop.info,
        main = 'Stop connectivity vs busyness') +
    xlab("Area Importance Score") + ylab('Area Busyness Score') +
    labs(subtitle = sub.title)
}

# 3. An analysis on routes -
# a) Which are the most important routes? (Most trips per routes)
# b) Routes by distances covered.

# a) Which are the most important routes? (Most trips per routes)
PlotTripsByRoutes <- function (trips, routes) {
  # Plots the number of trips against the number of routes.
  #
  # Args:
  #   trips: the trips data frame.
  #   routes: the routes data frame.
  trips.by.routes <- trips %>%
    inner_join(routes, by="route_id") %>%
    group_by(route_id, route_short_name, route_long_name) %>%
    filter(service_id == 1) %>%
    summarise(trip_count=sum(!is.na(trip_id))) %>%
    ungroup() %>%
    arrange(-trip_count) %>%
    select(route_short_name, trip_count)
  mean.trip.count <- round(mean(trips.by.routes$trip_count), 2)
  sub.title <- paste("Mean trips per route is", mean.trip.count)
  trips.by.routes <- head(trips.by.routes, 10)
  qplot(x=reorder(route_short_name, -trip_count),
        y=trip_count, data = trips.by.routes,
        geom = 'blank', main = 'Trips by services') +
    geom_bar(stat='identity', position = 'dodge') +
    xlab("Routes") + ylab('Trips') + labs(subtitle = sub.title)
}

# b) Routes by distances covered
# Direction of the trip and service should be factored in before trying to
# find the mean distance covered.

# Distance covered in each shape is summarised.
shapes.and.distances <- shapes %>%
  group_by(shape_id) %>%
  summarise(total_distance=max(shape_dist_traveled))

routes.and.distances <- trips %>%
  inner_join(routes, by = 'route_id') %>%
  inner_join(shapes.and.distances, by = 'shape_id') %>%
  group_by(route_short_name, direction_id) %>%
  filter(service_id == 1) %>%
  summarise(distance_covered = mean(total_distance)) %>%
  summarise(distance_covered = round(mean(distance_covered), 2))


# ---------------------- Route 40D -------------------- #
# Pick a bus route (forward and reverse directions) and drill down into the
# details of it extracting statistics to answer a series of questions e.g.
# suppose that you were moving to a house along the route and relying on this bus
# to get to and from work on time everyday, what questions would need to be
# answered to make you an expert on the route?. Create a plot for the most
# interesting piece of this analysis.
#
# Questions
# 1. What's the average distance, trip time and speed?
# What are the peak hours?
# How much increase in travel time is there for the peak hours
# What is the average waiting time for a bus to some area (break down by area maybe?)

# Melting calendar to make it more useful to the further analysis
calendar <- melt(calendar, id.vars = 'service_id')
colnames(calendar) <- c('service_id', 'day', 'availability')
calendar <- calendar %>%
  filter(availability == 1) %>%
  select(-availability)

# Collecting all the information about 40D route that is spread across the
# tables.
route.40d <- routes %>%
  filter(route_short_name == '40D') %>%
  inner_join(trips, by = 'route_id') %>%
  inner_join(stop.times, by = 'trip_id') %>%
  inner_join(stops, by = 'stop_id') %>%
  inner_join(shapes.and.distances, by = 'shape_id') %>%
  arrange(route_id, trip_id, stop_sequence)

route.40d.days <- route.40d %>%
  inner_join(calendar, by = 'service_id')

mean.distances.to.fro <- route.40d %>%
  group_by(stop_headsign) %>%
  summarise(mean_distance = round(mean(total_distance), 2))

route.40d.trips <- route.40d %>%
  group_by(service_id, direction_id, stop_headsign, trip_id, total_distance) %>%
  summarise(start_time = min(arrival_time), end_time = max(arrival_time),
            travel_time_in_mins = (end_time-start_time) / 60) %>%
  filter(!is.na(travel_time_in_mins))
mean.times.to.fro <- route.40d.trips %>%
  summarise(mean_travel_time_in_mins=round(mean(travel_time_in_mins)))
route.40d.mean.summary <- inner_join(mean.times.to.fro, mean.distances.to.fro,
                                     by="stop_headsign")
route.40d.mean.summary <- data.frame(route.40d.mean.summary)

PlotDayWiseTrips <- function (route.40d.days) {
  # Plots the number of trips running in the 40D route per day.
  #
  # Args:
  #   trips: the trips data frame.
  #   routes: the routes data frame.  
  day.wise.40d <- route.40d.days %>%
    group_by(day, trip_id) %>%
    summarise(trip_count = sum(!is.na(trip_id))) %>%
    summarise(trip_count = sum(!is.na(trip_id)))
  qplot(x=day, y=trip_count, data = day.wise.40d,
        geom = 'blank', main = '40D - Trips per day') +
    geom_bar(stat='identity', position = 'dodge') +
    xlab("Days") + ylab('Trips')
}

ClassifyStartTime <- function (start.time) {
  # Classifies the time into 6 buckets of 3 hours each and returns the bucket.
  #
  # Args:
  #   start.time: time represented in string in hh:mm:ss format.
  #
  # Returns:
  #   The bucket in which start.time falls.
  start.time <- strptime(as.character(start.time), format = "%H:%M:%S")
  bucket <- kBucket1
  if (start.time < kNineAm)
    bucket <- kBucket1
  else if (start.time < kNoon)
    bucket <- kBucket2
  else if (start.time < kThreePm)
    bucket <- kBucket3
  else if (start.time < kSixPm)
    bucket <- kBucket4
  else if (start.time < kNinePm)
    bucket <- kBucket5
  else
    bucket <- kBucket6
  return(bucket)
}

PlotBucketWiseSpeeds <- function (route.40d.trips) {
  # Plots the average speed of buses along the 40D route as observed in
  # different times of the day and across services.
  #
  # Args:
  #   route.40d.trips: data frame containing trip information for 40D.
  #
  # Returns:
  #   The bucket in which start.time falls.  
  speed.40d <- route.40d.trips %>%
    mutate(kmph = (total_distance * 3) /
             (as.integer(travel_time_in_mins) * 50)) %>%
    mutate(kmph = round(kmph, 2)) %>%
    mutate(time_bucket = ClassifyStartTime(start_time))
  speed.40d$time_bucket <- factor(speed.40d$time_bucket,
                                  levels = c(kBucket1, kBucket2, kBucket3,
                                             kBucket4, kBucket5, kBucket6))
  
  speed.by.time.bucket <- speed.40d %>%
    group_by(service_id, time_bucket) %>%
    summarise(average_kmph = mean(kmph)) %>%
    mutate(service = ifelse(service_id == 1, 'Service 1 - Weekdays',
                            ifelse(service_id == 2,
                                   'Service 2 - Sunday & Monday',
                                   'Service 3 - Saturday')))
  qplot(x=time_bucket, y=average_kmph, data = speed.by.time.bucket,
        group=time_bucket,
        geom = 'blank',
        main = '40D - Mean speed at different times of the day.') +
    geom_line(aes(group = 1)) +
    geom_bar(stat='identity', position = 'dodge', alpha = 0.4) +
    xlab("Time") + ylab('Speed (in kmph)') + facet_grid(. ~ service) +
    theme(axis.text.x = element_text(angle=90)) +
    theme(axis.title.y = element_text(angle=0, vjust = 0.5))
}
